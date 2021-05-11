-module(executor).
-export([execute/1]).
-include("msg.hrl").


% just a module that "decides" what to do with msg
% yeah I know that the name is odd but my tiny brain could not think of a better name

execute({Msg, Socket}) ->
    execute(msg_types:get_type(Msg), {Msg, Socket}).


% create topics if not present
execute(conn_pub, {Msg, _Socket}) ->
    topicker:create_topics(msg_types:get_option(?topics, Msg));

% yeah ok, have a great day!
execute(disconn_pub, {_Msg, _Socket}) ->
    ok;

execute(register, {Msg, Socket}) ->
    Name = msg_types:get_option(?reg_name, Msg),
    ets:insert(clients, {Name, Socket}),
    ok;

execute(subscribe_msg, {Msg, Socket}) ->
    [{ClientName, _S}|_T] = ets:match_object(clients, {'$0', Socket}),
    topicker:subscribe_to_topics(msg_types:get_option(?topics, Msg), ClientName),
    ok;


execute(unsubscribe_msg, {Msg, Socket}) ->
    [{ClientName, _S}|_T] = ets:match_object(clients, {'$0', Socket}),
    topicker:unsuscribe_from_topics(msg_types:get_option(?topics, Msg), ClientName),
    ok; 



execute(data, {Msg, Socket}) ->
    case msg_types:get_option(?presistent, Msg) of
        true -> execute_persistent(Msg);
        _ -> forwarder:forward(Msg)
    end.


execute_persistent(Msg) ->
    disk_storage:store(Msg), % first thing first - store it on disk
    case forwarder:forward(Msg) of
        {error, expired} -> 
            disk_storage:delete(msg_types:get_id(Msg)),
            ok;
        [] -> 
            disk_storage:delete(msg_types:get_id(Msg)),
            ok;
        {Msg, Err} -> queue_holder:in({Msg, Err})
    end.





