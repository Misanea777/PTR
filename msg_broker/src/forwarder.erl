-module(forwarder).
-include("msg.hrl").

-export([
    forward/1,
    forward_to_many/2,
    forward_to_many/3
]).

% Cause it's forwarding the msg)

forward(Msg) ->
    Consumers = msg_types:get_consumers(Msg),
    forward_to_many(proxy, Msg, Consumers).

% Returns [{Msg, [joric, vasea, killua]}] where joirc vasea and killua didn't receivie the msg
forward_to_many(Msg, Consumers) ->
    case lists:foldl(fun(Consumer, Errors) -> Errors ++ forward(Msg, Consumer) end, [], Consumers) of
        [] -> [];
        Err -> {Msg, Err}
    end.



forward(Msg, Consumer) ->
    [{Name, Adress}|_T] = ets:lookup(clients, Consumer),
    try gen_tcp:send(Adress, msg_types:serialize(Msg)) of
        ok -> [];
        {error,closed} -> [Name]
    catch
        _:_ -> [Name]
    end.


% Checks if the message is expired or not
% If the message is persistent and has not exp date - CONGRAJULASHIONS you made an unkillable god
% and the only thing to kill it is to send it to all the consumers that are supposed to rcv it
forward_to_many(proxy, Msg, Consumers) ->
    case msg_types:is_expired(Msg) of
        false -> forward_to_many(Msg, Consumers);
        true -> {error, expired}
    end.