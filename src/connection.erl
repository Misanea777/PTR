-module(connection).
-export([connect/2]).


connect(Caller, URL) ->
    inets:start(),
    httpc:request(get, {URL, []}, [], [{sync, false}, {stream, self}]),
    inf_loop(Caller, URL), ok.


inf_loop(Caller, URL) ->
    receive
        stop ->
            {stoped, self()};
        {http,{_Ref,{error,socket_closed_remotely}}} ->
            io:format("Error: serrver_closed_remotely Pid: ~p~n Recconecting........~n", [self()]),
            connect(Caller,URL);
        {http,{_,{error,{failed_connect,_}}}} ->
            io:format("Error: failed_to_connect Pid: ~p~n Recconecting........~n", [self()]),
            timer:sleep(5000),
            connect(Caller,URL);
        Msg ->
            gen_server:cast(Caller, Msg),
            inf_loop(Caller,URL)
    end.


