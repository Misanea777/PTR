-module(connection).
-export([connect/2]).


connect(Caller, URL) ->
    httpc:request(get, {URL, []}, [], [{sync, false}, {stream, self}]),
    inf_loop(Caller), ok.


inf_loop(Caller) ->
    receive
        stop ->
            {stoped, self()};
        Msg ->
            Caller ! Msg,
            inf_loop(Caller)
    end.