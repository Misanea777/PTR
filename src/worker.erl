-module(worker).
-export([init/1]).

init(Creator_Pid) ->
    inf_loop(),
    ok.

inf_loop() ->
    receive
        stop ->
            ok;
        Msg ->
            inf_loop()
    end.
