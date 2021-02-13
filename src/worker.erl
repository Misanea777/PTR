-module(worker).
-export([init/1]).

init(Creator_Pid) ->
    inf_loop(0),
    ok.

inf_loop(Acc) ->
    %  timer:sleep(random:uniform(451)+49),
    timer:sleep(50),
    receive
        stop ->
            ok;
        {msg, Msg} ->
            inf_loop(Acc+1)
    end.
