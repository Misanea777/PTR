-module(router).
-export([init/0]).

init() ->
    inf_loop([]),
    ok.

inf_loop(Workers) ->
    receive
        {workers, U_workers} ->
            inf_loop(U_workers);
        stop ->
            {stoped, self()};
        msg ->
            inf_loop(Workers)
    end.