-module(router).
-export([init/0]).

init() ->
    inf_loop(),
    ok.

inf_loop() ->
    receive
        stop ->
            {stoped, self()};
        msg ->
            inf_loop()
    end.