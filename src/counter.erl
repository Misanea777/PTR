-module(counter).
-export([init/0]).

init() ->
    inf_loop(),
    ok.

inf_loop() ->
    receive
        stop ->
            {stoped, self()};
        Msg ->
            router ! {msg, Msg},
            auto_scaler ! new_mess,
            inf_loop()
    end.