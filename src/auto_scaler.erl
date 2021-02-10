-module(auto_scaler).
-export([init/0]).
-define(SCALER_RATE, 1).

init() ->
    inf_loop({erlang:monotonic_time(second), 0}),
    ok.

inf_loop({Prev_time, Acc}) ->
    receive
        stop ->
            {stoped, self()};
        new_mess ->
            case check_if_time_passed(Prev_time) of
                false -> inf_loop({Prev_time, Acc+1});
                true ->
                    dynamic_supervisor ! {mess_quant, Acc},
                    inf_loop({erlang:monotonic_time(second), 0})
            end
    end.

check_if_time_passed(Initial_time) ->
    (erlang:monotonic_time(second) - Initial_time) >= ?SCALER_RATE.