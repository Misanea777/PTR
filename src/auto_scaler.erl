-module(auto_scaler).
-define(SCALER_RATE, 1).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, auto_scaler}, ?MODULE, [], []).

init(_Args) ->
    {ok, {erlang:monotonic_time(second), 0}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(new_mess, {Prev_time, Acc} = _State) ->
    Is_true = check_if_time_passed(Prev_time),
    if Is_true ->
            % dynamic_supervisor ! {mess_quant, Acc},
            io:format("count: ~p~n", [Acc]),
            New_state = {erlang:monotonic_time(second), 0};
        true ->
            New_state = {Prev_time, Acc+1}
    end,
    {noreply, New_state}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_if_time_passed(Initial_time) ->
    (erlang:monotonic_time(second) - Initial_time) >= ?SCALER_RATE.
