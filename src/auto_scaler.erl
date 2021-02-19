-module(auto_scaler).
-define(SCALER_RATE, 1).
-define(MSG_PER_WORKER, 50).
-define(INITIAL_WORKERS, 1).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, auto_scaler}, ?MODULE, [], []).

init(_Args) ->
    dynamic_supervisor:add_workers(?INITIAL_WORKERS),
    {ok, {erlang:monotonic_time(second), 0}}.



handle_cast(new_mess, {Prev_time, Acc} = _State) ->
    Is_true = check_if_time_passed(Prev_time),
    if Is_true ->
            adjust_workers(Acc),
            % io:format("----------Msg:~p W:~p :~p ~n", [Acc, length(global:registered_names()), length(supervisor:which_children(dynamic_supervisor))]),
            % print_workers_load(),
            New_state = {erlang:monotonic_time(second), 0};
        true ->
            New_state = {Prev_time, Acc+1}
    end,
    {noreply, New_state}.

% Logic
adjust_workers(Msg_nr) ->
    Diff = length(supervisor:which_children(dynamic_supervisor)) - (Msg_nr div ?MSG_PER_WORKER),
    if Diff > 0 ->
            dynamic_supervisor:remove_workers(Diff);
        true ->
            dynamic_supervisor:add_workers(-Diff)
    end.

% For debugging
print_workers_load() ->
    print_workers_load(global:registered_names()).

print_workers_load([]) ->
    ok;

print_workers_load(L) ->
    [H|T] = L,
    {message_queue_len, Len} = process_info(H, message_queue_len),
    io:format("Worker: ~p : ~p~n", [H, Len]),
    print_workers_load(T).


% Unused func

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_if_time_passed(Initial_time) ->
    (erlang:monotonic_time(second) - Initial_time) >= ?SCALER_RATE.
