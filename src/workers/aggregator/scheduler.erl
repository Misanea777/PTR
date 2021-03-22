-module(scheduler).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {rate}).
-define(PERIOD, 100). % how often the agg gives msg to sink
-define(INITIAL_RATE, 200). % num of msg per period
-define(BASE_RATE_MOD, 0.25). % used for aggressive rate adjusting

start_link() ->
    gen_server:start_link({local, scheduler}, ?MODULE, [], []).

init(_Args) ->
    periodic_loop(),
    {ok, #state{rate = ?INITIAL_RATE}}.



handle_cast({adjust_rate, RateModif}, State) when RateModif > -2 ->
    % io:format("here nahui~n",[]),
    AdjustedRateModif = ?BASE_RATE_MOD * RateModif,
    AdjustedRate = State#state.rate + trunc(State#state.rate * AdjustedRateModif),
    {noreply, State#state{rate = AdjustedRate}};

handle_cast({adjust_rate, _RateModif}, State)->
    {noreply, State}.


handle_info(periodic_loop, State) ->
    % io:format("Loop me harder dady~n", []),
    % io:format("Rate:: ~p~n", [State#state.rate]),
    gen_server:cast(aggregator, {get, State#state.rate}),
    periodic_loop(),
    {noreply, State}.



% Logic

periodic_loop() ->
    timer:send_after(?PERIOD, periodic_loop),
    ok.





% Trash func

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
