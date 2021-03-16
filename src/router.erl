-module(router).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, router}, ?MODULE, [], []).

init(_Args) ->
    {ok, 1}.



handle_cast(Tweet, State) ->
    New_state = round_robin(State, Tweet),
    {noreply, New_state}.


%Logic

round_robin(Index, Msg) ->
    NextIndex = round_robin(Index, Msg, length(global:registered_names()), sent_anal),
    round_robin(NextIndex, Msg, length(global:registered_names()), eng_anal).

round_robin(Index, Msg, Nr_of_Workers, Command) when Nr_of_Workers > 0 ->
    Is_true = Index > length(global:registered_names()),
    if Is_true ->
            Next_index = 1;
        true ->
           Next_index = Index
    end,
    gen_server:cast(lists:nth(Next_index, global:registered_names()), {Command, Msg}),
    Next_index + 1;

round_robin(_Index, Msg, _Nr_of_Workers, _Command) ->
    gen_server:cast(router, {msg, Msg}),
    1.



% Unused fun
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