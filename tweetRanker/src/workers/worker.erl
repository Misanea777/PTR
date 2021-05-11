-module(worker).
-behaviour(gen_server).

%% API
-export([stop/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    global:register_name(self(), self()),
    {ok, none}.

handle_cast({_Type, {_Id, panic}}, State) ->
    exit(self(), kill),
    {noreply, State}; 


handle_cast({sent_anal, Msg}, State) ->
    timer:sleep(rand:uniform(41) + 9),
    {Id, Tweet} = Msg,
    
    % get_hashtags(Parssed_message),
    SentScore = sent_anal:analyze(Tweet),
    gen_server:cast(aggregator, {sent_score, {Id, SentScore}}),
    {noreply, State}; 


handle_cast({eng_anal, Msg}, State) ->
    timer:sleep(rand:uniform(41) + 9),
    {Id, Tweet} = Msg,
    EngScore = eng_anal:analyze(Tweet),
    gen_server:cast(aggregator, {eng_score, {Id, EngScore}}),
    {noreply, State}.


send_hashtags([]) ->
    ok;

send_hashtags(Hashtags) ->
    gen_server:cast(hashtag_ranker, {hashtag, Hashtags}).

get_hashtags(Parssed_message) ->
    Hashtags = ej:get({"message", "tweet", "entities", "hashtags"}, Parssed_message),
    send_hashtags(Hashtags).

% Logic



% Unused func

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    {message_queue_len, Len} = process_info(self(), message_queue_len),
    io:format("????????????Terminating worker ~p with ~p messages~n", [self(), Len]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

