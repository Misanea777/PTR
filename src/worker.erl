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



handle_cast({sent_anal, Msg}, State) ->
    timer:sleep(rand:uniform(41) + 9),
    {Id, Tweet} = Msg,
    Formated_message = string:chomp(Tweet),
    Parssed_message = generate_json(Formated_message),

    % get_hashtags(Parssed_message),


    
    % io:format("Sentiment- ~p:: ~p~n", [Id, sent_anal:analyze(Parssed_message)]),
    {noreply, State}; 


handle_cast({eng_anal, Msg}, State) ->
    timer:sleep(rand:uniform(41) + 9),
    {Id, Tweet} = Msg,
    Formated_message = string:chomp(Tweet),
    Parssed_message = generate_json(Formated_message),

    io:format("Engagement- ~p:: ~p~n", [Id, eng_anal:analyze(Parssed_message)]),
    {noreply, State}.

generate_json("{\"message\": panic}") ->
    exit(self(), kill);

generate_json(S) ->
    mochijson2:decode(S).

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

