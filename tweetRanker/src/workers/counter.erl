-module(counter).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).

start_link() ->
    gen_server:start_link({local, counter}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{dummy=1}}.



handle_cast(Msg, State) ->
    process(Msg),
    % gen_server:cast(router, {msg, Msg}),
    % gen_server:cast(auto_scaler, new_mess),
    {noreply, State}.



% Logic


process(Tweet) ->
    Formated_message = string:chomp(Tweet),
    Parssed_message = generate_json(Formated_message),
    Retweet = extract_retweet(Parssed_message),
    send_tweet_and_retweet(Parssed_message, Retweet).

send_tweet_and_retweet(Tweet, undefined) ->
    send_tweet(Tweet);

send_tweet_and_retweet(Tweet, Retweet) ->
    send_tweet(Tweet),
    send_tweet(Retweet).


send_tweet(Tweet) ->
    gen_server:cast(router, {erlang:unique_integer([positive]), Tweet}),
    gen_server:cast(auto_scaler, new_mess).



extract_retweet(panic) ->
    undefined;

extract_retweet(Tweet) ->
    ej:get({"retweeted_status"}, Tweet).

generate_json(S) ->
    try
        Json = mochijson2:decode(S),
        ej:get({"message", "tweet"}, Json)
    catch
        _:_ -> panic
    end.


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
