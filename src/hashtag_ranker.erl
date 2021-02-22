-module(hashtag_ranker).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, hashtag_ranker}, ?MODULE, [], []).

init(_Args) ->
    Initial_sketch = count_min_sketch:init(),
    {ok, {Initial_sketch, []}}.


handle_cast({hashtag, Hashtags}, State) ->
    New_state = extract_value(Hashtags, State),
    {noreply, New_state}.

extract_value([], {Sketch, Current_top}) ->
    {Sketch, Current_top};

extract_value([H|T], {Sketch, Current_top}) ->
    Hashtag = ej:get({"text"}, H),
    % io:format("Recived: ~s~n", [Hashtag]),
    {New_sketch, New_top} = update_top(Hashtag, Sketch, Current_top),
    extract_value(T, {New_sketch, New_top}).

update_top(Hashtag, Sketch, Current_top) ->
    {Ocur, New_sketch} = count_min_sketch:update_sketch(Sketch, Hashtag), 
    New_top = lists:reverse(lists:sort(Current_top ++ {Ocur, Hashtag})),
    {New_sketch, lists:sublist(New_top, 10)}.



   










% Fun that nobody wants

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
