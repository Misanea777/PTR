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
    extract_value(Hashtags, State),
    {noreply, State}.

extract_value([], {Sketch, Current_top}) ->
    ok;

extract_value([H|T], {Sketch, Current_top}) ->
    Hashtag = ej:get({"text"}, H),
    % io:format("Recived: ~s~n", [Hashtag]),
    {Is_it_worth, Ocur, New_sketch} = is_it_worthy(Hashtag, Sketch, Current_top),
    New_top = update_top({Is_it_worth, Ocur, New_sketch}),
    extract_value(T, {New_sketch, New_top}).

update_top({false, Ocur, New_sketch}, Current_top, Hashtag) ->
    Current_top;

update_top({true, Ocur, New_sketch}, Current_top, Hashtag) ->
    New_top = [{Ocur, Hashtag}|Current_top],
    New_top.

is_it_worthy(Hashtag, Sketch, []) ->
    {Ocur, New_sketch} = count_min_sketch:update_sketch(Sketch, Hashtag),
    {true, Ocur, New_sketch};

is_it_worthy(Hashtag, Sketch, Current_top) ->
    {Ocur, New_sketch} = count_min_sketch:update_sketch(Sketch, Hashtag),
    {battle_of_hashtags(Ocur, Current_top), Ocur, New_sketch}.

battle_of_hashtags(_Ocur, []) ->
    false;

battle_of_hashtags(Ocur, [H|T]) ->
    {Ocur_rival, _Name} = H,
    if Ocur > Ocur_rival ->
        true;
    true ->
        battle_of_hashtags(Ocur, T)
    end.
   










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
