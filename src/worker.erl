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



handle_cast({msg, Msg}, State) ->
    timer:sleep(rand:uniform(41) + 9),
    Formated_message = string:prefix(string:chomp(Msg), "event: \"message\"\n\ndata: "),
    Parssed_message = mochijson2:decode(Formated_message),
    Text = ej:get({"message", "tweet", "text"}, Parssed_message),
    % io:format("~nText: ~s~n", [Text]),
    {noreply, State}. 



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

