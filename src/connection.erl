-module(connection).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

%% API

start_link({Name, URL}) ->
    gen_server:start_link({local, Name}, ?MODULE, [URL], []).

init(URL) ->
    inets:start(),
    httpc:request(get, {URL, []}, [], [{sync, false}, {stream, self}, {full_result, false}]),
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_Ref,stream_start, _Headers}}, State) ->
    {noreply, State};

handle_info({http, {_RequestId, stream, BinBodyPart}}, State) ->
    L = binary:bin_to_list(BinBodyPart),
    Is_true = lists:suffix([125,10,10], L),
    if Is_true ->
            % io:format("::::::::::::::::::::::::::  ~s~n", [State ++ L]),
            gen_server:cast(counter, [State ++ L]),
            New_state = [];
        true ->
            New_state = State ++ L
    end,
    {noreply, New_state}.



terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



