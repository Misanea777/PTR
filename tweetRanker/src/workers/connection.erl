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
    L = split_msg(binary:bin_to_list(BinBodyPart)),
    New_State = construct_msg(L, State),
    {noreply, New_State}.

% Logic

construct_msg([], Buffer) ->
    Buffer;

construct_msg([H|T], Buffer) ->
    New_Buffer = construct_msg(lists:suffix([10,10], H), Buffer ++ H),
    construct_msg(T, New_Buffer);
    
construct_msg(true, Buffer) ->
    gen_server:cast(counter, Buffer),
    [];

construct_msg(false, Buffer) ->
    Buffer.

split_msg(S) ->
    string:split(S, "event: \"message\"\n\ndata: ", all).


% Unused func

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



