-module(disk_storage).
-behaviour(gen_server).

%% API
% future impl - make a gc 
-export([stop/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    store/1,
    delete/1,
    delete_mult/1,
    load_all/0
]).
-record(state, {disk_table}).

stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, Name} = dets:open_file("disk", []),
    {ok, #state{disk_table = Name}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};



handle_call({load, Pattern}, _From, State) ->
    Result = lists:map(fun({_Id, Msg}) -> {Msg, msg_types:get_consumers(Msg)} end, dets:match_object(State#state.disk_table, Pattern)),
    {reply, Result, State}.




handle_cast({store, Msg}, State) ->
    dets:insert(State#state.disk_table, {msg_types:get_id(Msg), Msg}),
    {noreply, State};


handle_cast({delete, Id}, State) ->
    dets:delete(State#state.disk_table, Id),
    {noreply, State}.





store(Msg) ->
    gen_server:cast(?MODULE, {store, Msg}).

delete(Id) ->
    gen_server:cast(?MODULE, {delete, Id}).


delete_mult(_Ids = [H|T]) ->
    delete(H),
    delete_mult(T);

delete_mult(_Ids = []) ->
    ok.


load_all() ->
    gen_server:call(?MODULE, {load, _Pattern = '_'}).





% Usless

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

