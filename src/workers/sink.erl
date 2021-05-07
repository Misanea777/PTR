-module(sink).

-behaviour(gen_server).

%% API
-export([insert_to_db/3, insert_usr_and_tw/2]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state_obj, {db_conn, buffer, timer}).

-define(conn_opt, [
        { name,  mongoc_pool },
        { register,  mongoc_topology },
        { pool_size, 5 },
        { max_overflow, 10 },
        { overflow_ttl, 1000 },
        { overflow_check_period, 1000 },
        { localThresholdMS, 1000 },
        { connectTimeoutMS, 20000 },
        { socketTimeoutMS, 100 },
        { serverSelectionTimeoutMS, 30000 },
        { waitQueueTimeoutMS, 1000 },
        { heartbeatFrequencyMS, 10000 },
        { minHeartbeatFrequencyMS, 1000 },
        { rp_mode, primary },
        { rp_tags, [{tag,1}] }
]).

-define(worker_opt, [
    {database, <<"Erlang">>},
    {login, <<"erl_user">>},
    {password, <<"erl_user">>},
    {ssl, true}
]).

-define(tweets_collection, <<"tweets">>).
-define(users_collection, <<"users">>).
-define(seed, {rs, <<"atlas-v26yq5-shard-0">>, [
    "cluster0-shard-00-00.ux9po.mongodb.net:27017",
    "cluster0-shard-00-01.ux9po.mongodb.net:27017",
    "cluster0-shard-00-02.ux9po.mongodb.net:27017"    
]}).


-define(BATCH, 100).
-define(OPT_INS_TIME, 550). % optimal insertion time
-define(TIMEOUT, 5500).

start_link() ->
    gen_server:start_link({local, sink}, ?MODULE, [], []).

init(_Args) ->
    DBConn = start_db_conn(),
    TRef = start_timer(),
    {ok, #state_obj{db_conn = DBConn, buffer = [], timer = TRef}}.


handle_cast({_, []}, State) ->
    {noreply, State};

handle_cast({tweets, Msg}, State) ->

    % io:format("size: ~p ~n", [length(State#state_obj.buffer)]),
    NewBuffer = State#state_obj.buffer ++ Msg,
    NewState = try_insert_to_db(State#state_obj{buffer = NewBuffer}),
    {noreply, NewState}.

handle_info(timeout, State) ->
    io:format("Timeout insertion::~p Len:: ~p~n",[State#state_obj.timer, length(State#state_obj.buffer)]),
    NewState = timeout_insert(State),
    {noreply, NewState}.


% Logic

% --Timer - Jikan desu!

start_timer() ->
    {ok, TRef} = timer:send_after(?TIMEOUT, timeout),
    % io:format("Starting:: ~p~n",[TRef]),
    TRef.

reset_timer(TRef) ->
    {ok, _} = timer:cancel(TRef),
    % io:format("Canceling:: ~p~n",[TRef]),
    start_timer().



% --DB operations

start_db_conn() ->
    application:ensure_all_started (mongodb),
    {ok, DBConn} = mongoc:connect(?seed, ?conn_opt, ?worker_opt),
    DBConn.

insert_to_db(DBConn, ElemList, Coll) ->
    mongo_api:insert(DBConn, Coll, ElemList),
    timer:sleep(trunc(rand:normal(50, 3))),
    ok.

% --Batching

timeout_insert(State) ->
    insert_usr_and_tw(State#state_obj.db_conn, State#state_obj.buffer),
    gen_server:cast(scheduler, {adjust_rate, 0.01}), 
    TRef = start_timer(),
    State#state_obj{buffer = [], timer = TRef}.

try_insert_to_db(State) when length(State#state_obj.buffer) > ?BATCH ->
    ToSend = lists:sublist(State#state_obj.buffer, ?BATCH),
    

    {Time, _} = timer:tc(?MODULE, insert_usr_and_tw, [State#state_obj.db_conn, ToSend]),
    io:format("Inserted in:: ~p ms Len:: ~p ~n", [Time div 1000, length(ToSend)]),

    gen_server:cast(scheduler, {adjust_rate, check_deviation(Time)}),
    io:format("Dev:: ~p~n", [check_deviation(Time)]),
    
    NewBuffer = lists:sublist(State#state_obj.buffer, ?BATCH + 1, length(State#state_obj.buffer)),

    TRef = reset_timer(State#state_obj.timer),

    State#state_obj{buffer = NewBuffer, timer = TRef};

try_insert_to_db(State) ->
    State.


check_deviation(InsertionTime) ->
    (?OPT_INS_TIME - (InsertionTime div 1000)) / 100.


% --Other

extract_users(AggParts) ->
    extract_users(AggParts, []).

extract_users([], Acc) ->
    % io:format("Users:::~n~p~n",[Acc]),
    Acc;

extract_users(AggParts, Acc) ->
    [H|T] = AggParts,
    Tweet = proplists:get_value(tweet, H),
    User = ej:get({"user"}, Tweet),
    extract_users(T, Acc ++ [User]).


insert_usr_and_tw(DbConn, AggParts) ->
    Users = extract_users(AggParts),
    io:format(":::::::::::::: ~p  ~p~n", [length(AggParts), length(Users)]),
    insert_to_db(DbConn, AggParts, ?tweets_collection),
    insert_to_db(DbConn, Users, ?users_collection),
    ok.






% Usless func

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
