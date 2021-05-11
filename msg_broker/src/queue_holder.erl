-module(queue_holder).

-behaviour(gen_server).
-define(TIMEOUT, 5000).



%% API
-export([stop/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    in/1,
    out/0,
    peek/0
]).
-record(state, {queue, timer}).

stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    TRef = start_timer(),
    Queue = queue:new(),
    FromDisk = disk_storage:load_all(), % Load from the disk if are any
    lists:foreach(fun(Item) -> in(Item) end, FromDisk),
    {ok, #state{queue = Queue, timer = TRef}}.


handle_info(timeout, State) ->
    io:format("Resending..... ~n",[]),
    NewQueue = resend(20, State#state.queue),
    TRef = start_timer(),
    {noreply, State#state{queue = NewQueue, timer = TRef}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};


handle_call(out, _From, State) ->
    Result = case queue:out(State#state.queue) of
        {{value, Item}, NewQueue} -> Item;
        {empty, NewQueue} -> empty
    end,
    {reply, Result, State#state{queue = NewQueue}};


handle_call(peek, _From, State) ->
    Result = case queue:peek(State#state.queue) of
        {value, Item} -> Item;
        empty -> empty
    end,
    {reply, Result, State}.


handle_cast({in, Msg}, State) ->
    NewQueue = queue:in(Msg, State#state.queue),
{noreply, State#state{queue = NewQueue}}.




in(Msg) ->
    gen_server:cast(?MODULE, {in, Msg}).

out() ->
    gen_server:call(?MODULE, out).

peek() ->
    gen_server:call(?MODULE, peek).



% Useless func as me


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





% --Timer - Jikan desu!

start_timer() ->
    {ok, TRef} = timer:send_after(?TIMEOUT, timeout),
    % io:format("Starting:: ~p~n",[TRef]),
    TRef.

reset_timer(TRef) ->
    {ok, _} = timer:cancel(TRef),
    % io:format("Canceling:: ~p~n",[TRef]),
    start_timer().

% Periodic resending

resend(0, Queue) ->
    Queue;

resend(Nr, Queue) ->
    case queue:out(Queue) of
        {{value, Item}, NewQueue} -> resend(Nr, NewQueue, Item);
        {empty, NewQueue} -> NewQueue
    end.

resend(Nr, Queue, {Msg, Err}) ->
    io:format("Sending...~n~p~n", [{Msg, Err}]),
    case forwarder:forward_to_many(proxy, Msg, Err) of
        {error, expired} -> 
            disk_storage:delete(msg_types:get_id(Msg)),
            ok;
        [] -> 
            disk_storage:delete(msg_types:get_id(Msg)),
            ok;
        {Msg, Err} -> queue_holder:in({Msg, Err})
    end,
    resend(Nr-1, Queue).





