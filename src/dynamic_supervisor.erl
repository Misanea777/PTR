-module(dynamic_supervisor).
-export([init/0, create_workers/2, decide/1, increase_workers/2]).
-define(INITIAL_WORKERS, 10).
-define(MINIMAL_WORKERS, 2).

init() ->
    Workers = create_workers([], ?INITIAL_WORKERS),
    register(router, spawn(router, init, [Workers])),
    inf_loop(Workers),
    ok.

inf_loop(Workers) ->
    receive
        stop ->
            {stoped, self()};
        {mess_quant, Acc} ->
            New_workers = decide({Workers, Acc}),
            % next step - send to router
            router ! {workers, New_workers},
            inf_loop(New_workers)

    end.

decide({Workers, Acc}) ->
    Diff = length(Workers) - (Acc div 10),
    case Diff >= 0 of
        true ->
            decrease_workers(Workers, Diff);
        false ->
            increase_workers(Workers, -Diff)
    end.




increase_workers(Workers, Value) ->
    create_workers(Workers, Value).

decrease_workers(Workers, 0) ->
    Workers;

decrease_workers(Workers, Value) ->
    [H|T] = Workers,
    H ! {stop, self()},
    decrease_workers(T, Value-1).

create_workers([H|T], 0) ->
    [H|T];

create_workers([], Value) ->
    Pid = spawn(worker, init, [self()]),
    create_workers([Pid], Value-1);

create_workers(Workers, Value) ->
    Pid = spawn(worker, init, [self()]),
    create_workers([Pid|Workers], Value-1).