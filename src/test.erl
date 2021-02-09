-module(test).
-export([init/0, request/1, count_mess/2, count_mess/1, worker/1, increase_workers/2, decrease_workers/2, create_workers/2]).


count_mess(Alias) ->
    count_mess(0, whereis(Alias)).


count_mess(Prev_mess_len, Pid) ->
    timer:sleep(1000),
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    Recived = Len - Prev_mess_len,
    io:write(Recived),
    io:nl(),
    count_mess(Len, Pid).

init() ->
    inets:start(),
    init_req().



init_req() ->
    register(p1,spawn(?MODULE, request, ["http://localhost:8000/tweets/1"])),
    register(p2,spawn(?MODULE, request, ["http://localhost:8000/tweets/2"])),
    ok.

request(Request) ->
    httpc:request(get, {Request, []}, [], [{sync, false}, {stream, self}]),
    inf_loop(),
    ok.

inf_loop() ->
    receive
        stop ->
            ok
        % Msg ->
        %     Worker = spawn(?MODULE, worker, [self()]),
        %     Worker ! {msg, Msg},
        %     inf_loop()
    end.



worker(Creator_Pid) ->
    receive
        {msg, Msg} ->
            ok;
        {stop, Pid} when Creator_Pid =:= Pid ->
            ok
    end.

% send_to_workers(Workers, Msg) ->
    


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
    Pid = spawn(?MODULE, worker, [self()]),
    create_workers([Pid], Value-1);

create_workers(Workers, Value) ->
    Pid = spawn(?MODULE, worker, [self()]),
    create_workers([Pid|Workers], Value-1).











    