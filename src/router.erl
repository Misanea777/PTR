-module(router).
-export([init/1, round_robin/2]).

init(Workers) ->
    inf_loop(Workers, 1),
    ok.

inf_loop(Workers, Index) ->
    receive
        {workers, U_workers} ->
            inf_loop(U_workers, 1);
        stop ->
            {stoped, self()};
        {msg, Msg} ->
            {message_queue_len, Len} = process_info(lists:last(Workers), message_queue_len),
            io:write(Len),
            io:nl(),  
            inf_loop(Workers, round_robin({Workers, Index}, Msg))
    end.


round_robin({Workers, Index}, Msg) when Index =< length(Workers) ->
    lists:nth(Index, Workers) ! {msg, Msg},
    Index+1;

round_robin({Workers, Index}, Msg) when Index > length(Workers) ->
    lists:nth(1, Workers) ! {msg, Msg},
    2.

