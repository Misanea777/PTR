-module(main).
-export([init/0, get_nr_of_msg/1]).
-define(ROUTE_1, "http://localhost:8000/tweets/1").
-define(ROUTE_2, "http://localhost:8000/tweets/2").

init() ->
    counter:start_link(),
    register(c1, spawn_link(connection, connect, [counter, ?ROUTE_1])),
    ok.


get_nr_of_msg(Alias) ->
    process_info(whereis(Alias), message_queue_len).