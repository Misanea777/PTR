-module(main).
-export([init/0, get_nr_of_msg/1]).
-define(ROUTE_1, "http://localhost:8000/tweets/1").
-define(ROUTE_2, "http://localhost:8000/tweets/2").

init() ->
    register(router, spawn(router, init, [])),
    register(auto_scaler, spawn(auto_scaler, init, [])),
    register(counter, spawn(counter, init, [])),
    inets:start(),
    register(c1, spawn(connection, connect, [whereis(counter), ?ROUTE_1])),
    register(c2, spawn(connection, connect, [whereis(counter), ?ROUTE_2])).


get_nr_of_msg(Alias) ->
    process_info(whereis(Alias), message_queue_len).