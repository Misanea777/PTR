-module(tcp_client).
-include("msg.hrl").
-export([start/1]).
-compile({parse_transform, fancyflow_trans}).


-define(HEADER_SIZE, 4).
-define(TCP_OPTIONS, [binary, {packet, ?HEADER_SIZE}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    {ok, Socket} = gen_tcp:connect("localhost", Port, ?TCP_OPTIONS),
    register(Socket),
    subscribe(Socket),
    loop(Socket).




loop(Socket) ->
    {ok, Data} = gen_tcp:recv(Socket, 0),
    Msg = msg_types:deserialize(Data),
    io:format("REC::~n~p~n", [msg_types:get_type(Msg)]),
    loop(Socket).


subscribe(Socket) ->
    Msg = [pipe](
        msg_types:build_msg(),
        msg_types:with_header(_, 
            [pipe](
                msg_types:build_header(subscribe_msg), 
                msg_types:with_option(_, msg_types:mk_option(topics, [tweets]))
            )
        )
    ),
    Serialized = msg_types:serialize(Msg),
    gen_tcp:send(Socket, Serialized),
    ok.


unsubscribe(Socket) ->
    Msg = [pipe](
        msg_types:build_msg(),
        msg_types:with_header(_, 
            [pipe](
                msg_types:build_header(unsubscribe_msg), 
                msg_types:with_option(_, msg_types:mk_option(topics, [tweets]))
            )
        )
    ),
    Serialized = msg_types:serialize(Msg),
    gen_tcp:send(Socket, Serialized),
    ok.


register(Socket) ->
    Msg = [pipe](
        msg_types:build_msg(),
        msg_types:with_header(_, 
            [pipe](
                msg_types:build_header(register), 
                msg_types:with_option(_, msg_types:mk_option(reg_name, <<"Loh">>))
            )
        )
    ),
    Serialized = msg_types:serialize(Msg),
    gen_tcp:send(Socket, Serialized),
    ok.



