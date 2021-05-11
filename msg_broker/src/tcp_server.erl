-module(tcp_server).

-include("msg.hrl").

-compile({parse_transform, fancyflow_trans}).

-export([start/1, serve/1]).

-define(HEADER_SIZE, 4).
-define(TCP_OPTIONS, [binary, {packet, ?HEADER_SIZE}, {active, false}, {reuseaddr, true}]).



start(LPort) ->
    case gen_tcp:listen(LPort, ?TCP_OPTIONS) of
        {ok, ListenSock} ->
            accept(ListenSock),
            {ok, LPort} = inet:port(ListenSock),
            LPort;
        {error,Reason} ->
            {error,Reason}
    end.


accept(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            Pid = spawn_link(?MODULE, serve, [S]),
            gen_tcp:controlling_process(S, Pid), % give the socket to new process
            accept(LS); % continnue accepting

        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.


% inf loop
serve(S) ->
    case gen_tcp:recv(S, 0) of
        {ok, Data} -> 
            Msg = msg_types:deserialize(Data),
            
            executor:execute({Msg, S}),

            % MyMsg = [pipe](
            %     msg_types:build_msg(),
            %     msg_types:with_header(_, 
            %         [pipe](
            %             msg_types:build_header(jora), 
            %             msg_types:with_option(_, msg_types:mk_option(loh, true))
            %         )
            %     )
            % ),

            
            % io:format("suca:::::::::::::::::::~n ~p~n", [Msg]),
            % io:format("moio:::::::::::::::::::~n ~p~n", [MyMsg]),
            serve(S);
        {error, Reason} ->
            io:format("Socket terminated: ~s~n", [Reason])
    end.




