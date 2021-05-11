-module(msg_types).

-include("msg.hrl").
-compile({parse_transform, fancyflow_trans}).

% Header fields------
-define(type, "type").
-define(options, "opt").

% -------------------


-record(msg, {
    header :: any(),
    body :: any()
}).

-export([
    serialize/1,
    deserialize/1
]).

-export([
    get_type/1,
    get_option/2,
    get_body/1,
    get_id/1,
    is_expired/1,
    get_consumers/1
]).


-export([
    build_msg/0,
    with_header/2,
    with_body/2,
    build_header/1,
    with_option/2,
    mk_option/2
]).




serialize(Msg) ->
    mochijson2:encode([Msg#msg.header, Msg#msg.body]).

deserialize(RawMsg) ->
    ParsedMsg = mochijson2:decode(RawMsg),
    #msg{header = get_header_from_parsed(ParsedMsg), body = get_body_from_parsed(ParsedMsg)}.

get_header_from_parsed(ParsedMsg) ->
    ej:get({first}, ParsedMsg).

get_body_from_parsed(ParsedMsg) ->
    case ej:get({last}, ParsedMsg) of
        {struct,[]} -> undefined;
        Body -> Body
    end.


get_type(Msg) ->
    binary_to_atom(ej:get({?type}, Msg#msg.header)).

get_option(Option, Msg) ->
    case get_options(Msg) of
        undefined -> undefined;
        Options -> find_option(Option, Options)
    end.


get_options(Msg) ->
    ej:get({?options}, Msg#msg.header).

get_options_from_header(Header) ->
    ej:get({?options}, Header).


find_option(Name, _Options = [H|T]) ->
    case ej:get({Name}, H) of
        undefined -> find_option(Name, T); 
        Option -> Option
    end;

find_option(_Name, _Options = []) ->
    undefined.


get_body(Msg) ->
    Msg#msg.body.



% Creating msg from DZERO wiz allmighty builder pattern
build_msg() ->
    #msg{header = undefined, body = undefined}.

with_header(Msg, Header) ->
    Msg#msg{header = Header}.

with_body(Msg, Body) ->
    Msg#msg{body = Body}.

build_header(Type) ->
    ej:set({?type}, {struct,[]}, Type).

with_option(Header, Option) ->
    case get_options_from_header(Header) of
        undefined -> ej:set({?options}, Header, [Option]);
        _Options -> ej:set({?options, new}, Header, Option)
    end.

mk_option(Key, Value) ->
    ej:set({atom_to_list(Key)}, {struct,[]}, Value).


% ______________ Im lazy to refactor and add the id, so lets use the one from db

get_id(Msg) ->
    ej:get({"_id"}, get_body(Msg)).


is_expired(Msg) ->
    case msg_types:get_option(?exp, Msg)  of
        undefined -> false;
        Date -> 
            [Year,Month,Day,Hour,Min,Sec] = Date,
            calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds({{Year,Month,Day},{Hour,Min,Sec}}) > 0
    end.

get_consumers(Msg) ->
    Consumers = lists:map(fun({_Topic, Client}) -> Client end, topicker:get_subscribtions_from_topics(get_option(?topics, Msg))).


% Older version

% -export([mk_msg/1, mk_json/1, mk_json_header/1]).
% -export([
%     deserialize/1,
%     get_header/1,
%     get_body/1,
%     get_type/1,
%     get_topics/1,
%     get_opt/1
% ]).

% % Abandon all hope, ye who enter here

% % Msg -> Json -> Bin -> Json -> Msg


% % From Bin Json -> Msg

% mk_msg(RawMsg) ->
%     ParsedMsg = deserialize(RawMsg),
%     Header = mk_header(ParsedMsg),
%     mk_msg(Header#header.type, Header, ParsedMsg).

% mk_msg(data, Header, ParsedMsg) ->
%     #msg{header = Header, body = get_body(ParsedMsg)};


% mk_msg(_Control_Type, Header, _ParsedMsg) ->
%     #msg{header = Header}.


% mk_header(ParsedMsg) ->
%     #header{type = get_type(ParsedMsg), topics = get_topics(ParsedMsg), opt = get_opt(ParsedMsg)}.



% % From Msg -> Json

% mk_json(Msg) ->
%     [
%         mk_json_header(Msg#msg.header),
%         Msg#msg.body
%     ].


% % my little ugly child......
% mk_json_header(Header) ->
%     JsonHeader = {struct, []},
%     WithTopics = ej:set({"topics"}, JsonHeader, [to_merged_bin_list(Header#header.topics)]),
%     WithTopicsAndType = ej:set({"type"}, WithTopics, to_binary(Header#header.type)),
%     WithTopicsAndTypeAndOpt = ej:set({"opt"}, WithTopicsAndType, Header#header.opt),
%     WithTopicsAndTypeAndOpt.







% deserialize(RawMsg) ->
%     io:format("zis iz::~n~p~n", [mochijson2:decode(RawMsg)]),
%     mochijson2:decode(RawMsg).

% serialize(ParsedMsg) ->
%     mochijson2:encode(ParsedMsg).

% get_header(ParsedMsg) ->
%     ej:get({first}, ParsedMsg).

% get_body(ParsedMsg) ->
%     ej:get({last}, ParsedMsg).

% get_type(header, Header) ->
%     binary_to_atom(ej:get({"type"}, Header)).

% get_topics(header, Header) ->
%     [H|_T] = ej:get({"topics"}, Header),
%     to_atom_list(H).

% get_opt(header, Header) ->
%     ej:get({"opt"}, Header).


% get_type(ParsedMsg) ->
%     get_type(header, get_header(ParsedMsg)).

% get_topics(ParsedMsg) ->
%     get_topics(header, get_header(ParsedMsg)).

% get_opt(ParsedMsg) ->
%     get_opt(header, get_header(ParsedMsg)).



% % Helper fun

% to_atom_list(Binary) ->
%     [list_to_atom(Item) || Item <- to_list(Binary)].

% to_list(Binary) ->
%     string:tokens(binary:bin_to_list(Binary), ", ").



% to_binary_list(AtomList) ->
%     [atom_to_binary(Item) || Item <- AtomList].

% to_binary(Atom) ->
%     atom_to_binary(Atom).

% to_string_list(AtomList) ->
%     [atom_to_list(Item) || Item <- AtomList].


% to_merged_bin_list(AtomList) ->
%     unicode:characters_to_binary(lists:join(", ", to_string_list(AtomList)), utf8).

    

