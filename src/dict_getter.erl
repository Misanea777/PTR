-module(dict_getter).
-define(ROUTE, "http://localhost:8000/emotion_values").
-export([start_link/0, init/1]).

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]). 

init(Parent) ->
    case get_dict() of
        ok ->
            proc_lib:init_ack(Parent, {ok, self()});
        {error, Reason} ->
            exit(Reason)
    end.
    

get_dict() ->
    inets:start(),
    {ok,{{_Version,200,_Ok}, _Headers, Body}} = httpc:request(?ROUTE),
    gen_key_and_val(Body),
    % io:format("RESPONSE::::::~n~p", [Result]),
    ok.
    

split_words(S) ->
    string:tokens(S, [$\n, $\r]).

gen_key_and_val(S) ->
    gen_key_and_val(split_words(S), [], ets:whereis(sent_dict)).

gen_key_and_val([], Acc, _Table) ->
    Acc;

gen_key_and_val([H|T], Acc, Table) ->
    [Key|[Val|_Empty]] = string:tokens(H, [$\t]),
    ets:insert(Table, {Key, Val}),
    gen_key_and_val(T, [{Key, Val}|Acc], Table).







