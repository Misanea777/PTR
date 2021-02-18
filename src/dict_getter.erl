-module(dict_getter).
-define(ROUTE, "http://localhost:8000/emotion_values").
-export([get_dict/0]).

get_dict() ->
    inets:start(),
    {ok,{{_Version,200,_Ok}, _Headers, Body}} = httpc:request(?ROUTE),
    Result = split_words(Body),
    io:format("RESPONSE::::::~n~p", [Result]),
    ok.

split_words(S) ->
    string:tokens(S, [$\n, $\r]).

gen_key_and_val([], Acc) ->
    Acc.

gen_key_and_val([H|T], Acc) ->
    [Key|[Val|_Empty]] = string:tokens(H, [$\t]),
    gen_key_and_val(T, Acc ++ {Key, Val}).




