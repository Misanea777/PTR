-module(sent_anal).
-export([analyze/1]).

analyze(Tweet) ->
    Text = ej:get({"text"}, Tweet),
    Cov = binary:bin_to_list(Text),
    Splited = split_words(Cov),
    
    get_score(score, Splited).


split_words(S) ->
    string:lexemes(S, [$\s, "\""]).


get_score(score, List) ->
    get_score(List, 0) / length(List);

get_score([], Acc) ->
    Acc;

get_score([H|T], Acc) ->
    get_score(T, Acc + get_score(ets:lookup(sent_dict, H))).

get_score([]) ->
    0;

get_score([{_Key, Val}|_Tail]) ->
    list_to_integer(Val).