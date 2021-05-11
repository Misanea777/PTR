-module(array_2d).
-export([new/1, size/1, get/3, set/4, print/3]).

new({Rows, Cols}) ->
    Dict = dict:new(),
    new({row, Rows, Cols}, Dict).

new({row, 0, _Column}, Dict) ->
    Dict;

new({row, Row, Column}, Dict) ->
    New_dict = new({col, Row, Column}, Dict),
    new({row, Row-1, Column}, New_dict);

new({col, _Row, 0}, Dict) ->
    Dict;

new({col, Row, Column}, Dict) ->
    New_dict = dict:store({Row, Column}, 0 , Dict),
    new({col, Row, Column-1}, New_dict).

size(Dict) ->
    dict:size(Dict).

get(Row, Column, Dict) ->
    {ok, Val} = dict:find({Row, Column}, Dict),
    Val.

set(Row, Column, Value, Dict) ->
    dict:store({Row, Column}, Value , Dict).


print(Rows, Cols, Dict) ->
    print(1, 1, {Rows, Cols}, Dict).

print(Row, _Col, {Rows, _Cols}, _Dict) when Rows < Row ->
    ok;

print(Row, Col, {Rows, Cols},  Dict) when Cols >= Col ->
    io:format("~p", [get(Row, Col, Dict)]),
    print(Row, Col+1, {Rows, Cols}, Dict);

print(Row, Col, {Rows, Cols}, Dict) when Cols < Col ->
    io:format("~n", []),
    print(Row+1, 1, {Rows, Cols}, Dict).
