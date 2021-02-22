-module(count_min_sketch).
-export([init_sketch/0, update_sketch/2, print/1]).

-define(NR_OF_HASH_F, 4). 
-define(MAX_HASH_VAL, 256).


gen_hashes(N) ->
    gen_hashes(N, []).

gen_hashes(0, Prefixes) ->
    Prefixes;

gen_hashes(N, Prefixes) ->
    gen_hashes(N-1, [{N, binary:bin_to_list(base64:encode(crypto:strong_rand_bytes(N)))} | Prefixes]).


% this will greatly increase the collisions, but well I don't have a workstation just a shity laptop
get_hash_val(Prefix, Data) ->
    binary:first(crypto:hash(md5, Data ++ Prefix)).


init_sketch() ->
    {gen_hashes(?NR_OF_HASH_F), array_2d:new({?MAX_HASH_VAL, ?NR_OF_HASH_F})}.

update_sketch({Hashes, Sketch_T}, Data) ->
    {New_sketch_T, Ocur} = update_sketch({Hashes, Sketch_T}, Data, []),
    {Ocur, {Hashes, New_sketch_T}}.

update_sketch({[] = _Hashes, Sketch_T}, _Data, Ocur) ->
    {Sketch_T, Ocur};

update_sketch({[H|T] = _Hashes, Sketch_T}, Data, Ocur) ->
    {New_sketch_T, New_ocur} = update_sketch_with_a_hash(H, Data, Sketch_T, Ocur),
    update_sketch({T, New_sketch_T}, Data, Ocur ++ New_ocur).
    

update_sketch_with_a_hash({Nr, Hash}, Data, Sketch_T, Ocur) ->
    Row = get_hash_val(Hash, Data),
    Current_val = array_2d:get(Row, Nr, Sketch_T),
    io:format("~p", [Current_val]),
    {array_2d:set(Row, Nr, Current_val+1, Sketch_T), [Current_val+1 | Ocur]}.




print({_Hashes, Sketch_T}) ->
    array_2d:print(?MAX_HASH_VAL, ?NR_OF_HASH_F, Sketch_T).







