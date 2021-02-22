-module(count_min_sketch).
-export([hexlify/1, generate_hashes/1]).


generate_hashes(N) ->
    generate_hashes(N, []).

generate_hashes(0, Prefixes) ->
    Prefixes;

generate_hashes(N, Prefixes) ->
    generate_hashes(N-1, [{N, binary:bin_to_list(base64:encode(crypto:strong_rand_bytes(N)))} | Prefixes]).





hexlify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.