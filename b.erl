-module(b).

-export([encode/1, decode/1]).

%%%
%%% Encode function
%%%

encode(<<S/binary>>) ->
    [integer_to_list(size(S)),
     $:,
     binary_to_list(S)];

encode(I) when is_integer(I) ->
    [$i,
     integer_to_list(I),
     $e];

encode([{Key, Value} | Rest]) ->
    [$d,
     [[encode(K), encode(V)] || {K, V} <- [{Key, Value} | Rest]],
     $e];

encode(L) ->
    [$l,
     [encode(E) || E <- L],
     $e].

%%%
%%% Decode function
%%%

decode("i" ++ Rest) ->
    {I, "e" ++ R} = lists:splitwith(fun(C) -> C /= $e end, Rest),
    {list_to_integer(I), R};

decode([N | Rest]) when N >= $0, N =< $9 ->
    {I, ":" ++ R1} = lists:splitwith(fun(C) -> C /= $: end, [N | Rest]),
    {S, R2} = lists:split(list_to_integer(I), R1),
    {list_to_binary(S), R2};

decode("l" ++ Rest) ->
    decode_list(Rest, []);

decode("d" ++ Rest) ->
    decode_dictionary(Rest, []);

%% Ignore unknown
decode([_ | Rest]) ->
    decode(Rest);

decode(<<Bin/binary>>) ->
    decode(binary_to_list(Bin)).


%%% Decode List

decode_list("e" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};

decode_list(List, Acc) ->
    {V, R} = decode(List),
    decode_list(R, [V | Acc]).


%%% Decode Dictionary

decode_dictionary("e" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};

decode_dictionary(List, Acc) ->
    {K, R1} = decode(List),
    {V, R2} = decode(R1),
    decode_dictionary(R2, [{K, V} | Acc]).
