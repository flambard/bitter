-module(url).

-export([safe/1, encode/1, decode/1]).


safe(C) when $a =< C, C =< $z;
	     $A =< C, C =< $Z;
	     $0 =< C, C =< $9 ->
    C;

safe(C) ->
    %% "$-_.+!*'(),"
    case lists:member(C, ".-_~") of
	true  -> C;
	false -> "%" ++ integer_to_hexlist(C)
    end.


encode(<<Binary/binary>>) ->
    encode(binary_to_list(Binary));

encode(String) ->
    one_level_flatten(lists:map(fun safe/1, String)).


decode("%" ++ [N, M | Rest]) ->
    [httpd_util:hexlist_to_integer([N, M]) | decode(Rest)];

decode([C | Rest]) ->
    [C | decode(Rest)];

decode([]) ->
    [].


integer_to_hexlist(C) ->
    case httpd_util:integer_to_hexlist(C) of
	[X] -> [$0, X];
	NN  -> NN
    end.


one_level_flatten(List) ->
    lists:foldr(fun ([Obj | Rest], Acc) -> [Obj | Rest ++ Acc];
		    ([], Acc)           -> Acc;
		    (Obj, Acc)          -> [Obj | Acc]
		end,
		[],
		List).
