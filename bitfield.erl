-module(bitfield).

-export([new/1, to_list/1, set_bit/2]).


new(Bits) when Bits rem 8 =:= 0 ->
    <<0:Bits>>;
new(Bits) ->
    <<0:(Bits - (Bits rem 8) + 8)>>.

to_list(Bitfield) ->
    lists:reverse(to_list(Bitfield, 0, [])).

to_list(<<A:1, B:1, C:1, D:1, E:1, F:1, G:1, H:1, Rest/binary>>, Offset, List0) ->
    List1 = add_if_set(A, 0 + Offset, List0),
    List2 = add_if_set(B, 1 + Offset, List1),
    List3 = add_if_set(C, 2 + Offset, List2),
    List4 = add_if_set(D, 3 + Offset, List3),
    List5 = add_if_set(E, 4 + Offset, List4),
    List6 = add_if_set(F, 5 + Offset, List5),
    List7 = add_if_set(G, 6 + Offset, List6),
    List8 = add_if_set(H, 7 + Offset, List7),
    to_list(Rest, 8 + Offset, List8);
to_list(<<>>, _Bit, List) ->
    List.

add_if_set(0, _Index, List) -> List;
add_if_set(1, Index, List)  -> [Index | List].


set_bit(Bitfield, Bit) ->
    <<Before:Bit, _:1, After/binary>> = Bitfield,
    <<Before:Bit, 1:1, After>>.
