-module(block).

-include("message.hrl").

-export([divide_piece/3]).


divide_piece(Index, PieceSize, BlockSize) when PieceSize rem BlockSize == 0 ->
    N = PieceSize div BlockSize,
    Lengths = lists:duplicate(N, BlockSize),
    Offsets = lists:seq(0, PieceSize-1, BlockSize),
    Indexes = lists:duplicate(N, Index),
    lists:zipwith3(fun request/3, Indexes, Offsets, Lengths);

divide_piece(Index, PieceSize, BlockSize) ->
    N = PieceSize div BlockSize,
    R = PieceSize rem BlockSize,
    Lengths = lists:duplicate(N, BlockSize) ++ [R],
    Offsets = lists:seq(0, PieceSize, BlockSize),
    Indexes = lists:duplicate(N+1, Index),
    lists:zipwith3(fun request/3, Indexes, Offsets, Lengths).

%% Used?
request(Index, Offset, Length) ->
    #request{index=Index, offset=Offset, length=Length}.
