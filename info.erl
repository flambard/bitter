-module(info).

-include("info.hrl").

-export([hash/1, read/1, number_of_pieces/1, piece_length/1, sha/2, file_name/1]).


hash(InfoDict) ->
    crypto:sha(b:encode(InfoDict)).


read(Dict) ->
    case lists:keymember(<<"files">>, 1, Dict) of
	true  -> read_multifile(Dict);
	false -> read_singlefile(Dict)
    end.


number_of_pieces(#singlefile{pieces = Pcs}) ->
    byte_size(Pcs) div 20;
number_of_pieces(#multifile{pieces = Pcs}) ->
    byte_size(Pcs) div 20.

piece_length(#singlefile{piece_length = L}) -> L;
piece_length(#multifile{piece_length = L})  -> L.

sha(#singlefile{pieces = Pcs}, Piece) -> sha(Pcs, Piece);
sha(#multifile{pieces = Pcs}, Piece)  -> sha(Pcs, Piece);
sha(Pcs, Piece) when is_binary(Pcs)   ->
    Index = Piece * 20,
    <<_:Index/binary, SHA:20/binary, _/binary>> = Pcs,
    SHA.

file_name(#singlefile{name = Name}) -> Name;
file_name(#multifile{name = Name})  -> Name.

%%%
%%% Read singlefile
%%%

read_singlefile(Dict) ->
    lists:foldl(fun sf/2, #singlefile{_ = '_'}, Dict).

sf({Key, Value}, SF) ->
    case Key of
	<<"length">>       -> SF#singlefile{length = Value};
	<<"md5sum">>       -> SF#singlefile{md5sum = Value};
	<<"name">>         -> SF#singlefile{name = binary_to_list(Value)};
	<<"piece length">> -> SF#singlefile{piece_length = Value};
	<<"pieces">>       -> SF#singlefile{pieces = Value};
	_ignored           -> SF
    end.


%%%
%%% Read multifile
%%%

read_multifile(Dict) ->
    lists:foldl(fun mf/2, #multifile{_ = '_'}, Dict).

mf({Key, Value}, MF) ->
    case Key of
	<<"files">>        -> MF#multifile{files = lists:map(fun read_file/1, Value)};
	<<"name">>         -> MF#multifile{name = binary_to_list(Value)};
	<<"piece length">> -> MF#multifile{piece_length = Value};
	<<"pieces">>       -> MF#multifile{pieces = Value};
	_ignored           -> MF
    end.


%%%
%%% Read file
%%%

read_file(Dict) ->
    lists:foldl(fun f/2, #file{_ = '_'}, Dict).

f({Key, Value}, F) ->
    case Key of
	<<"length">> -> F#file{length = Value};
	<<"md5sum">> -> F#file{md5sum = Value};
	<<"path">>   -> F#file{path = lists:map(fun binary_to_list/1, Value)};
	_ignored     -> F
    end.
