-module(torrent).

-include("torrent.hrl").

-export([trackers/1, read_file/1]).


trackers(#torrent{announce = URL, announce_list = '_'}) -> [[URL]];
trackers(#torrent{announce_list = URLs})                -> URLs.


read_file(Filename) ->
    case file:read_file(Filename) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Data} ->
	    {Structure, []} = b:decode(binary_to_list(Data)),
	    {ok, read(Structure)}
    end.


read(Dict) ->
    {value, {<<"info">>, InfoDict}} = lists:keysearch(<<"info">>, 1, Dict),
    Torrent = lists:foldl(fun t/2, #torrent{_ = '_'}, Dict),
    Torrent#torrent{info_hash = info:hash(InfoDict)}.

t({Key, Value}, T) ->
    case Key of
	<<"info">>          -> T#torrent{info = info:read(Value)};
	<<"announce">>      -> T#torrent{announce = binary_to_list(Value)};
	<<"announce-list">> -> T#torrent{announce_list = read_announce_list(Value)};
	<<"creation date">> -> T#torrent{creation_date = Value};
	<<"comment">>       -> T#torrent{comment = binary_to_list(Value)};
	<<"created by">>    -> T#torrent{created_by = binary_to_list(Value)};
	_ignored            -> io:format("Ignored in metainfo file: ~s~n", [_ignored]),
			       T
    end.


read_announce_list(Tiers) ->
    [lists:map(fun binary_to_list/1, shuffle_list(Tier)) || Tier <- Tiers].

shuffle_list(List) ->
    [X || {_, X} <- lists:keysort(1, [{rand_byte(), X} || X <- List])].

rand_byte() ->
    <<R>> = crypto:rand_bytes(1),
    R.
