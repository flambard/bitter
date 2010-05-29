-module(bitter).

-include_lib("kernel/include/file.hrl").

-include("torrent.hrl").
-include("info.hrl").
-include("global.hrl").

-export([start/1]).


%%%
%%% This is the start file
%%%

start(TorrentFilename) ->
    ok = crypto:start(),
    ok = inets:start(),
    PeerID = generate_id(),
    io:format("bitter ~s  [~s]~n", [?VERSION_STRING, url:encode(PeerID)]),
    {ok, Torrent} = torrent:read_file(TorrentFilename),
    manager:start(Torrent, PeerID),
    running.


generate_id() ->
    Random = crypto:rand_bytes(12),
    <<"-B.", ?COMPACT_VERSION_STRING, "-", Random/binary>>.
