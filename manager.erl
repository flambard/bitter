-module(manager).

-include("torrent.hrl").
-include("tracker_response.hrl").
-include("tracker_request.hrl").
-include("message.hrl").

-export([start/2]).

%%%
%%% Question to self:
%%% What is the managers responsibilities?
%%%
%%% Answer:
%%% 1. Supervise the peer_handler processes.
%%% 2. Send requests to the trackers.
%%% 3. Decide who should download, and which piece.
%%%


start(Torrent, PeerID) ->
    Pid = spawn(fun () -> init(Torrent, PeerID) end),
    true = register(manager, Pid),
    Pid.

init(T = #torrent{info = Info, info_hash = InfoHash}, PeerID) ->
    process_flag(trap_exit, true),
    {ok, _DiskPid} = disk:start_link(Info),
    io:format("disk is ~w~n", [_DiskPid]),
    KnownPieces = [],
    {ok, _DBpid} = db:start_link(T, KnownPieces),
    io:format("db is ~w~n", [_DBpid]),
    Handshake = #handshake{info_hash = InfoHash, id = PeerID},
    {ok, {Listener, Port}} = listener:start_link(Handshake),
    BaseTR = #tracker_request{
      info_hash = InfoHash,
      peer_id = PeerID,
      port = Port
     },
    FirstTR = BaseTR#tracker_request{event = started},
    Trackers = torrent:trackers(T),
    case tracker_request:send(Trackers, FirstTR) of
	{ok, {Response = #tracker_response{failure_reason = '_',
					   interval = Interval},
	      RearrangedTrackers}} ->
	    handle_tracker_response(Response, Handshake);
	{ok, {#tracker_response{failure_reason = FailReason},
	      RearrangedTrackers}} ->
	    %% Tracker responded with some error.
	    io:format("Tracker responded with error: ~s\n", [FailReason]),
	    Interval = 300 * 1000;
	{error, _Reason} ->
	    io:format("Could not connect to any tracker!~n"),
	    RearrangedTrackers = Trackers,
	    Interval = 300 * 1000
    end,
    erlang:send_after(Interval, self(), 'QUERY_TRACKER'),
    loop(Listener, Handshake, BaseTR, RearrangedTrackers).

%%%
%%% Peers should send messages in the form {Pid, Command, Args}
%%%

loop(Listener, Handshake, BaseTR, Trackers) ->
    receive
	'QUERY_TRACKER' ->
	    case tracker_request:send(Trackers, BaseTR) of
		{ok, {Response = #tracker_response{failure_reason = '_',
						   interval = Interval},
		      RearrangedTrackers}} ->
		    handle_tracker_response(Response, Handshake);
		{ok, {#tracker_response{failure_reason = FailReason},
		      RearrangedTrackers}} ->
		    %% Tracker responded with some error.
		    io:format("TRACKER ERROR: ~s\n", [FailReason]),
		    Interval = 300 * 1000;
		{error, _Reason} ->
		    io:format("Could not connect to any tracker!~n"),
		    RearrangedTrackers = Trackers,
		    Interval = 300 * 1000
	    end,
	    erlang:send_after(Interval, self(), 'SEND_TRACKER_REQUEST'),
	    loop(Listener, Handshake, BaseTR, RearrangedTrackers);

	{'EXIT', Listener, Reason} ->
	    io:format("Listener crashed for reason: ~p~nRestarting listener.~n", [Reason]),
	    {ok, {NewListener, Port}} = listener:start_link(Handshake),
	    NewBaseTR = BaseTR#tracker_request{port = Port},
	    loop(NewListener, Handshake, NewBaseTR, Trackers);

	{'EXIT', Peer, Reason} ->
	    db:remove_peer(Peer),
	    io:format("~w disconnected for reason ~p~n", [Peer, Reason]),
	    loop(Listener, Handshake, BaseTR, Trackers);

	{Pid, Command, Args} ->
	    message(Command, Args, Pid),
	    loop(Listener, Handshake, BaseTR, Trackers);

	_Msg ->
	    io:format("manager got an unexpected message: ~w~n", [_Msg]),
	    loop(Listener, Handshake, BaseTR, Trackers)	    
    end.

handle_tracker_response(#tracker_response{warning_message = Warning,
					  tracker_id = TrackerID,
					  complete = Seeders,
					  incomplete = Leechers,
					  peers = Peers},
		       Handshake) ->
    io:format("Got response from tracker ~s.\n", [TrackerID]),
    io:format("Seeders:  ~p\nLeechers: ~p\n", [Seeders, Leechers]),
    if Warning =/= '_' ->
	    io:format("TRACKER WARNING: ~s\n", [Warning]);
       true ->
	    ok
    end,
    io:format("Peers:\n~p\n\n", [Peers]),
    [peer_handler:start(Peer, Handshake) || Peer <- Peers],
    ok.

%%%
%%% Messages from peer processes (perhaps we can get rid of this?)
%%%

message(Cmd, Args, Pid) ->
    io:format("manager got an unexpected command from ~w: ~w ~w", [Pid, Cmd, Args]).
