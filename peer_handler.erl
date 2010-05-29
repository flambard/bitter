-module(peer_handler).

-include("message.hrl").
-include("peer.hrl").
-include("global.hrl").

-export([start/2]).

%%%
%%% Explain to self:
%%% What exactly, does peer_handler do?
%%%
%%% Answer:
%%% peer_handler is responsible for downloading/uploading whole pieces from/to
%%% the peer by downloading/uploading the individual blocks.
%%%
%%% That means the manager should not care about blocks, only full pieces.
%%%


-record(peer_status, % There will be more slots here for keeping track of blocks
	{
	  me_choked = true,
	  peer_choked = true,
	  bytes_downloaded = 0, %% Since last report to DB
	  bytes_uploaded = 0    %% --
	}).

start(Peer = #peer{}, Handshake) ->
    spawn(fun () ->
		  io:format("Trying to connect to peer ~p...\n", [Peer]),
		  case peer:connect(Peer) of
		      {error, Reason} ->
			  io:format("Failed to connect to ~p\n", [Peer]),
			  {error, Reason};
		      {ok, Socket} ->
			  io:format("We connected to peer: ~p\n", [Peer]),
			  start(Socket, Handshake)
		  end
	  end),
    ok;

start(Socket, Handshake) ->
    Pid = spawn(fun () -> receive ok -> init(Socket, Handshake) end end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! ok,
    ok.

init(Socket, Handshake) ->
    {ok, Port} = protocol:start(Socket, Handshake),
    receive
	{Port, #handshake{reserved=_Reserved, id=ID}} -> ok
    end,
    link(whereis(manager)),
    db:add_peer(self(), ID),
    Bitfield = db:bitfield(),
    Port ! {self(), #bitfield{bitfield = Bitfield}},
    erlang:send_after(10000, self(), 'REPORT_BYTES'),
    listen(Port, #peer_status{}).

%%% peer_handler should have a store for not yet uploaded blocks and downloaded
%%% blocks for not yet complete pieces.
listen(Port, PeerStatus) ->
    receive
	'REPORT_BYTES' ->
	    #peer_status{bytes_uploaded = U, bytes_downloaded = D} = PeerStatus,
	    db:peer_byte_speed(self(), U, D),
	    erlang:send_after(10000, self(), 'REPORT_BYTES'),
	    NewStatus = PeerStatus#peer_status{bytes_uploaded = 0,
					       bytes_downloaded = 0},
	    listen(Port, NewStatus);

	{manager, Command} ->
	    NewStatus = manager_message(Command, Port, PeerStatus),
	    listen(Port, NewStatus);

	{Port, Message} ->
	    NewStatus = peer_message(Message, PeerStatus),
	    listen(Port, NewStatus);
	
	_Other ->
	    io:format("peer_handler ~w got unexpected message ~w~n", [self(), _Other]),
	    listen(Port, PeerStatus)
    end.


%%%
%%% Messages from manager
%%%

manager_message(#choke{}, Port, Status) ->
    %% Update status -- peer is now choked!
    Port ! {self(), #choke{}},
    Status#peer_status{peer_choked = true};

manager_message(#unchoke{}, Port, Status) ->
    %% Update status -- peer is now unchoked!
    Port ! {self(), #unchoke{}},
    Status#peer_status{peer_choked = false};

manager_message(#interested{}, Port, Status) ->
    Port ! {self(), #interested{}},
    Status;

manager_message(#not_interested{}, Port, Status) ->
    Port ! {self(), #not_interested{}},
    Status;

manager_message(H = #have{}, Port, Status) ->
    Port ! {self(), H},
    Status;

%%% Hmm!
manager_message({'DOWNLOAD', Index}, Port, Status) ->
    PieceSize = db:piece_size(),
    Requests = block:divide_piece(Index, PieceSize, ?BLOCK_SIZE),
    Send = fun(R) -> Port ! {self(), R} end,
    lists:foreach(Send, Requests),
    Status.


%%%
%%% Messages from peer
%%%

peer_message(#choke{}, Status) ->
    %% Clear queue
    %% Need to tell manager? or just update the DB?
    Status#peer_status{me_choked = true};

peer_message(#unchoke{}, Status) ->
    %% Update status -- I am now unchoked!
    %% Need to tell manager? or just update the DB?
    Status#peer_status{me_choked = false};

peer_message(#interested{}, Status) ->
    %% Need to tell manager? or just update the DB?
    Status;

peer_message(#not_interested{}, Status) ->
    %% Need to tell manager? or just update the DB?
    Status;

peer_message(#have{index = Piece}, Status) ->
    db:peer_have(self(), Piece),
    Status;

peer_message(#bitfield{bitfield = Bitfield}, Status) ->
    db:peer_bitfield(self(), Bitfield),
    Status;

peer_message(#request{index = Piece, offset = _ByteOffset, length = _Length}, Status) ->
    %% Fetch piece from disk and divide it into blocks.
    %% Place blocks in some kind of send queue
    %% If we don't already have read the needed piece, do so.
    _Bytes = disk:read(Piece),
    %% Measure upload speed!
    Status;

peer_message(#cancel{}, Status) ->
    %% Remove block from queue
    %% (for now, do nothing)
    Status;

peer_message(#piece{}, Status) ->
    %% Put block in some kind of memory and wait until a whole piece is collected.
    %% Measure download speed!
    Status.
