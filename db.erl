%%%-------------------------------------------------------------------
%%% File    : db.erl
%%% Author  : Markus Flambard <markus@flambard.se>
%%% Description : A database for peers and pieces.
%%%
%%% Created : 30 Apr 2008 by Markus Flambard <markus@flambard.se>
%%%-------------------------------------------------------------------
-module(db).

-behaviour(gen_server).

-include("torrent.hrl").
-include("info.hrl").
-include("peer.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% db client functions
-export([
	 add_peer/2,
	 remove_peer/1,
	 peer_have/2,
	 peer_bitfield/2,
	 peer_byte_speed/3,
	 downloading/1,
	 download_finished/1,
	 download_interrupted/1,
	 any_piece/0,
	 rare_pieces/1,
	 piece_owners/1,
	 number_of_pieces/0,
	 number_of_pieces_left/0,
	 piece_size/0,
	 info_hash/0,
	 bitfield/0,
	 sha/1,
	 known_peers/0,
	 known_peers/1,
	 bytes/0,
	 bytes_uploaded/0,
	 bytes_downloaded/0,
	 bytes_left_to_download/0
	]).


-record(state,
	{
	  db,
	  torrent,
	  bitfield
	 }).

-record(tables,
	{
	  peer_info,
	  peer_status,
	  piece_owners,
	  pieces
	 }
       ).


%%%
%%% TABLE DEFINITIONS
%%%

-record(peer_info,
	{
	  pid,
	  id,
	  ip,
	  port
	}).

-record(peer_status,
	{
	  pid,
	  piece_count            = 0,
	  am_choking             = true,
	  am_interested          = false,
	  peer_choking           = true,
	  peer_interested        = false,
	  current_download_speed = 0,
	  max_download_speed     = 0,
	  total_bytes_downloaded = 0,
	  current_upload_speed   = 0,
	  max_upload_speed       = 0,
	  total_bytes_uploaded   = 0
	}).

-record(pieces,
	{
	  index,
	  count  = 0,
	  status = want %% want | have
	}).

-record(piece_owners,
       {
	 index,
	 pids
       }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Torrent, Pieces) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Torrent, Pieces], []).


add_peer(Pid, ID) ->
    gen_server:cast(?MODULE, {add_peer, Pid, ID}).

remove_peer(PeerPid) ->
    gen_server:cast(?MODULE, {remove_peer, PeerPid}).

peer_have(PeerPid, Piece) ->
    gen_server:cast(?MODULE, {peer_have, PeerPid, Piece}).

peer_bitfield(Peer, Bitfield) ->
    gen_server:cast(?MODULE, {peer_bitfield, Peer, Bitfield}).

peer_byte_speed(Peer, Up, Down) ->
    gen_server:cast(?MODULE, {peer_byte_speed, Peer, Up, Down}).

downloading(Piece) ->
    gen_server:cast(?MODULE, {downloading, Piece}).

download_finished(Piece) ->
    gen_server:cast(?MODULE, {download_finished, Piece}).

download_interrupted(Piece) ->
    gen_server:cast(?MODULE, {download_interrupted, Piece}).


any_piece() ->
    gen_server:call(?MODULE, any_piece).

rare_pieces(N) ->
    gen_server:call(?MODULE, {rare_pieces, N}).

piece_owners(Piece) ->
    gen_server:call(?MODULE, {piece_owners, Piece}).

number_of_pieces() ->
    gen_server:call(?MODULE, number_of_pieces).

number_of_pieces_left() ->
    gen_server:call(?MODULE, number_of_pieces_left).

piece_size() ->
    gen_server:call(?MODULE, piece_size).

info_hash() ->
    gen_server:call(?MODULE, info_hash).

bitfield() ->
    gen_server:call(?MODULE, bitfield).

sha(Piece) ->
    gen_server:call(?MODULE, {sha, Piece}).

known_peers() ->
    gen_server:call(?MODULE, {known_peers, 0}).

known_peers(Compact) ->
    gen_server:call(?MODULE, {known_peers, Compact}).

bytes() ->
    gen_server:call(?MODULE, bytes).

bytes_uploaded() ->
    gen_server:call(?MODULE, bytes_uploaded).

bytes_downloaded() ->
    gen_server:call(?MODULE, bytes_downloaded).

bytes_left_to_download() ->
    gen_server:call(?MODULE, bytes_left_to_download).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Torrent = #torrent{info = Info}, Pieces]) ->
    DB = #tables{
      peer_info    = ets:new(pit, [{keypos, #peer_info.pid}]),
      peer_status  = ets:new(pst, [{keypos, #peer_status.pid}]),
      pieces       = ets:new(pct, [{keypos, #pieces.index}]),
      piece_owners = ets:new(pot, [{keypos, #piece_owners.index}, bag])
     },
    NumberOfPieces = info:number_of_pieces(Info),
    Bitfield = bitfield:new(NumberOfPieces),
    Indices = lists:seq(0, NumberOfPieces - 1),
    ets:insert(DB#tables.pieces, [#pieces{index = Index} || Index <- Indices]),
    State = #state{db = DB, torrent = Torrent, bitfield = Bitfield},
    {ok, lists:foldl(fun have_piece/2, State, Pieces)}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(any_piece, _From, S = #state{db = DB}) ->
    case ets:match(DB#tables.pieces, #pieces{index = '$1', count = '_'}, 1) of
	'$end_of_table' -> {reply, no_wanted_pieces_left, S};
	{[Piece], _}    ->
	    {reply, {Piece, ets:lookup(DB#tables.piece_owners, Piece)}, S}
    end;

handle_call({rare_pieces, N}, _From, S = #state{db = DB}) ->
    Pieces = ets:match(DB#tables.pieces, #pieces{index = '$1', count = '$2'}),
    Sorted = lists:keysort(#pieces.count, Pieces),
    Trimmed = lists:dropwhile(fun ({_, C}) -> C =:= 0 end, Sorted),
    {Rare, _} = lists:split(N, Trimmed),
    Reply = [{P, ets:lookup(DB#tables.piece_owners, P)} || P <- Rare],
    {reply, Reply, S};

handle_call({piece_owners, Piece}, _From, S = #state{db = DB}) ->
    {reply, ets:lookup(DB#tables.piece_owners, Piece), S};

handle_call(number_of_pieces, _From, S = #state{torrent = T}) ->
    {reply, info:number_of_pieces(T#torrent.info), S};

handle_call(number_of_pieces_left, _From, S = #state{db = DB}) ->
    C = ets:select_count(DB#tables.pieces, [{#pieces{index = '_', count = '_'},
					     [],
					     [true]}]),
    {reply, C, S};

handle_call(piece_size, _From, S = #state{torrent = T}) ->
    {reply, info:piece_length(T#torrent.info), S};

handle_call(info_hash, _From, S = #state{torrent = T}) ->
    {reply, T#torrent.info_hash, S};

handle_call(bitfield, _From, S = #state{bitfield = Bitfield}) ->
    {reply, Bitfield, S};

handle_call({sha, Piece}, _From, S = #state{torrent = T}) ->
    {reply, info:sha(T#torrent.info, Piece), S};

handle_call({known_peers, Compact}, _From, S = #state{db = DB}) ->
    Peers = ets:match(DB#tables.peer_info, '$1'),
    Reply =
	case Compact of
	    1 -> [#peer{ip = IP, port = Port}
		  || [#peer_info{ip = IP, port = Port}] <- Peers];
	    0 -> [#peer{id = ID, ip = IP, port = Port}
		  || [#peer_info{id = ID, ip = IP, port = Port}] <- Peers]
	end,
    {reply, Reply, S};

handle_call(bytes, From, S) ->
    {reply, Uploaded, _} = handle_call(bytes_uploaded, From, S),
    {reply, Downloaded, _} = handle_call(bytes_downloaded, From, S),
    {reply, Left, _} = handle_call(bytes_left_to_download, From, S),
    {reply, {Uploaded, Downloaded, Left}, S};

handle_call(bytes_uploaded, _From, S = #state{db = _DB, torrent = _T}) ->
    BytesUploaded = 0,
    {reply, BytesUploaded, S};

handle_call(bytes_downloaded, _From, S = #state{db = _DB, torrent = _T}) ->
    BytesDownloaded = 0,
    {reply, BytesDownloaded, S};

handle_call(bytes_left_to_download, _From, S = #state{db = DB, torrent = T}) ->
    C = ets:select_count(DB#tables.pieces, [{#pieces{index = '_', count = '_'},
					     [],
					     [true]}]),
    {reply, C * info:piece_length(T#torrent.info), S};

handle_call(_Request, _From, S = #state{}) ->
    Reply = ok,
    {reply, Reply, S}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_peer, Pid, ID}, S = #state{db = DB}) ->
    ets:insert(DB#tables.peer_info, #peer_info{pid = Pid, id = ID}),
    ets:insert(DB#tables.peer_status, #peer_status{pid = Pid}),
    {noreply, S};

handle_cast({remove_peer, Pid}, S = #state{db = DB}) ->
    ets:delete(DB#tables.peer_info, Pid),
    ets:delete(DB#tables.peer_status, Pid),
    lists:foreach(fun ([P]) ->
			  ets:update_counter(DB#tables.pieces, P, -1),
			  ets:delete_object(DB#tables.piece_owners, {P, Pid})
		  end,
		  ets:match(DB#tables.piece_owners, {'$1', Pid})),
    {noreply, S};

handle_cast({peer_have, Peer, Piece}, S = #state{db = DB}) ->
    ets:insert(DB#tables.piece_owners,
	       #piece_owners{index = Piece, pids = Peer}),
    ets:update_counter(DB#tables.pieces, Piece, 1),
    {noreply, S};

handle_cast({peer_bitfield, Peer, Bitfield}, S = #state{db = DB}) ->
    lists:foreach(fun (P) ->
			  ets:insert(DB#tables.piece_owners,
				     #piece_owners{index = P, pids = Peer}),
			  ets:update_counter(DB#tables.pieces, P, 1)
		  end,
		  bitfield:to_list(Bitfield)),
    {noreply, S};

handle_cast({peer_byte_speed, Peer, CurrentUp, CurrentDown}, S = #state{db = DB}) ->
    [PS] = ets:lookup(DB#tables.peer_status, Peer),
    #peer_status{max_download_speed = MaxDown,
		 total_bytes_downloaded = TotalDown,
		 max_upload_speed = MaxUp,
		 total_bytes_uploaded = TotalUp} = PS,
    ets:insert(DB#tables.peer_status,
	       PS#peer_status{
		 current_download_speed = CurrentDown,
		 max_download_speed = erlang:max(MaxDown, CurrentDown),
		 total_bytes_downloaded = TotalDown + CurrentDown,
		 current_upload_speed = CurrentUp,
		 max_upload_speed = erlang:max(MaxUp, CurrentUp),
		 total_bytes_uploaded = TotalUp + CurrentUp}),
    {noreply, S};

handle_cast({downloading, Piece}, S = #state{db = DB}) ->
    ets:update_element(DB#tables.pieces, Piece, {#pieces.status, downloading}),
    {noreply, S};

handle_cast({download_finished, Piece}, S = #state{}) ->
    {noreply, have_piece(Piece, S)};

handle_cast({download_interrupted, Piece}, S = #state{db = DB}) ->
    ets:update_element(DB#tables.pieces, Piece, {#pieces.status, want}),
    {noreply, S};

handle_cast(_Msg, State) ->
    io:format("db got an unexpected message:~n~w", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

have_piece(Piece, S = #state{db = DB, bitfield = Bitfield}) ->
    NewBitfield = bitfield:set_bit(Bitfield, Piece),
    ets:update_element(DB#tables.pieces, Piece, {#pieces.status, have}),
    S#state{bitfield = NewBitfield}.
