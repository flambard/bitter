%%%-------------------------------------------------------------------
%%% File    : disk.erl
%%% Author  : Markus Flambard <markus@flambard.se>
%%% Description : A disk reader/writer server that possibly could perform caching.
%%%
%%% Created : 12 May 2008 by Markus Flambard
%%% Updated : 27 February 2010 by Markus Flambard
%%%-------------------------------------------------------------------
-module(disk).

-behaviour(gen_server).

-include("info.hrl").

%% API
-export([start_link/1, read/1, write/2, affected_files/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {info, files}).

%%%
%%% Change proposal:
%%% #state{} should be a list of open files together with the index of the
%%% first piece, the last piece, and the length of the last piece in that file.
%%%
%%% Thusly, the type of 'files' is:
%%% [{FirstPieceIndex, LastPieceIndex, LastPieceLength, FileDescriptor}]
%%%
%%% Note: This change has not been implemented yet!
%%%
%%% The current type of 'files' is:
%%% [{BytePosition, ByteLength, FileDescriptor}]
%%%
%%% Question to self: WHY should this be changed?
%%%   The current solutions seems fine -- crude perhaps, but simple.
%%%

%%%
%%% Needed:
%%% Functions for checking that the data on disk is correct using the SHA
%%% checksums in Info.
%%%
%%%
%%%

%%%
%%% At startup:
%%% Do full data integrity check (SHA checksum on every piece)
%%%

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Info) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Info], []).

read(Index) ->
    gen_server:call(?MODULE, {read, Index}).

write(Index, Piece) ->
    gen_server:call(?MODULE, {write, Index, Piece}).

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
init([Info]) ->
    Files = make_file_list(Info),
    {ok, #state{info = Info, files = Files}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({read, Index}, _From, S = #state{info = I, files = Files}) ->
    try lists:foldl(fun ({Position, Length, FD}, Acc) ->
			    case file:pread(FD, Position, Length) of
				{ok, Bytes} ->
				    <<Acc/binary, Bytes/binary>>;
				{error, Reason} ->
				    throw(Reason)
			    end
		    end,
		    <<>>,
		    affected_files(Index, info:piece_length(I), Files))
    of
	Reply -> {reply, Reply, S}
    catch
	throw:Reason -> {reply, {error, {pread, Reason}}, S}
    end;

handle_call({write, Index, Piece}, _From, S = #state{info = I, files = Files}) ->
    SHA = info:sha(I, Index),
    case crypto:sha(Piece) of
	SHA ->
	    try lists:foldl(fun ({Position, Length, FD}, Data) ->
				    <<Bytes:Length/binary, Rest/binary>> = Data,
				    case file:pwrite(FD, Position, Bytes) of
					ok ->
					    Rest;
					{error, Reason} ->
					    throw(Reason)
				    end
			    end,
			    Piece,
			    affected_files(Index, info:piece_length(I), Files))
	    of
		<<>> -> {reply, ok, S}
	    catch
		throw:Reason -> {reply, {error, {pwrite, Reason}}, S}
	    end;
	_Mismatch ->
	    {reply, {error, sha_mismatch}, S}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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

%%
%% Files:
%% [{BytePosition, ByteLength, FileDescriptor}]
%%

affected_files(Index, PieceLength, Files) ->
    Start = Index * PieceLength,
    End = Start + PieceLength - 1,
    Trimmed = lists:dropwhile(fun ({P, L, _}) -> P + L < Start end, Files),
    FilesOI = lists:takewhile(fun ({P, _, _}) -> P < End end, Trimmed),
    lists:map(fun ({P, L, FD}) ->
		      First = P,
		      Last = P + L,
		      case {First =< Start, End =< Last} of
			  {true, true}   ->
			      Position = Start - First,
			      Length = End - Start + 1;
			  {true, false}  ->
			      Position = Start - First,
			      Length = Last - Start + 1;
			  {false, true}  ->
			      Position = 0,
			      Length = End - First + 1;
			  {false, false} ->
			      Position = 0,
			      Length = Last - First + 1
		      end,
		      {Position, Length, FD}
	      end,
	      FilesOI).


-define(FILE_OPTIONS, [read, write, raw, binary]).

make_file_list(#singlefile{length = Length, name = Filename}) ->
    {ok, FD} = file:open(Filename, ?FILE_OPTIONS),
    [{0, Length - 1, FD}];

make_file_list(#multifile{files = Files, name = DirName}) ->
    ok = file:make_dir(DirName),
    ok = file:set_cwd(DirName),
    try	lists:foldl(fun (#file{length = Length, path = Path}, {Position, Acc}) ->
			    ok = ensure_dirs_exist(Path),
			    Filename = string:join(Path, "/"),
			    case file:open(Filename, ?FILE_OPTIONS) of
				{ok, FD} ->
				    Next = Position + Length,
				    {Next, [{Position, Next - 1, FD} | Acc]};
				{error, Reason} ->
				    throw({{file, open, 2}, Reason})
			    end
		    end,
		    {0, []},
		    Files)
    of
	{_, FileList} -> lists:reverse(FileList)
    catch
	throw:Reason -> {error, Reason}
    after
	ok = file:set_cwd("..")
    end.


ensure_dirs_exist([_File]) -> ok;
ensure_dirs_exist([Directory | Path]) ->
    case file:make_dir(Directory) of
	ok ->
	    ok = file:set_cwd(Directory),
	    ensure_dirs_exist(Path),
	    ok = file:set_cwd("..");
	{error, eexist} ->
	    ok = file:set_cwd(Directory),
	    ensure_dirs_exist(Path),
	    ok = file:set_cwd("..");
	{error, Reason} ->
	    throw({{file, make_dir, 1}, Reason})
    end.
