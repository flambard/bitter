-record(handshake,
	{
	  reserved = <<0:64>>,
	  info_hash,
	  id
	 }).

-record(keepalive, {}).

-record(choke, {}).

-record(unchoke, {}).

-record(interested, {}).

-record(not_interested, {}).

-record(have,
	{
	  index
	 }).

-record(bitfield,
	{
	  bitfield
	 }).

-record(request,
	{
	  index,
	  offset, % begin is a reserved word
	  length
	 }).

-record(piece,
	{
	  index,
	  offset, % begin is a reserved word
	  block
	 }).

-record(cancel,
	{
	  index,
	  offset, % begin is a reserved word
	  length
	 }).


-define(HANDSHAKE_LENGTH, 68).


-define( HANDSHAKE(Reserved, InfoHash, ID),
	 <<19:8, "BitTorrent protocol", Reserved:8/binary, InfoHash:20/binary, ID:20/binary>> ).

-define( KEEPALIVE,
         <<>> ).

-define( CHOKE,
	 <<0:8>> ).

-define( UNCHOKE,
	 <<1:8>> ).

-define( INTERESTED,
	 <<2:8>> ).

-define( NOT_INTERESTED,
	 <<3:8>> ).

-define( HAVE(Index),
	 <<4:8, Index:32>> ).

-define( BITFIELD(BF),
	 <<5:8, BF/binary>> ).

-define( REQUEST(Index, Begin, Length),
	 <<6:8, Index:32, Begin:32, Length:32>> ).

-define( PIECE(Index, Begin, Block),
	 <<7:8, Index:32, Begin:32, Block/binary>> ).

-define( CANCEL(Index, Begin, Length),
	 <<8:8, Index:32, Begin:32, Length:32>> ).
