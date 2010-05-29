-module(message).

-include("message.hrl").

-export([from_packet/1, to_packet/1]).


from_packet(Packet) ->
    case Packet of
	?HANDSHAKE(R, H, I) -> {ok, #handshake{reserved=R, info_hash=H, id=I}};
	?KEEPALIVE          -> {ok, #keepalive{}};
	?CHOKE              -> {ok, #choke{}};
	?UNCHOKE            -> {ok, #unchoke{}};
	?INTERESTED         -> {ok, #interested{}};
	?NOT_INTERESTED     -> {ok, #not_interested{}};
	?HAVE(Index)        -> {ok, #have{index=Index}};
	?BITFIELD(BF)       -> {ok, #bitfield{bitfield=BF}};
	?REQUEST(I, B, L)   -> {ok, #request{index=I, offset=B, length=L}};
	?PIECE(I, B, Blk)   -> {ok, #piece{index=I, offset=B, block=Blk}};
	?CANCEL(I, B, L)    -> {ok, #cancel{index=I, offset=B, length=L}};
	<<_Other/binary>>   -> {error, malformed_packet}
    end.

to_packet(Term) ->
    case Term of
	#handshake{reserved=R, info_hash=H, id=I} -> ?HANDSHAKE(R, H, I);
	#keepalive{}                              -> ?KEEPALIVE;
	#choke{}                                  -> ?CHOKE;
	#unchoke{}                                -> ?UNCHOKE;
	#interested{}                             -> ?INTERESTED;
	#not_interested{}                         -> ?NOT_INTERESTED;
	#have{index=Index}                        -> ?HAVE(Index);
	#bitfield{bitfield=BF}                    -> ?BITFIELD(BF);
	#request{index=I, offset=B, length=L}     -> ?REQUEST(I, B, L);
	#piece{index=I, offset=B, block=Blk}      -> ?PIECE(I, B, Blk);
	#cancel{index=I, offset=B, length=L}      -> ?CANCEL(I, B, L)
    end.
