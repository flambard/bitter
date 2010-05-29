-module(protocol).

-include("message.hrl").
-include("global.hrl").

-export([start/2]).

-define(KA_TIMEOUT, ?TIMEOUT - 5000).
-define(PR_TIMEOUT, ?TIMEOUT + 5000).


%%%
%%% A bittorrent protocol driver
%%%   - Hides the socket stuff so that the peer looks like
%%%     an ordinary Erlang process.
%%%


start(Socket, Handshake) ->
    Pid = spawn_link(fun () -> receive {Master, ok} -> init(Master, Socket, Handshake) end end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! {self(), ok},
    {ok, Pid}.

init(Master, Socket, Handshake) ->
    case gen_tcp:send(Socket, message:to_packet(Handshake)) of
	{error, ReasonSendError} -> exit({gen_tcp_send, ReasonSendError});
	ok -> ok
    end,
    case gen_tcp:recv(Socket, ?HANDSHAKE_LENGTH, ?PR_TIMEOUT) of
	{error, Reason}     -> exit({gen_tcp_recv, Reason});
	{ok, RecvHandshake} ->
	    case message:from_packet(RecvHandshake) of
		{error, Reason} -> exit({handshake_failed, Reason});
		{ok, Message}   ->
		    case inet:setopts(Socket, [{packet, 4}, {active, once}]) of
			{error, Reason} -> exit({inet_setopts, Reason});
			ok              -> Master ! {self(), Message}
		    end
	    end
    end,
    KATimer = erlang:send_after(?KA_TIMEOUT, self(), 'KEEPALIVE'),
    PRTimer = erlang:send_after(?PR_TIMEOUT, self(), 'TIMEOUT'),
    loop(Master, Socket, KATimer, PRTimer).

loop(Master, Socket, KATimer, PRTimer) ->
    receive
	{Master, Message} ->
	    erlang:cancel_timer(KATimer),
	    case gen_tcp:send(Socket, message:to_packet(Message)) of
		{error, Reason} -> exit({gen_tcp_send, Reason});
		ok              -> ok
	    end,
	    NewKATimer = erlang:send_after(?KA_TIMEOUT, self(), 'KEEPALIVE'),
	    loop(Master, Socket, NewKATimer, PRTimer);

	{tcp, Socket, Packet} ->
	    erlang:cancel_timer(PRTimer),
	    case message:from_packet(Packet) of
		{error, malformed_packet} -> exit(malformed_packet);
		{ok, #keepalive{}}        -> ok;
		{ok, Message}             -> Master ! {self(), Message}
	    end,
	    case inet:setopts(Socket, [{active, once}]) of
		{error, Reason} -> exit({inet_setopts, Reason});
		ok              -> ok
	    end,
	    NewPRTimer = erlang:send_after(?PR_TIMEOUT, self(), 'TIMEOUT'),
	    loop(Master, Socket, KATimer, NewPRTimer);

	'KEEPALIVE' ->
	    case gen_tcp:send(Socket, ?KEEPALIVE) of
		{error, Reason} -> exit({gen_tcp_send, Reason});
		ok              -> ok
	    end,
	    NewKATimer = erlang:send_after(?KA_TIMEOUT, self(), 'KEEPALIVE'),
	    loop(Master, Socket, NewKATimer, PRTimer);

	'TIMEOUT'            -> exit(timeout);
	{tcp_closed, Socket} -> exit(tcp_closed);
	{tcp_error,  Socket} -> exit(tcp_error);
	Other                -> exit({unexpected_message, Other})
    end.
