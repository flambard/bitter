-module(listener).

-include("global.hrl").

-export([start/1, start_link/1]).


start(Handshake) ->
    Pid = spawn(fun () -> init(Handshake) end),
    Port = ?PORT,
    {ok, {Pid, Port}}.

start_link(Handshake) ->
    Pid = spawn_link(fun () -> init(Handshake) end),
    Port = ?PORT,
    {ok, {Pid, Port}}.

init(Handshake) ->
    case gen_tcp:listen(?PORT, ?INITIAL_SOCKET_SETTINGS) of
	{error, Reason} -> exit({gen_tcp_listen, Reason});
	{ok, Socket}    ->
	    io:format("Listening on port ~b.~n", [?PORT]),
	    listen(Socket, Handshake)
    end.

listen(Socket, Handshake) ->
    case gen_tcp:accept(Socket) of
	{error, Reason}    -> exit({gen_tcp_accept, Reason});
	{ok, ActiveSocket} ->
	    peer_handler:start(ActiveSocket, Handshake),
	    listen(Socket, Handshake)
    end.
