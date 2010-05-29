-module(peer).

-include("peer.hrl").
-include("global.hrl").

-export([read/1, connect/1]).


read(BDict) ->
    lists:foldl(fun rp/2, #peer{_ = '_'}, BDict).

rp({Key, Value}, P) ->
    case Key of
	<<"peer id">> -> P#peer{id = Value};
	<<"ip">>      -> P#peer{ip = binary_to_list(Value)};
	<<"port">>    -> P#peer{port = Value};
	_ignored      -> P
    end.

connect(#peer{ip = IP, port = Port}) ->
    gen_tcp:connect(IP, Port, ?INITIAL_SOCKET_SETTINGS).
