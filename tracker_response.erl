-module(tracker_response).

-include("tracker_response.hrl").
-include("peer.hrl").

-export([read/1]).


read(Response) ->
    lists:foldl(fun tr/2, #tracker_response{_ = '_'}, Response).

tr({Key, Value}, TR) ->
    case Key of
	<<"failure reason">>                      ->   #tracker_response{failure_reason = Value, _ = '_'};
	<<"warning message">>                     -> TR#tracker_response{warning_message = Value};
	<<"interval">> when is_integer(Value)     -> TR#tracker_response{interval = 1000 * Value};
	<<"min interval">> when is_integer(Value) -> TR#tracker_response{min_interval = 1000 * Value};
	<<"tracker id">>                          -> TR#tracker_response{tracker_id = Value};
	<<"complete">> when is_integer(Value)     -> TR#tracker_response{complete = Value};
	<<"incomplete">> when is_integer(Value)   -> TR#tracker_response{incomplete = Value};
	<<"peers">> when is_list(Value)           -> TR#tracker_response{peers = [peer:read(P) || P <- Value]};
	<<"peers">> when is_binary(Value)         -> TR#tracker_response{peers = read_compact_peers(Value)};
	_ignored                                  -> TR
    end.

read_compact_peers(<<>>) -> [];
read_compact_peers(<<A, B, C, D, Port:16, Rest/binary>>) ->
    %% IP = string:join(lists:map(fun integer_to_list/1, [A, B, C, D]), "."),
    IP = lists:concat([A, '.', B, '.', C, '.', D]),
    [#peer{ip = IP, port = Port} | read_compact_peers(Rest)].
