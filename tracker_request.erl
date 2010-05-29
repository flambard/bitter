-module(tracker_request).

-include("tracker_request.hrl").

-export([send/2]).


send(Tiers, BaseTR) ->
    {Uploaded, Downloaded, YetToDownload} = db:bytes(),
    TR = BaseTR#tracker_request{
	   uploaded = Uploaded,
	   downloaded = Downloaded,
	   left = YetToDownload
	  },
    CGI = to_cgi(TR),
    send_request(Tiers, [], CGI).

send_request([], _Tried, _Request) -> {error, trackers_not_reachable};
send_request([Tier | Rest], Tried, Request) ->
    case send_request_to_one_of(Tier, Request) of
	{ok, {Response, RearrangedTier}} ->
	    {ok, {Response, lists:reverse(Tried, [RearrangedTier | Rest])}};
	{error, _Reason} ->
	    send_request(Rest, [Tier | Tried], Request)
    end.


send_request_to_one_of(Trackers, Request) ->
    send_request_to_one_of(Trackers, [], Request).

send_request_to_one_of([], _Tried, _Request) -> {error, trackers_not_reachable};
send_request_to_one_of([Tracker | Rest], Tried, Request) ->
    case actually_send_request(Tracker, Request) of
	{ok, Response} ->
	    %% Tracker is working; place it first in tier
	    {ok, {Response, [Tracker | lists:reverse(Tried, Rest)]}};
	{error, _Reason} ->
	    send_request_to_one_of(Rest, [Tracker | Tried], Request)
    end.


actually_send_request(Tracker, Request) ->
    case http:request(Tracker ++ Request) of
	{ok, {{_Version, 200, "OK"}, _Headers, Data}} ->
	    {DecodedData, []} = b:decode(Data),
	    {ok, tracker_response:read(DecodedData)};
	{ok, {{_Version, X, Str}, _Headers, _Data}} ->
	    io:format("Received from tracker ~p: ~b ~p~n", [Tracker, X, Str]),
	    {error, something_weird};
	{error, Reason} ->
	    {error, Reason}
    end.


to_cgi(TR) ->
    "&" ++ CGI =
	lists:foldr(
	  fun ({Key, Value}, Acc) ->
		  [$& | atom_to_list(Key) ++ [$= | tr(Key, Value) ++ Acc]]
	  end,
	  [],
	  to_keylist(TR)),
    [$? | CGI].


to_keylist(#tracker_request{info_hash = InfoHash,
			    peer_id = PeerID,
			    port = Port,
			    uploaded = Uploaded,
			    downloaded = Downloaded,
			    left = Left,
			    compact = Compact,
			    no_peer_id = NoPeerID,
			    event = Event,
			    ip = IP,
			    numwant = NumWant,
			    key = Key,
			    trackerid = TrackerID}) ->
    KeyList =
	[{info_hash, InfoHash},
	 {peer_id, PeerID},
	 {port, Port},
	 {uploaded, Uploaded},
	 {downloaded, Downloaded},
	 {left, Left},
	 {compact, Compact},
	 {no_peer_id, NoPeerID},
	 {event, Event},
	 {ip, IP},
	 {numwant, NumWant},
	 {key, Key},
	 {trackerid, TrackerID}],
    [{K, Value} || {K, Value} <- KeyList, Value =/= '_'].


tr(info_hash, InfoHash)    -> url:encode(InfoHash);
tr(peer_id, PeerID)        -> url:encode(PeerID);
tr(port, Port)             -> integer_to_list(Port);
tr(uploaded, Uploaded)     -> integer_to_list(Uploaded);
tr(downloaded, Downloaded) -> integer_to_list(Downloaded);
tr(left, Left)             -> integer_to_list(Left);
tr(compact, Compact)       -> integer_to_list(Compact);
tr(no_peer_id, NoPeerID)   -> integer_to_list(NoPeerID);
tr(event, Event)           -> atom_to_list(Event);
tr(ip, IP)                 -> IP;
tr(numwant, NumWant)       -> integer_to_list(NumWant);
tr(key, Key)               -> Key;
tr(trackerid, TrackerID)   -> url:encode(TrackerID).
