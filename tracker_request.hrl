-record(tracker_request,
	{
	  info_hash,
	  peer_id,
	  port,
	  uploaded,
	  downloaded,
	  left,
	  compact = 1,
	  no_peer_id = '_',
	  event = '',
	  ip = '_',
	  numwant = '_',
	  key = '_',
	  trackerid = '_'
	 }
       ).
