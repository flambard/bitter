-record(torrent,
	{
	  info,          %% Description of file(s)
	  announce,      %% Tracker URL
	  announce_list, %% Tracker URLs (optional)
	  creation_date, %% Creation date of torrent (optional)
	  comment,       %% Comments (optional)
	  created_by,    %% Name and version of torrent creator program (optional)
	  info_hash      %% A hash of the info part (not in protocol)
	 }).
