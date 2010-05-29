-record(singlefile,
	{
	  length,       %% Length of file in bytes
	  md5sum,       %% md5sum of file (optional)
	  name,         %% Filename
	  piece_length, %% Number of bytes in each piece
	  pieces        %% The SHA1 hash values as a long string
	 }).

-record(multifile,
	{
	  files,        %% A list of file records
	  name,         %% Name of top directory
	  piece_length, %% Number of bytes in each piece
	  pieces        %% The SHA1 hash values as a long string
	 }).

-record(file,
	{
	  length, %% Length of file in bytes
	  md5sum, %% md5sum of file (optional)
	  path    %% e.g: ["some", "directory", "file"] = /some/directory/file
	 }).
