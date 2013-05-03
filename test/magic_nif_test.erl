-module(magic_nif_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).



from_file_test() ->
	FilePath = filename:absname(code:which(?MODULE)),
    % ?debugVal(FilePath),
    % ?debugVal(magic_nif:from_file(FilePath)),
	?assertMatch({ok, _}, magic_nif:from_file(FilePath)).

from_file_error_test() ->
	FilePath = "/path/to/no_file",
	% ?debugVal(FilePath),
	% ?debugVal(magic_nif:from_file(FilePath)),
	?assertMatch({error, _}, magic_nif:from_file(FilePath)).

from_buffer_test() ->
	FilePath = filename:absname(code:which(?MODULE)),
	% ?debugVal(FilePath),
	Size=64,
	{ok, File} = file:open(FilePath, [read,binary]),
	{ok, FileBinary} =  file:read(File, Size), 
	file:close(File), 
	% ?debugVal(magic_nif:from_buffer(FileBinary)),
	?assertMatch({ok, _}, magic_nif:from_buffer(FileBinary)).
