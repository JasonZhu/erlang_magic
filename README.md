#Erlang Magic Interface

erlang_magic is a Erlang interface to the libmagic file type identification library.  libmagic identifies file types by checking their headers according to a predefined list of file types. 


##Example Usage
	1> magic_nif:from_file("src/magic_nif.erl").
	{ok,"text/plain"}
	2> magic_nif:from_file("error_path").       
	{error,"cannot open `error_path' (No such file or directory)"}
	3> {ok, File}=file:open("src/magic_nif.erl", [read, binary]).
	{ok,<0.39.0>}
	4> {ok, Bin} = file:read(File, 32).
	{ok,<<"-module(magic_nif).\n\n-export([fr">>}
	5> magic_nif:from_buffer(Bin).     
	{ok,"text/plain"}

