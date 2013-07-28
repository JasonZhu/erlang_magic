#Erlang Magic Interface

erlang_magic is a Erlang interface to the libmagic file type identification library.  libmagic identifies file types by checking their headers according to a predefined list of file types. 


##Example Usage
	1> magic:from_file(<<"src/magic.erl">>).
	{ok,<<"text/plain">>}
	2> magic:from_file(<<"error_path">>).       
	{error, fail_to_magic}
	3> {ok, File}=file:open("src/magic.erl", [read, binary]).
	{ok,<0.39.0>}
	4> {ok, Bin} = file:read(File, 32).
	{ok,<<"-module(magic_nif).\n\n-export([fr">>}
	5> magic:from_buffer(Bin).     
	{ok,<<"text/plain">>}

