#Erlang Magic Interface

erlang_magic is a Erlang interface to the libmagic file type identification library.  libmagic identifies file types by checking their headers according to a predefined list of file types. 


##Example Usage
	1> emagic:from_file(<<"src/emagic.erl">>).
	{ok,<<"text/plain">>}
	2> emagic:from_file(<<"error_path">>).       
	{error, fail_to_magic}
	3> {ok, File}=file:open("src/emagic.erl", [read, binary]).
	{ok,<0.39.0>}
	4> {ok, Bin} = file:read(File, 32).
	{ok,<<"-module(emagic).\n\n-export([fr">>}
	5> emagic:from_buffer(Bin).     
	{ok,<<"text/plain">>}

