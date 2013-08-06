-module(emagic).

-export([from_file/1]).
-export([from_buffer/1]).

-export([magic/3]).


-define(FLAG_FROM_FILE, 0).
-define(FLAG_FROM_BUFFER, 1).

-define(TYPE_MAGIC_MIME, 0).
-define(TYPE_MAGIC_MIME_TYPE, 1).



-on_load(load_nif/0).
%% NIF version number (on the load_info argument of load_nif/2)
-define(NIF_LOAD_INFO, 101).

-define(nif_stub,
        erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).



%% On-load callback

%% @doc Loading NIF shared library file, used at the on-load callback.
load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
          {error, _} ->
              EbinDir = filename:dirname(code:which(?MODULE)),
              AppPath = filename:dirname(EbinDir),
              filename:join(AppPath, "priv");
          Path ->
              Path
          end,
    erlang:load_nif(filename:join(PrivDir, "emagic"), ?NIF_LOAD_INFO).


-spec from_file(string() | binary()) -> {ok, binary()} | {error, term()}.
from_file(FilePath) when is_list(FilePath)->
    from_file(list_to_binary(FilePath));
from_file(FilePath) when is_binary(FilePath) ->
    magic(FilePath, ?FLAG_FROM_FILE).

-spec from_buffer(binary()) -> {ok, binary()} | {error, term()}.
from_buffer(Binary) ->
    magic(Binary, ?FLAG_FROM_BUFFER).

magic(Binary, Flag) when is_binary(Binary), is_integer(Flag)->
    magic(Binary, Flag, ?TYPE_MAGIC_MIME_TYPE).

-spec magic(binary(), integer(), integer()) -> {ok, binary()} | {error, term()}.
magic(Binary, Flag, Type) when is_binary(Binary), is_integer(Flag), is_integer(Type)->
    ?nif_stub.




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


from_file_test() ->
    FilePath = filename:absname(code:which(?MODULE)),
    ?debugVal(FilePath),
    ?assertMatch({ok, <<"application/octet-stream">>}, from_file(FilePath)).

file_not_found_test() ->
    FilePath = "/file/not/found",
    ?assertMatch({error, _}, from_file(FilePath)).

from_buffer_test() ->
    FilePath = filename:absname(code:which(?MODULE)),
    Size=64,
    {ok, File} = file:open(FilePath, [read,binary]),
    {ok, FileBinary} =  file:read(File, Size), 
    file:close(File), 
    ?assertMatch({ok, <<"application/octet-stream">>}, from_buffer(FileBinary)).
-endif.