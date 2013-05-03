-module(magic_nif).

-export([from_file/1]).
-export([from_buffer/1]).



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
    erlang:load_nif(filename:join(PrivDir, "magic_nif"), ?NIF_LOAD_INFO).


-spec from_file(string()) -> {ok, string()} | {error, string()}.
from_file(_FilePath) ->
    ?nif_stub.

-spec from_buffer(string()) -> {ok, string()} | {error, string()}.
from_buffer(_Binary) ->
    ?nif_stub.
