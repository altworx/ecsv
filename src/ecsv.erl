-module(ecsv).

%% API exports
-export([parser_init/2, parse/1, parse/2]).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).

-on_load(init/0).

%%====================================================================
%% API functions
%%====================================================================

parse(Bin) ->
    parse(Bin, parser_init($,, $")).

parse(Bin, State) ->
    parse(Bin, State, 0).

parser_init(Delim, Quote) ->
    erlang:nif_error(not_loaded, [Delim, Quote]).

%%====================================================================
%% Internal functions
%%====================================================================

parse(Bin, _, _) ->
    erlang:nif_error(not_loaded, [Bin]).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).
