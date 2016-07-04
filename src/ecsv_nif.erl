-module(ecsv_nif).

%% API exports

-export([
         parser_init/1
         , parse/3
         , write/1
         , write_lines/1
        ]).

-on_load(init/0).

-define(APPNAME, ecsv).
-define(LIBNAME, ecsv).

%%====================================================================
%% API functions
%%====================================================================

-spec parser_init(Opts :: ecsv:options()) -> ecsv:state().
parser_init(Opts) ->
    erlang:nif_error(not_loaded, [Opts]).

-spec parse(Input :: ecsv:input(), State0 :: ecsv:state(), Acc0 :: [ecsv:row()]) ->
    {ok, Acc :: [ecsv:row()], State :: ecsv:state()}.
parse(Input, _, _) ->
    erlang:nif_error(not_loaded, [Input]).

-spec write_lines([ecsv:line()]) -> iolist().
write_lines(L) ->
    erlang:nif_error(not_loaded, [L]).

-spec write(ecsv:line()) -> iolist().
write(L) ->
    erlang:nif_error(not_loaded, [L]).

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
