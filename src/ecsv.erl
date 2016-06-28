-module(ecsv).

%% API exports
-export([parser_init/2, parse/1, parse/2]).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).

-define(BLOCK_SIZE, 1024*20). %% 20kB

-on_load(init/0).

%%====================================================================
%% API functions
%%====================================================================

parse(Bin) ->
    parse(Bin, parser_init($,, $")).

parse(Bin, State) ->
    parse(Bin, State, []).

parser_init(Delim, Quote) ->
    erlang:nif_error(not_loaded, [Delim, Quote]).

%%====================================================================
%% Internal functions
%%====================================================================

parse(<<>>, State, Acc) -> {ok, Acc, State};
parse(<<Bin:(?BLOCK_SIZE)/bytes, Rest/bytes>>, State, Acc) ->
    {ok, Acc2, S2} = parse_(Bin, State, Acc),
    parse(Rest, S2, Acc2);
parse(Bin, State, Acc) ->
    parse_(Bin, State, Acc).

parse_(Bin, _, _) ->
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
