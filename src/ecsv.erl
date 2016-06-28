-module(ecsv).

%% API exports
-export([
         parser_init/2
         , parse/1
         , parse/2
         , parse_stream/4
         , parse_stream/5
         , file_reader/0
         , block_chopper/1
         , default_block_size/0
        ]).

-export([parse_nif/3]).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).

-define(BLOCK_SIZE, 1024*20). %% 20kB

-on_load(init/0).

%%====================================================================
%% API functions
%%====================================================================

default_block_size() ->
    ?BLOCK_SIZE.

parse(Bin) ->
    {ok, Acc, S} = parse_raw(Bin, default_state(), []),
    {ok, Ls, _} = parse_raw(eof, S, Acc),
    lists:reverse(Ls).

parse(Bin, State) ->
    parse_raw(Bin, State, []).

parse_stream(ReaderFun, ReaderState, CallbackFun, CallbackState) ->
    parse_stream(ReaderFun, ReaderState, CallbackFun, CallbackState,
                 default_state()).

parse_stream(ReaderFun, ReaderState, CallbackFun, CallbackState, State) ->
    parse_stream_(ReaderFun, ReaderState, CallbackFun, CallbackState,
                 State).

parser_init(Delim, Quote) ->
    erlang:nif_error(not_loaded, [Delim, Quote]).

file_reader() ->
    fun(FS) ->
            case file:read(FS, ?BLOCK_SIZE) of
                eof -> {eof, FS};
                {ok, Bin} -> {Bin, FS}
            end
    end.

block_chopper(BS) ->
    fun(<<>>) ->
            {eof, <<>>};
       (<<Bin:BS/bytes, Rest/bytes>>) ->
            {Bin, Rest};
       (Bin) ->
            {Bin, <<>>}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

default_state() ->
    parser_init($,, $").

parse_stream_(RF, RS, CF, CS, State) ->
    case RF(RS) of
        {eof, RS2} ->
            {ok, Ls, S2} = parse_raw(eof, State, []),
            CS2 = CF({eof, Ls}, CS),
            {RS2, CS2, S2};
        {Bin, RS2} ->
            {ok, Ls, S2} = parse_raw(Bin, State, []),
            CS2 = CF({lines, Ls}, CS),
            parse_stream_(RF, RS2, CF, CS2, S2)
    end.

parse_raw(eof, State, Acc) -> parse_nif(eof, State, Acc);
parse_raw(<<Bin:(?BLOCK_SIZE)/bytes, Rest/bytes>>, State, Acc) ->
    {ok, Acc2, S2} = parse_nif(Bin, State, Acc),
    parse_raw(Rest, S2, Acc2);
parse_raw(<<>>, State, Acc) ->
    {ok, Acc, State};
parse_raw(Bin, State, Acc) ->
    parse_nif(Bin, State, Acc).

parse_nif(Bin, _, _) ->
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
