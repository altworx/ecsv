-module(ecsv).

%% API exports
-export([
         parser_init/1
         , parse/1
         , parse/2
         , parse_step/2
         , parse_stream/4
         , parse_stream/5
         , file_reader/0
         , block_chopper/1
         , default_block_size/0
        ]).

-export_type([
              state/0
              , options/0
              , row/0
              , input/0
              , reader_state/0
              , reader_fun/1
              , callback_state/0
              , callback_message/0
              , callback_fun/1
             ]).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).

-define(BLOCK_SIZE, 1024*20). %% 20kB

-on_load(init/0).

-type option() :: 'strict' | 'null' | 'all_lines' | 'strict_finish' |
                    {'delimiter', byte()} | {'quote', byte()}.
-type options() :: [option()].

-type state() :: any().
-type row() :: tuple().
-type input() :: eof | (Bin :: binary()).

-type reader_state() :: any().
-type reader_fun(RS) :: fun((State0 :: RS) -> State :: RS).

-type callback_state() :: any().
-type callback_message() :: {eof | lines, [row()]}.
-type callback_fun(CS) :: fun((Message :: callback_message(), State0 :: CS) -> State :: CS).

%%====================================================================
%% API functions
%%====================================================================

default_block_size() ->
    ?BLOCK_SIZE.

-spec(parse(Bin :: binary()) -> [row()]).
parse(Bin) ->
    parse(Bin, []).

-spec(parse(Bin :: binary(), Opts :: options()) -> [row()]).
parse(Bin, Opts) ->
    {ok, Acc, S} = parse_raw(Bin, parser_init(Opts), []),
    {ok, Ls, _} = parse_raw(eof, S, Acc),
    lists:reverse(Ls).

-spec(parse_step(Input :: input(), State :: state()) -> {ok, [row()], State2 :: state()}).
parse_step(Bin, State) ->
    {ok, Ls, S2} = parse_raw(Bin, State, []),
    {ok, lists:reverse(Ls), S2}.

-spec(parse_stream(RF :: reader_fun(RST), RS0 :: RST, CF :: callback_fun(CST), CS0 :: CST) -> {RS :: RST, CS :: CST, State :: state()}).
parse_stream(ReaderFun, ReaderState, CallbackFun, CallbackState) ->
    parse_stream(ReaderFun, ReaderState, CallbackFun, CallbackState,
                 parser_init([])).


-spec(parse_stream(RF :: reader_fun(RST), RS0 :: RST, CF :: callback_fun(CST), CS0 :: CST, State0 :: state()) -> {RS :: RST, CS :: CST, State :: state()}).
parse_stream(ReaderFun, ReaderState, CallbackFun, CallbackState, State) ->
    parse_stream_(ReaderFun, ReaderState, CallbackFun, CallbackState,
                 State).

-spec parser_init(Opts :: options()) -> state().
parser_init(Opts) ->
    erlang:nif_error(not_loaded, [Opts]).

-spec(file_reader() -> reader_fun(reader_state())).
file_reader() ->
    fun(FS) ->
            case file:read(FS, ?BLOCK_SIZE) of
                eof -> {eof, FS};
                {ok, Bin} -> {Bin, FS}
            end
    end.

-spec(block_chopper(BlockSize :: pos_integer()) -> reader_fun(reader_state())).
block_chopper(BlockSize) ->
    fun(<<>>) ->
            {eof, <<>>};
       (<<Bin:BlockSize/bytes, Rest/bytes>>) ->
            {Bin, Rest};
       (Bin) ->
            {Bin, <<>>}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec(parse_stream_(RF :: reader_fun(RST), RS0 :: RST, CF :: callback_fun(CST), CS0 :: CST, State0 :: state()) -> {RS :: RST, CS :: CST, State :: state()}).
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

-spec(parse_raw(Input :: input(), State0 :: state(), Acc :: [row()]) -> {ok, [row()], State :: state()}).
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
