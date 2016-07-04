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
         , write/1
         , write_lines/1
        ]).

-export_type([
              state/0
              , options/0
              , row/0
              , line/0
              , input/0
              , reader_state/0
              , reader_fun/1
              , callback_state/0
              , callback_message/0
              , callback_fun/1
             ]).

-define(BLOCK_SIZE, 20 * 1024). %% 20kB

-type option() :: 'strict' | 'null' | 'all_lines' | 'strict_finish' |
                    {'delimiter', byte()} | {'quote', byte()}.
-type options() :: [option()].

-type state() :: any().
-type row() :: tuple().
-type line() :: [atom() | number() | iolist()].
-type input() :: eof | binary().

-type reader_state() :: any().
-type reader_fun(ReaderStateType) ::
    fun((State0 :: ReaderStateType) -> State :: ReaderStateType).

-type callback_state() :: any().
-type callback_message() :: {eof | lines, [row()]}.
-type callback_fun(CallBackStateType) ::
    fun((Message :: callback_message(), State0 :: CallBackStateType) ->
        State :: CallBackStateType).

%%====================================================================
%% API functions
%%====================================================================

-spec default_block_size() -> ?BLOCK_SIZE.
default_block_size() ->
    ?BLOCK_SIZE.

-spec parse(Bin :: binary()) -> [row()].
parse(Bin) ->
    parse(Bin, []).

-spec parse(Bin :: binary(), Opts :: options()) -> [row()].
parse(Bin, Opts) ->
    {ok, Acc, S} = parse_raw(Bin, parser_init(Opts), []),
    {ok, Ls, _} = parse_raw(eof, S, Acc),
    lists:reverse(Ls).

-spec parse_step(Input :: input(), State0 :: state()) -> {ok, [row()], State :: state()}.
parse_step(Bin, State) ->
    {ok, Ls, S2} = parse_raw(Bin, State, []),
    {ok, lists:reverse(Ls), S2}.

-spec parse_stream(
        reader_fun(ReaderStateType), ReaderState0 :: ReaderStateType,
        callback_fun(CallBackStateType), CallbackState0 :: CallBackStateType) ->
    {ReaderState :: ReaderStateType, CallbackState :: CallBackStateType,
     State :: state()}.
parse_stream(Reader, ReaderState0, CallBack, CallbackState0) ->
    parse_stream(Reader, ReaderState0, CallBack, CallbackState0, parser_init([])).

-spec parse_stream(
        reader_fun(ReaderStateType), ReaderState0 :: ReaderStateType,
        callback_fun(CallBackStateType), CallbackState0 :: CallBackStateType,
        State0 :: state()) ->
    {ReaderState :: ReaderStateType, CallBackState :: CallBackStateType,
     State :: state()}.
parse_stream(Reader, ReaderState0, CallBack, CallbackState0, State) ->
    parse_stream_(Reader, ReaderState0, CallBack, CallbackState0, State).

-spec parser_init(Opts :: options()) -> state().
parser_init(Opts) ->
    ecsv_nif:parser_init(Opts).

-spec file_reader() -> reader_fun(file:io_device()).
file_reader() ->
    fun(FS) ->
            case file:read(FS, ?BLOCK_SIZE) of
                eof -> {eof, FS};
                {ok, Bin} -> {Bin, FS}
            end
    end.

-spec block_chopper(BlockSize :: pos_integer()) -> reader_fun(binary()).
block_chopper(BlockSize) ->
    fun(<<>>) ->
            {eof, <<>>};
       (<<Bin:BlockSize/bytes, Rest/bytes>>) ->
            {Bin, Rest};
       (<<_/bytes>> = Bin) ->
            {Bin, <<>>}
    end.

-spec write_lines([line()]) -> iolist().
write_lines(L) ->
    ecsv_nif:write_lines(L).

-spec write(line()) -> iolist().
write(L) ->
    ecsv_nif:write(L).

%%====================================================================
%% Internal functions
%%====================================================================

-spec parse_stream_(
        RF :: reader_fun(RST), RS0 :: RST,
        CF :: callback_fun(CST), CS0 :: CST,
        State0 :: state()) -> {RS :: RST, CS :: CST, State :: state()}.
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
parse_raw(eof, State, Acc) -> ecsv_nif:parse(eof, State, Acc);
parse_raw(<<Bin:(?BLOCK_SIZE)/bytes, Rest/bytes>>, State, Acc) ->
    {ok, Acc2, S2} = ecsv_nif:parse(Bin, State, Acc),
    parse_raw(Rest, S2, Acc2);
parse_raw(<<>>, State, Acc) ->
    {ok, Acc, State};
parse_raw(<<_/bytes>> = Bin, State, Acc) ->
    ecsv_nif:parse(Bin, State, Acc).
