-module(ecsv).

%% API exports
-export([
         parser_init/1
         , parse/1
         , parse/2
         , parse_step/2
         , parse_stream/4
         , parse_stream/5
         , parse_raw/3
         , file_reader/0
         , block_chopper/1
         , accumulator/0
         , default_block_size/0
         , write/1
         , write_lines/1
        ]).

-export_type([
              state/0
              , options/0
              , rows/0
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
-type rows() :: [row()].
-type line() :: [atom() | number() | iolist()].
-type input() :: eof | binary().

-type reader_state() :: any().
-type reader_fun(ReaderStateType) ::
    fun((State0 :: ReaderStateType) -> State :: ReaderStateType).

-type callback_state() :: any().
-type callback_message() :: {eof | rows, RevRows :: rows()}.
-type callback_fun(CallBackStateType) ::
    fun((Message :: callback_message(), State0 :: CallBackStateType) ->
        State :: CallBackStateType).

%%====================================================================
%% API functions
%%====================================================================

-spec default_block_size() -> ?BLOCK_SIZE.
default_block_size() ->
    ?BLOCK_SIZE.

-spec parse(Bin) -> Rows when
      Bin :: binary(),
      Rows :: rows().
parse(Bin) ->
    parse(Bin, []).

-spec parse(Bin, Opts) -> Rows when
      Bin :: binary(),
      Opts :: options(),
      Rows :: rows().
parse(Bin, Opts) ->
    {ok, Acc, S} = parse_raw(Bin, parser_init(Opts), []),
    {ok, Ls, _} = parse_raw(eof, S, Acc),
    lists:reverse(Ls).

-spec parse_step(Input, State0) -> Result when
      Input :: input(),
      State0 :: state(),
      Result :: {ok, Rows, State},
      Rows :: rows(),
      State :: state().
parse_step(Bin, State) ->
    {ok, Ls, S2} = parse_raw(Bin, State, []),
    {ok, lists:reverse(Ls), S2}.

-spec parse_stream(Reader, ReaderState0, CallBack, CallbackState0) ->
    Result when
      Reader :: reader_fun(ReaderStateType),
      ReaderState0 :: ReaderStateType,
      CallBack :: callback_fun(CallBackStateType),
      CallbackState0 :: CallBackStateType,
      Result :: {ReaderState, CallbackState, State},
      ReaderState :: ReaderStateType,
      CallbackState :: CallBackStateType,
      State :: state().
parse_stream(Reader, ReaderState0, CallBack, CallbackState0) ->
    parse_stream(Reader, ReaderState0, CallBack, CallbackState0, []).

-spec parse_stream(Reader, ReaderState0, CallBack, CallbackState0, StateOrOpts) ->
    Result when
      Reader :: reader_fun(ReaderStateType),
      ReaderState0 :: ReaderStateType,
      CallBack :: callback_fun(CallBackStateType),
      CallbackState0 :: CallBackStateType,
      StateOrOpts :: state() | options(),
      Result :: {ReaderState, CallbackState, State},
      ReaderState :: ReaderStateType,
      CallbackState :: CallBackStateType,
      State :: state().
parse_stream(Reader, ReaderState0, CallBack, CallbackState0, State) ->
    parse_stream_(Reader, ReaderState0, CallBack, CallbackState0,
                  if
                      is_list(State) -> parser_init(State);
                      true           -> State
                  end).

-spec parse_raw(Input, State0, Acc) -> Result when
      Input :: input(),
      State0 :: state(),
      Acc :: rows(),
      Result :: {ok, RevRows, State},
      RevRows :: rows(),
      State :: state().
parse_raw(eof, State, Acc) -> ecsv_nif:parse(eof, State, Acc);
parse_raw(<<Bin:(?BLOCK_SIZE)/bytes, Rest/bytes>>, State, Acc) ->
    {ok, Acc2, S2} = ecsv_nif:parse(Bin, State, Acc),
    parse_raw(Rest, S2, Acc2);
parse_raw(<<>>, State, Acc) ->
    {ok, Acc, State};
parse_raw(<<_/bytes>> = Bin, State, Acc) ->
    ecsv_nif:parse(Bin, State, Acc).

-spec parser_init(Opts) -> State when
      Opts :: options(),
      State :: state().
parser_init(Opts) ->
    ecsv_nif:parser_init(Opts).

-spec file_reader() -> Reader when
      Reader :: reader_fun(FH),
      FH :: file:io_device().
file_reader() ->
    fun(FS) ->
            case file:read(FS, ?BLOCK_SIZE) of
                eof -> {eof, FS};
                {ok, Bin} -> {Bin, FS}
            end
    end.

-spec block_chopper(BlockSize) -> Reader when
      BlockSize :: pos_integer(),
      Reader :: reader_fun(State),
      State :: binary().
block_chopper(BlockSize) when is_integer(BlockSize), BlockSize > 0 ->
    fun(<<>>) ->
            {eof, <<>>};
       (<<Bin:BlockSize/bytes, Rest/bytes>>) ->
            {Bin, Rest};
       (<<_/bytes>> = Bin) ->
            {Bin, <<>>}
    end.

-spec accumulator() -> CallBack when
      CallBack :: fun((Input, RevRows) -> Rows),
      Input :: callback_message(),
      RevRows :: rows(),
      Rows :: rows().
accumulator() -> fun acc/2.

-spec write_lines(Lines) -> Result when
      Lines :: [line()],
      Result :: iolist().
write_lines(L) ->
    ecsv_nif:write_lines(L).

-spec write(Line) -> Result when
      Line :: line(),
      Result :: iolist().
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
            CS2 = CF({rows, Ls}, CS),
            parse_stream_(RF, RS2, CF, CS2, S2)
    end.

-spec acc(callback_message(), rows()) -> rows().
acc({eof, X}, Acc) -> lists:reverse(X ++ Acc);
acc({rows, X}, Acc) -> X ++ Acc.
