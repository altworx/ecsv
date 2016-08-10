%% @doc Erlang NIF CSV parser and writer
%%
%% @end.
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
%% See {@link parser_init/1} for details about {@link option()}.

-type state() :: any().
%% Internal parser state which is an NIF resource type. Note it's value is not
%% immutable so doesn't have to be passed from call to call. Anyway, the
%% whole API is designed and internally used as the state is immutable which
%% allows writing pure Erlang implementation with exactly same API. So please
%% do not abuse this feature because it could be incompatible in future. On
%% another side, the current implementation doesn't allow restart parsing
%% from last correct state after error.
%%
%% All functions after finished parsing return state in a condition which
%% allows starting another parsing with the same settings. It means {@link
%% parse_step/2} and {@link parse_raw/3} after `Input = eof' and {@link
%% parse_stream/4} and {@link parse_stream/5} always.
-type row() :: tuple().
-type rows() :: [row()].
-type line() :: [atom() | number() | iolist()].
-type input() :: eof | binary().

-type reader_state() :: any().
-type reader_fun(ReaderStateType) ::
    fun((ReaderState0 :: ReaderStateType) ->
        {Input :: input(), ReaderState :: ReaderStateType}).
%% Reader function which feeds data to {@link parse_stream/4} and {@link
%% parse_stream/5}.
%%
%% Note function has to return `eof' as the last `Input' value or {@link
%% parse_stream/4} and {@link parse_stream/5} will never finish otherwise.

-type callback_state() :: any().
-type callback_message() :: {eof | rows, RevRows :: rows()}.
-type callback_fun(CallBackStateType) ::
    fun((Message :: callback_message(), CallBackState0 :: CallBackStateType) ->
        CallBackState :: CallBackStateType).
%% Callback function used for processing parsed data in {@link
%% parse_stream/4} and {@link parse_stream/5}.
%%
%% Note {@link callback_message()} contains `Rows' in reverse order.

%%====================================================================
%% API functions
%%====================================================================

-spec default_block_size() -> ?BLOCK_SIZE.
%% @doc Default block size used by parser API functions
%% @see parse_raw/3.
%% @see file_reader/0.
default_block_size() ->
    ?BLOCK_SIZE.

%% @equiv parse(Bin, [])
-spec parse(Bin) -> Rows when
      Bin :: binary(),
      Rows :: rows().
parse(Bin) ->
    parse(Bin, []).

%% @doc Parse CSV data in binary and return rows in order.
-spec parse(Bin, Opts) -> Rows when
      Bin :: binary(),
      Opts :: options(),
      Rows :: rows().
parse(Bin, Opts) ->
    {ok, Acc, S} = parse_raw(Bin, parser_init(Opts), []),
    {ok, Ls, _} = parse_raw(eof, S, Acc),
    lists:reverse(Ls).

%% @doc Parse `Input' and return rows in order.
%%
%% This function allows writing simple parsing loop over chunked data. It
%% requires initialised parser state and returns rows in order. The call with
%% `eof' is necessary if there is missing line terminator after the last row.
%% Use {@link parser_raw/3} if an order of rows is not important for you or
%% you want accumulate rows and use `lists:reverse/0' afterward.
%%
%% @see parser_init/1.
%% @see parser_raw/3.
-spec parse_step(Input, State0) -> Result when
      Input :: input(),
      State0 :: state(),
      Result :: {ok, Rows, State},
      Rows :: rows(),
      State :: state().
parse_step(Bin, State) ->
    {ok, Ls, S2} = parse_raw(Bin, State, []),
    {ok, lists:reverse(Ls), S2}.

%% @equiv parse_stream(Reader, ReaderState0, CallBack, CallbackState0, [])
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

%% @doc Parse stream produced by `Reader' and process by `CallBack'.
%%
%% Function parses `Input' form `Reader' (See {@link reader_fun()}) and
%% result feeds into `CallBack' (See {@link callback_fun()}).
%%
%% Code
%%
%% ```
%% {ok, Bin} = file:read_file("test/FL_insurance_sample.csv"),
%% Rows = ecsv:parse(Bin).
%% '''
%%
%% leads in same result as
%%
%% ```
%% {ok, FH} = file:open("test/FL_insurance_sample.csv", [read, raw, binary]),
%% try ecsv:parse_stream(ecsv:file_reader(), FH, ecsv:accumulator(), []) of
%%     {_, Rows, _} -> Rows
%% after file:close(FH)
%% end.
%% '''
%%
%% or
%%
%% ```
%% {ok, Bin} = file:read_file("test/FL_insurance_sample.csv"),
%% BC = ecsv:block_chopper(ecsv:default_block_size()),
%% {_, Rows, _} = ecsv:parse_stream(BC, Bin, ecsv:accumulator(), []).
%% '''
%%
%% But `using parse_stream/4,5' allows stream processing. For example
%%
%% ```
%% Counter = fun({_, Rs}, {Fs, Ls}) ->
%%               {Fs + lists:sum([tuple_size(X) || X <- Rs]),
%%                Ls + length(Rs)}
%%           end,
%% {ok, FH2} = file:open("test/FL_insurance_sample.csv", [read, raw, binary]),
%% try ecsv:parse_stream(ecsv:file_reader(), FH2, Counter, {0, 0}) of
%%     {_, {NumberOfFields, NumberOfRows} = Result, _} -> Result
%% after file:close(FH2)
%% end.
%% '''
%%
%% will be way more efficient than reading all rows into memory for big data
%% files.
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

%% @doc Parse Input and accumulate result
%%
%% It is low-level parsing function which allows writing your own iterative
%% parsing functions like {@link parse_stream/5}. Note it returns newly
%% parsed rows in reversed order with `Acc0' content appended. All other
%% parser functions use this function internally.
%%
%% ```
%% 1> {ok, R1, S1} = ecsv:parse_raw(<<"foo\nbar">>, ecsv:parser_init([]), []).
%% {ok,[{<<"foo">>}],<<>>}
%% 2> {ok, R2, S2} = ecsv:parse_raw(<<"\nbaz\nquux">>, S1, R1).
%% {ok,[{<<"baz">>},{<<"bar">>},{<<"foo">>}],<<>>}
%% 3> ecsv:parse_raw(eof, S2, R2).
%% {ok,[{<<"quux">>},{<<"baz">>},{<<"bar">>},{<<"foo">>}],<<>>}
%% '''
%%
%% Function chops `Input' binary by {@link default_block_size/0} which should
%% take 10-15% of timeslice on decent 2.6GHz CPU and keeps VM responsive. You
%% should not call NIF function directly.
-spec parse_raw(Input, State0, Acc) -> Result when
      Input :: input(),
      State0 :: state(),
      Acc :: rows(),
      Result :: {ok, Acc, State} | {error, Acc, Reason},
      Acc :: rows(),
      State :: state(),
      Reason :: any().
parse_raw(eof, State, Acc) -> ecsv_nif:parse(eof, State, Acc);
parse_raw(<<Bin:(?BLOCK_SIZE)/bytes, Rest/bytes>>, State, Acc) ->
    case ecsv_nif:parse(Bin, State, Acc) of
        {ok, Acc2, S2} -> parse_raw(Rest, S2, Acc2);
        {error, _, _} = E -> E
    end;
parse_raw(<<>>, State, Acc) ->
    {ok, Acc, State};
parse_raw(<<_/bytes>> = Bin, State, Acc) ->
    ecsv_nif:parse(Bin, State, Acc).

%% @doc Initialise parser state
%%
%% Return `State' for parsing CSV using given 'Opts' {@link options()}. See
%% {@link state()} for more details about `State' behaviour.
%%
%% <dl>
%%  <dt>`strict'</dt>
%%  <dd>Force strict quoting rules.</dd>
%%  <dt>`null'</dt>
%%  <dd>Unquoted empty field is returned as atom `null'. Compare
%%  ```
%% 1> ecsv:parse(<<"a,,b,\"\",c">>, []).
%% [{<<"a">>,<<>>,<<"b">>,<<>>,<<"c">>}]
%% 2> ecsv:parse(<<"a,,b,\"\",c">>, [null]).
%% [{<<"a">>,null,<<"b">>,<<>>,<<"c">>}]
%%  '''
%%  </dd>
%%  <dt>`all_lines'</dt>
%%  <dd>Return all even empty rows. Compare
%%  ```
%% 1> ecsv:parse(<<"a\n\nb\n\n">>,[]).
%% [{<<"a">>},{<<"b">>}]
%% 2> ecsv:parse(<<"a\n\nb\n\n">>,[all_lines]).
%% [{<<"a">>},{},{<<"b">>},{}]
%%  '''
%%  </dd>
%%  <dt>`strict_finish'</dt>
%%  <dd>Force strict quoting rules only for last field of last row with
%%  missing line terminator.
%%  </dd>
%%  <dt>`{delimiter, D}'</dt>
%%  <dd>Define alternative delimiter character. See `{quote, Q}' for example.</dd>
%%  <dt>`{quote, Q}'</dt>
%%  <dd>Define alternative quotation character. Compare
%%  ```
%% 1> ecsv:parse(<<"'a,\",';'b,\"\",''c';\"">>, [strict]).
%% [{<<"'a">>,<<",';'b,\",''c';">>}]
%% 2> ecsv:parse(<<"'a,\",';'b,\"\",''c';\"">>, [strict, {delimiter, $;}, {quote, $'}]).
%% [{<<"a,\",">>,<<"b,\"\",'c">>,<<"\"">>}]
%%  '''
%%  </dd>
%% </dl>
-spec parser_init(Opts) -> State when
      Opts :: options(),
      State :: state().
parser_init(Opts) ->
    ecsv_nif:parser_init(Opts).

%% @doc Return file reader function.
%%
%% Returned  reader function reads `file:io_device()' using `file:read/2'
%% calls with default block size.
%%
%% @see default_block_size/0.
%% @see parse_stream/4.
%% @see parse_stream/5.
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

%% @doc Return simple binary reader function.
%%
%% Function comes handy when you have already whole CSV data but would like
%% use {@link parse_stream/4} or {@link parse_stream/5} with custom callback
%% function working on amount of data defined by `BlockSize'.
%%
%% @see parse_stream/4.
%% @see parse_stream/5.
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

%% @doc Return simple accumulator callback function.
%%
%% The callback function (see {@link callback_fun()}) reverses rows as
%% reaction to  `{eof, _}' callback message so returned final state is in
%% order when used with {@link parse_stream/4} and {@link parse_stream/5}.
%%
%% Returned callback is equivalent to
%%
%% ```
%% Accumulator = fun({eof, Rs}, Acc) -> lists:reverse(Rs ++ Acc);
%%                  ({rows, Rs}, Acc) -> Rs ++ Acc
%%               end
%% '''
%%
%% If efficiency is a concern (even only new rows are appended to
%% accumulator), consider direct using of {@link parse_raw/3}.
%%
%% @see parse_stream/4.
%% @see parse_stream/5.
%% @see parse_raw/3.
-spec accumulator() -> CallBack when
      CallBack :: fun((Input, RevRows) -> Rows),
      Input :: callback_message(),
      RevRows :: rows(),
      Rows :: rows().
accumulator() -> fun acc/2.

%% @doc Experimental CSV writer
%%
%% Function writes binaries, iolists, atoms, integers and floats. Fields are
%% quoted and quotes escaped as needed.
-spec write_lines(Lines) -> Result when
      Lines :: [line()],
      Result :: iolist().
write_lines(L) ->
    ecsv_nif:write_lines(L).

%% @equiv write_lines([Line])
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
