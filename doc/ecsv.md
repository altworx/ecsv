

# Module ecsv #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Erlang NIF CSV parser and writer.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_fun">callback_fun()</a> ###


<pre><code>
callback_fun(CallBackStateType) = fun((Message::<a href="#type-callback_message">callback_message()</a>, CallBackState0::CallBackStateType) -&gt; CallBackState::CallBackStateType)
</code></pre>

 Callback function used for processing parsed data in [`parse_stream/4`](#parse_stream-4) and [`parse_stream/5`](#parse_stream-5).

Note [`callback_message()`](#type-callback_message) contains `Rows` in reverse order.



### <a name="type-callback_message">callback_message()</a> ###


<pre><code>
callback_message() = {eof | rows, RevRows::<a href="#type-rows">rows()</a>}
</code></pre>




### <a name="type-callback_state">callback_state()</a> ###


<pre><code>
callback_state() = any()
</code></pre>




### <a name="type-input">input()</a> ###


<pre><code>
input() = eof | binary()
</code></pre>




### <a name="type-line">line()</a> ###


<pre><code>
line() = [atom() | number() | iolist()]
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = strict | null | all_lines | strict_finish | {delimiter, byte()} | {quote, byte()}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>

 See [`parser_init/1`](#parser_init-1) for details about [`option()`](#type-option).



### <a name="type-reader_fun">reader_fun()</a> ###


<pre><code>
reader_fun(ReaderStateType) = fun((ReaderState0::ReaderStateType) -&gt; {Input::<a href="#type-input">input()</a>, ReaderState::ReaderStateType})
</code></pre>

 Reader function which feeds data to [`parse_stream/4`](#parse_stream-4) and [`parse_stream/5`](#parse_stream-5).

Note function has to return `eof` as the last `Input` value or [`parse_stream/4`](#parse_stream-4) and [`parse_stream/5`](#parse_stream-5) will never finish otherwise.



### <a name="type-reader_state">reader_state()</a> ###


<pre><code>
reader_state() = any()
</code></pre>




### <a name="type-row">row()</a> ###


<pre><code>
row() = tuple()
</code></pre>




### <a name="type-rows">rows()</a> ###


<pre><code>
rows() = [<a href="#type-row">row()</a>]
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = any()
</code></pre>

 Internal parser state which is an NIF resource type. Note it's value is not
immutable so doesn't have to be passed from call to call. Anyway, the
whole API is designed and internally used as the state is immutable which
allows writing pure Erlang implementation with exactly same API. So please
do not abuse this feature because it could be incompatible in future. On
another side, the current implementation doesn't allow restart parsing
from last correct state after error.

All functions after finished parsing return state in a condition which
allows starting another parsing with the same settings. It means [`parse_step/2`](#parse_step-2) and [`parse_raw/3`](#parse_raw-3) after `Input = eof` and [`parse_stream/4`](#parse_stream-4) and [`parse_stream/5`](#parse_stream-5) always.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accumulator-0">accumulator/0</a></td><td>Return simple accumulator callback function.</td></tr><tr><td valign="top"><a href="#block_chopper-1">block_chopper/1</a></td><td>Return simple binary reader function.</td></tr><tr><td valign="top"><a href="#default_block_size-0">default_block_size/0</a></td><td>Default block size used by parser API functions.</td></tr><tr><td valign="top"><a href="#file_reader-0">file_reader/0</a></td><td>Return file reader function.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Equivalent to <a href="#parse-2"><tt>parse(Bin, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parse CSV data in binary and return rows in order.</td></tr><tr><td valign="top"><a href="#parse_raw-3">parse_raw/3</a></td><td>Parse Input and accumulate result.</td></tr><tr><td valign="top"><a href="#parse_step-2">parse_step/2</a></td><td>Parse <code>Input</code> and return rows in order.</td></tr><tr><td valign="top"><a href="#parse_stream-4">parse_stream/4</a></td><td>Equivalent to <a href="#parse_stream-5"><tt>parse_stream(Reader, ReaderState0, CallBack,
CallbackState0, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#parse_stream-5">parse_stream/5</a></td><td>Parse stream produced by <code>Reader</code> and process by <code>CallBack</code>.</td></tr><tr><td valign="top"><a href="#parser_init-1">parser_init/1</a></td><td>Initialise parser state.</td></tr><tr><td valign="top"><a href="#write-1">write/1</a></td><td>Equivalent to <a href="#write_lines-1"><tt>write_lines([Line])</tt></a>.</td></tr><tr><td valign="top"><a href="#write_lines-1">write_lines/1</a></td><td>Experimental CSV writer.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accumulator-0"></a>

### accumulator/0 ###

<pre><code>
accumulator() -&gt; CallBack
</code></pre>

<ul class="definitions"><li><code>CallBack = fun((Input, RevRows) -&gt; Rows)</code></li><li><code>Input = <a href="#type-callback_message">callback_message()</a></code></li><li><code>RevRows = <a href="#type-rows">rows()</a></code></li><li><code>Rows = <a href="#type-rows">rows()</a></code></li></ul>

Return simple accumulator callback function.

The callback function (see [`callback_fun()`](#type-callback_fun)) reverses rows as
reaction to`{eof, _}` callback message so returned final state is in
order when used with [`parse_stream/4`](#parse_stream-4) and [`parse_stream/5`](#parse_stream-5).

Returned callback is equivalent to

```
  Accumulator = fun({eof, Rs}, Acc) -> lists:reverse(Rs ++ Acc);
                   ({rows, Rs}, Acc) -> Rs ++ Acc
                end
```

If efficiency is a concern (even only new rows are appended to
accumulator), consider direct using of [`parse_raw/3`](#parse_raw-3).

__See also:__ [parse_raw/3](#parse_raw-3), [parse_stream/4](#parse_stream-4), [parse_stream/5](#parse_stream-5).

<a name="block_chopper-1"></a>

### block_chopper/1 ###

<pre><code>
block_chopper(BlockSize) -&gt; Reader
</code></pre>

<ul class="definitions"><li><code>BlockSize = pos_integer()</code></li><li><code>Reader = <a href="#type-reader_fun">reader_fun</a>(State)</code></li><li><code>State = binary()</code></li></ul>

Return simple binary reader function.

Function comes handy when you have already whole CSV data but would like
use [`parse_stream/4`](#parse_stream-4) or [`parse_stream/5`](#parse_stream-5) with custom callback
function working on amount of data defined by `BlockSize`.

__See also:__ [parse_stream/4](#parse_stream-4), [parse_stream/5](#parse_stream-5).

<a name="default_block_size-0"></a>

### default_block_size/0 ###

<pre><code>
default_block_size() -&gt; 20480
</code></pre>
<br />

Default block size used by parser API functions

__See also:__ [file_reader/0](#file_reader-0), [parse_raw/3](#parse_raw-3).

<a name="file_reader-0"></a>

### file_reader/0 ###

<pre><code>
file_reader() -&gt; Reader
</code></pre>

<ul class="definitions"><li><code>Reader = <a href="#type-reader_fun">reader_fun</a>(FH)</code></li><li><code>FH = <a href="file.md#type-io_device">file:io_device()</a></code></li></ul>

Return file reader function.

Returned  reader function reads `file:io_device()` using `file:read/2`
calls with default block size.

__See also:__ [default_block_size/0](#default_block_size-0), [parse_stream/4](#parse_stream-4), [parse_stream/5](#parse_stream-5).

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin) -&gt; Rows
</code></pre>

<ul class="definitions"><li><code>Bin = binary()</code></li><li><code>Rows = <a href="#type-rows">rows()</a></code></li></ul>

Equivalent to [`parse(Bin, [])`](#parse-2).

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(Bin, Opts) -&gt; Rows
</code></pre>

<ul class="definitions"><li><code>Bin = binary()</code></li><li><code>Opts = <a href="#type-options">options()</a></code></li><li><code>Rows = <a href="#type-rows">rows()</a></code></li></ul>

Parse CSV data in binary and return rows in order.

<a name="parse_raw-3"></a>

### parse_raw/3 ###

<pre><code>
parse_raw(Input, State0, Acc) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Input = <a href="#type-input">input()</a></code></li><li><code>State0 = <a href="#type-state">state()</a></code></li><li><code>Acc = <a href="#type-rows">rows()</a></code></li><li><code>Result = {ok, Acc, State} | {error, Acc, Reason}</code></li><li><code>Acc = <a href="#type-rows">rows()</a></code></li><li><code>State = <a href="#type-state">state()</a></code></li><li><code>Reason = any()</code></li></ul>

Parse Input and accumulate result

It is low-level parsing function which allows writing your own iterative
parsing functions like [`parse_stream/5`](#parse_stream-5). Note it returns newly
parsed rows in reversed order with `Acc0` content appended. All other
parser functions use this function internally.

```
  1> {ok, R1, S1} = ecsv:parse_raw(<<"foo\nbar">>, ecsv:parser_init([]), []).
  {ok,[{<<"foo">>}],<<>>}
  2> {ok, R2, S2} = ecsv:parse_raw(<<"\nbaz\nquux">>, S1, R1).
  {ok,[{<<"baz">>},{<<"bar">>},{<<"foo">>}],<<>>}
  3> ecsv:parse_raw(eof, S2, R2).
  {ok,[{<<"quux">>},{<<"baz">>},{<<"bar">>},{<<"foo">>}],<<>>}
```

Function chops `Input` binary by [`default_block_size/0`](#default_block_size-0) which should
take 10-15% of timeslice on decent 2.6GHz CPU and keeps VM responsive. You
should not call NIF function directly.

<a name="parse_step-2"></a>

### parse_step/2 ###

<pre><code>
parse_step(Input, State0) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Input = <a href="#type-input">input()</a></code></li><li><code>State0 = <a href="#type-state">state()</a></code></li><li><code>Result = {ok, Rows, State}</code></li><li><code>Rows = <a href="#type-rows">rows()</a></code></li><li><code>State = <a href="#type-state">state()</a></code></li></ul>

Parse `Input` and return rows in order.

This function allows writing simple parsing loop over chunked data. It
requires initialised parser state and returns rows in order. The call with
`eof` is necessary if there is missing line terminator after the last row.
Use [`parser_raw/3`](#parser_raw-3) if an order of rows is not important for you or
you want accumulate rows and use `lists:reverse/0` afterward.

__See also:__ [parser_init/1](#parser_init-1), [parser_raw/3](#parser_raw-3).

<a name="parse_stream-4"></a>

### parse_stream/4 ###

<pre><code>
parse_stream(Reader, ReaderState0, CallBack, CallbackState0) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Reader = <a href="#type-reader_fun">reader_fun</a>(ReaderStateType)</code></li><li><code>ReaderState0 = ReaderStateType</code></li><li><code>CallBack = <a href="#type-callback_fun">callback_fun</a>(CallBackStateType)</code></li><li><code>CallbackState0 = CallBackStateType</code></li><li><code>Result = {ReaderState, CallbackState, State}</code></li><li><code>ReaderState = ReaderStateType</code></li><li><code>CallbackState = CallBackStateType</code></li><li><code>State = <a href="#type-state">state()</a></code></li></ul>

Equivalent to [`parse_stream(Reader, ReaderState0, CallBack,CallbackState0, [])`](#parse_stream-5).

<a name="parse_stream-5"></a>

### parse_stream/5 ###

<pre><code>
parse_stream(Reader, ReaderState0, CallBack, CallbackState0, StateOrOpts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Reader = <a href="#type-reader_fun">reader_fun</a>(ReaderStateType)</code></li><li><code>ReaderState0 = ReaderStateType</code></li><li><code>CallBack = <a href="#type-callback_fun">callback_fun</a>(CallBackStateType)</code></li><li><code>CallbackState0 = CallBackStateType</code></li><li><code>StateOrOpts = <a href="#type-state">state()</a> | <a href="#type-options">options()</a></code></li><li><code>Result = {ReaderState, CallbackState, State}</code></li><li><code>ReaderState = ReaderStateType</code></li><li><code>CallbackState = CallBackStateType</code></li><li><code>State = <a href="#type-state">state()</a></code></li></ul>

Parse stream produced by `Reader` and process by `CallBack`.

Function parses `Input` form `Reader` (See [`reader_fun()`](#type-reader_fun)) and
result feeds into `CallBack` (See [`callback_fun()`](#type-callback_fun)).

Code

```
  {ok, Bin} = file:read_file("test/FL_insurance_sample.csv"),
  Rows = ecsv:parse(Bin).
```

leads in same result as

```
  {ok, FH} = file:open("test/FL_insurance_sample.csv", [read, raw, binary]),
  try ecsv:parse_stream(ecsv:file_reader(), FH, ecsv:accumulator(), []) of
      {_, Rows, _} -> Rows
  after file:close(FH)
  end.
```

or

```
  {ok, Bin} = file:read_file("test/FL_insurance_sample.csv"),
  BC = ecsv:block_chopper(ecsv:default_block_size()),
  {_, Rows, _} = ecsv:parse_stream(BC, Bin, ecsv:accumulator(), []).
```

But `using parse_stream/4,5` allows stream processing. For example

```
  Counter = fun({_, Rs}, {Fs, Ls}) ->
                {Fs + lists:sum([tuple_size(X) || X <- Rs]),
                 Ls + length(Rs)}
            end,
  {ok, FH2} = file:open("test/FL_insurance_sample.csv", [read, raw, binary]),
  try ecsv:parse_stream(ecsv:file_reader(), FH2, Counter, {0, 0}) of
      {_, {NumberOfFields, NumberOfRows} = Result, _} -> Result
  after file:close(FH2)
  end.
```

will be way more efficient than reading all rows into memory for big data
files.

<a name="parser_init-1"></a>

### parser_init/1 ###

<pre><code>
parser_init(Opts) -&gt; State
</code></pre>

<ul class="definitions"><li><code>Opts = <a href="#type-options">options()</a></code></li><li><code>State = <a href="#type-state">state()</a></code></li></ul>

Initialise parser state

Return `State` for parsing CSV using given 'Opts' [`options()`](#type-options). See
[`state()`](#type-state) for more details about `State` behaviour.



<dt><code>strict</code></dt>




<dd>Force strict quoting rules.</dd>




<dt><code>null</code></dt>




<dd>Unquoted empty field is returned as atom <code>null</code>. Compare
<pre>  1> ecsv:parse(<<"a,,b,\"\",c">>, []).
  [{<<"a">>,<<>>,<<"b">>,<<>>,<<"c">>}]
  2> ecsv:parse(<<"a,,b,\"\",c">>, [null]).
  [{<<"a">>,null,<<"b">>,<<>>,<<"c">>}]</pre>
</dd>




<dt><code>all_lines</code></dt>




<dd>Return all even empty rows. Compare
<pre>  1> ecsv:parse(<<"a\n\nb\n\n">>,[]).
  [{<<"a">>},{<<"b">>}]
  2> ecsv:parse(<<"a\n\nb\n\n">>,[all_lines]).
  [{<<"a">>},{},{<<"b">>},{}]</pre>
</dd>




<dt><code>strict_finish</code></dt>




<dd>Force strict quoting rules only for last field of last row with
missing line terminator.
</dd>




<dt><code>{delimiter, D}</code></dt>




<dd>Define alternative delimiter character. See <code>{quote, Q}</code> for example.</dd>




<dt><code>{quote, Q}</code></dt>




<dd>Define alternative quotation character. Compare
<pre>  1> ecsv:parse(<<"'a,\",';'b,\"\",''c';\"">>, [strict]).
  [{<<"'a">>,<<",';'b,\",''c';">>}]
  2> ecsv:parse(<<"'a,\",';'b,\"\",''c';\"">>, [strict, {delimiter, $;}, {quote, $'}]).
  [{<<"a,\",">>,<<"b,\"\",'c">>,<<"\"">>}]</pre>
</dd>



<a name="write-1"></a>

### write/1 ###

<pre><code>
write(Line) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Line = <a href="#type-line">line()</a></code></li><li><code>Result = iolist()</code></li></ul>

Equivalent to [`write_lines([Line])`](#write_lines-1).

<a name="write_lines-1"></a>

### write_lines/1 ###

<pre><code>
write_lines(Lines) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Lines = [<a href="#type-line">line()</a>]</code></li><li><code>Result = iolist()</code></li></ul>

Experimental CSV writer

Function writes binaries, iolists, atoms, integers and floats. Fields are
quoted and quotes escaped as needed.

