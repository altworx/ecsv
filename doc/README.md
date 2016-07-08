

# Erlang NIF CSV parser and writer #

Copyright (c) 2016 Altworx

__Version:__ Jul 8 2016 22:31:27

__Authors:__ Hynek Vychodil ([`hynek.vychodil@altworx.com`](mailto:hynek.vychodil@altworx.com)).

__See also:__ [ecsv](ecsv.md).

`ecsv` is fast NIF parser and writer based on [`libcsv`](https://sourceforge.net/projects/libcsv/)

The main purpose of the module is the fast parsing of CSV data in GB volumes.
This requirement leads to the necessity of stream oriented API (see [`ecsv:parse_stream/4`](ecsv.md#parse_stream-4)).


### <a name="Building">Building</a> ###

Application requires development files for [`libcsv`](https://sourceforge.net/projects/libcsv/) version 3.0.x
(tested with 3.0.3). For example in debian you need run

```
apt-get install libcsv3 libcsv-dev
```

For building you need installed [`rebar3`](https://www.rebar3.org/).

```
rebar3 compile
```

And run tests using

```
rebar3 eunit
```


### <a name="Running">Running</a> ###

For interactive Erlang shell use

```
rebar3 shell
```

Try as starter

```
1> ecsv:parse(<<"Hello,World">>).
[{<<"Hello">>,<<"World">>}]
```

See [`ecsv`](ecsv.md) for API description.


### <a name="Known_performance_caveats">Known performance caveats</a> ###

[`ecsv:write/1`](ecsv.md#write-1) and [`ecsv:write_lines/1`](ecsv.md#write_lines-1) doesn't perform as
expected. For an unknown reason, it is slower than pure Erlang implementation
when compiled using HiPE and used in a real application. On the other hand,
[`ecsv:parse_stream/5`](ecsv.md#parse_stream-5) met expectations and performs around 100MB/s on
commodity HW (i7 2.6GHz).

Current implementation uses `enif_make_new_binary()` for parsed fields. From
our experience, this call allocates small binaries on the process heap in
contrast to `enif_alloc_binary()` always allocates on the binary heap which
is slightly slower. Fields from currently parsed line is kept between NIF
calls in an own environment which could lead to bad behavior when [`parse_raw/3`](#parse_raw-3) is called with very short binaries and there is a long row with
many fields. In an extreme case, one long line with short or in worst case
empty fields will lead to quadratic behavior if fed one byte a time. If you
would like parse CSV file with more than 20kB rows with thousands of fields
you should probably use another parser or fix the issue.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="ecsv.md" class="module">ecsv</a></td></tr></table>

