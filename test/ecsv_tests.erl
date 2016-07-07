-module(ecsv_tests).

-include_lib("eunit/include/eunit.hrl").

test_data() -> [[a, <<"b\"oo">>, c], [1, 2, 3], [d, e, <<"foo\"">>], [4, 5, 6]].

to_binaries(X) ->
    if
        is_binary(X)  -> X;
        is_list(X)    -> [ to_binaries(Y) || Y <- X ];
        is_atom(X)    -> atom_to_binary(X, latin1);
        is_integer(X) -> integer_to_binary(X);
        is_float(X)   -> float_to_binary(X);
        true          -> error(badarg)
    end.

data_to_rows(D) ->
    [ list_to_tuple(to_binaries(X)) || X <- D ].

list_reader([])    -> {eof, []};
list_reader([H|T]) -> {H, T}.

stream_parse(L, Opts) ->
    element(2, ecsv:parse_stream(fun list_reader/1, L, ecsv:accumulator(), [], Opts)).

split_bin(X, Bin) -> 
    <<A:X/bytes, B/bytes>> = Bin,
    [A, B].

parse_test_() ->
    D    = test_data(),
    Rs   = data_to_rows(D),
    Bin  = iolist_to_binary(ecsv:write_lines(D)),
    Size = byte_size(Bin),
    [
     [ {Prefix ++ "basic", ?_assertEqual(Rs, ecsv:parse(B, Opts))},
       [ {Prefix ++ "split at " ++ integer_to_list(X),
          ?_assertEqual(
             Rs,
             stream_parse(split_bin(X, B), Opts)
            )}
         || X <- lists:seq(0, Size) ],
       [ {Prefix ++ "chop by " ++ integer_to_list(X),
          ?_assertEqual(
             Rs,
             element(2, ecsv:parse_stream(
                          ecsv:block_chopper(X), B, ecsv:accumulator(), [], Opts))
            )}
         || X <- lists:seq(1, Size) ]
     ]
     || {NLS, B} <- [{"unix nl ", Bin}, {"dos crnl ",
                                         binary:replace(Bin, <<"\n">>, <<"\r\n">>)}],
        {FS, Opts} <- [{[],[]}, {"strict ", [strict]}],
        Prefix <- [NLS ++ FS]].

parse_libcsv_rules_test_() ->
    [ ?_assertEqual(ecsv:parse(<<"\"abc\", \"def\"">>),
                    ecsv:parse(<<"abc ,  def">>))
    , ?_assertEqual(ecsv:parse(<<"\"abc\", \"def\", \"\"">>),
                    ecsv:parse(<<"\"abc\", \"def\",">>))
    ].

parse_libcsv_examples_test_() ->
    [ ?_assertEqual([{<<"abc\"">>}], ecsv:parse(<<"\"abc\"\"\"">>)) ].

parse_malformed_test_() ->
    [ ?_assertEqual(ecsv:parse(<<"\"a\"\"c\", \"d\"\"f\"">>),
                    ecsv:parse(<<"a\"c, \"d\"f\"">>))
    , ?_assertEqual(
         ecsv:parse(<<"\"Sally said \"\"Hello\", \"Wally said \"\"Goodbye\"\"\"\"\"">>),
         ecsv:parse(<<"\"Sally said \"Hello\", Wally said \"Goodbye\"\"">>))
    ].

parse_flags_test_() ->
    [ ?_assertEqual([{<<"a">>}, {<<"b">>}],
                    ecsv:parse(<<"a\n\nb\n\n">>))
      , ?_assertEqual([{<<"a">>}, {}, {<<"b">>}, {}],
                    ecsv:parse(<<"a\n\nb\n\n">>, [all_lines]))
      , ?_assertEqual([{<<"a">>, <<>>, <<"b">>, <<>>, <<"c">>}],
                    ecsv:parse(<<"a,,b,\"\",c">>, []))
      , ?_assertEqual([{<<"a">>, null, <<"b">>, <<>>, <<"c">>}],
                    ecsv:parse(<<"a,,b,\"\",c">>, [null]))
      , ?_assertEqual([{<<"'a">>, <<",';'b,\",''c';">>}],
                    ecsv:parse(<<"'a,\",';'b,\"\",''c';\"">>, [strict]))
      , ?_assertEqual([{<<"a,\",">>, <<"b,\"\",'c">>, <<"\"">>}],
                    ecsv:parse(<<"'a,\",';'b,\"\",''c';\"">>,
                               [strict, {delimiter, $;}, {quote, $'}]))
    ].

parse_file(FileName, Callback, CBState, Opts) ->
    {ok, FH} = file:open(FileName, [read, raw, binary]),
    try ecsv:parse_stream(ecsv:file_reader(), FH, Callback, CBState, Opts) of
        {_, Result, _} -> Result
    after file:close(FH)
    end.

parse_file_test_() ->
    FileName = "test/FL_insurance_sample.csv",
    Expect1 = {18018, 1001},
    Expect2 = {18018, 1002},
    Counter = fun({_, Rs}, {Fs, Ls}) ->
                      {Fs + lists:sum([tuple_size(X) || X <- Rs]),
                       Ls + length(Rs)}
              end,
    [ { string:join([atom_to_list(A) || A <- Fs], " "),
        ?_assertEqual(Expect, parse_file(FileName, Counter, {0, 0}, Fs)) }
      || {Expect, Fs} <- [{Expect1, []}, {Expect1, [strict]},
                          {Expect2, [all_lines]},
                          {Expect2, [strict, strict_finish, all_lines, null]}]
    ].
