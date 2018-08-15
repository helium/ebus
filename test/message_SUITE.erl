-module(message_SUITE).

-export([all/0, init_per_testcase/2]).
-export([signal_test/1,
         call_test/1,
         arg_int_test/1,
         arg_struct_test/1,
         arg_map_test/1,
         arg_array_int_test/1,
         arg_array_bin_test/1,
         arg_array_string_test/1,
         arg_string_test/1]).

all() ->
    [ signal_test,
      call_test,
      arg_int_test,
      arg_struct_test,
      arg_map_test,
      arg_string_test,
      arg_array_int_test,
      arg_array_bin_test,
      arg_array_string_test
    ].

init_message(Config) ->
    {ok, M} = ebus_message:new_call("test.call.dest", "/test/call/Object", "test.call.Type", "Test"),
    [{message, M} | Config].

init_per_testcase(signal_test, Config) ->
    Config;
init_per_testcase(call_test, Config) ->
    Config;
init_per_testcase(_, Config) ->
    init_message(Config).



signal_test(_Config) ->
    {ok, M} = ebus_message:new_signal("/test/signal/Object", "test.signal.Type", "Test"),
    0 = ebus_message:get_serial(M),

    {'EXIT', {badarg, _}} = (catch ebus_message:new_signal("badpath", "test.signal.Type", "Test")),
    {'EXIT', {badarg, _}} = (catch ebus_message:new_signal("/test/signal/Object", "bad_if", "Test")),
    {'EXIT', {badarg, _}} = (catch ebus_message:new_signal("/test/signal/Object", "test.signal.Type", "Bad.Name")),

    ok.

call_test(_Config) ->
    {ok, M} = ebus_message:new_call("test.call.dest", "/test/call/Object", "test.call.Type", "Test"),
    0 = ebus_message:get_serial(M),

    %% {ok, _} = ebus:message_new_call("test.call.dest", "/test/call/Object", undefined, "Test"),

    {'EXIT', {badarg, _}} = (catch ebus_message:new_call("baddest", "/test/call/Object", "test.call.Type", "Test")),
    {'EXIT', {badarg, _}} = (catch ebus_message:new_call("test.call.dest", "badpath", "test.call.Type", "Test")),
    {'EXIT', {badarg, _}} = (catch ebus_message:new_call("test.call.dest", "/test/call/Object", "bad_if", "Test")),
    {'EXIT', {badarg, _}} = (catch ebus_message:new_call("test.call.dest", "/test/call/Object", "test.call.Type", "Bad.Name")),

    ok.


arg_int_test(Config) ->
    M = proplists:get_value(message, Config),

    %% basic ints
    Arg = [42, -43, 43, -44, 44, -45, 45],
    ok = ebus_message:append_args(M, [byte, int16, uint16, int32, uint32, int64, uint64], Arg),
    {ok, Arg} = ebus_message:get_args(M),

    %% bounds checking
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [byte], [500])),
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [int16], [32768])),
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [int16], [-32769])),
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [uint16], [-1])),
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [uint16], [65536])),

    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [int32], [2147483648])),
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [int32], [-2147483649])),
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [uint32], [-1])),
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [uint32], [4294967296])),

    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [uint64], [-1])),

    ok.

arg_string_test(Config) ->
    M = proplists:get_value(message, Config),

    Arg = ["hello"],
    ok = ebus_message:append_args(M, [string], Arg),
    {ok, Arg} = ebus_message:get_args(M),

    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [string], [-1])),

    ok.

arg_struct_test(Config) ->
    M = proplists:get_value(message, Config),

    Arg = [{401, "hello", <<"world">>}],
    ok = ebus_message:append_args(M, [{struct, [int16, string, {array, byte}]}], Arg),
    {ok, Arg} = ebus_message:get_args(M),

    ok.

arg_map_test(Config) ->
    M = proplists:get_value(message, Config),

    Arg = [#{"hello" => {42, <<"world">>}}],
    ok = ebus_message:append_args(M, [{dict, string, {struct, [int16, {array, byte}]}}], Arg),
    {ok, Arg} = ebus_message:get_args(M),

    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [{dict, string, string}], [#{42 => "hello"}])),
    {'EXIT', {badarg, _}} = (catch ebus_message:append_args(M, [{dict, string, string}], [#{"hello" => <<"hello">>}])),

    ok.

arg_array_int_test(Config) ->
    M = proplists:get_value(message, Config),

    IntArgs = [[1, 2, 3, 4]],
    ok = ebus_message:append_args(M, [{array, int16}], IntArgs),
    {ok, IntArgs} = ebus_message:get_args(M),

    ok.

arg_array_bin_test(Config) ->
    M = proplists:get_value(message, Config),

    BinArgs = [<<"hello">>],
    ok = ebus_message:append_args(M, [{array, byte}], BinArgs),
    {ok, BinArgs} = ebus_message:get_args(M),

    ok.


arg_array_string_test(Config) ->
    M = proplists:get_value(message, Config),

    ArrArgs =  [["hello", "world"]],
    ok = ebus_message:append_args(M, [{array, string}], ArrArgs),
    {ok, ArrArgs} = ebus_message:get_args(M),

    ok.
