-module(message_SUITE).

-export([all/0, init_per_testcase/2]).
-export([signal_test/1,
         call_test/1,
         arg_int_test/1,
         arg_array_test/1,
         arg_string_test/1]).

-include_lib("dbus.hrl").

all() ->
    [ signal_test,
      call_test,
      arg_int_test,
      arg_string_test,
      arg_array_test
    ].

init_message(Config) ->
    {ok, M} = dbus:message_new_call("test.call.dest", "/test/call/Object", "test.call.Type", "Test"),
    [{message, M} | Config].

init_per_testcase(signal_test, Config) ->
    Config;
init_per_testcase(call_test, Config) ->
    Config;
init_per_testcase(_, Config) ->
    init_message(Config).



signal_test(_Config) ->
    {ok, _} = dbus:message_new_signal("/test/signal/Object", "test.signal.Type", "Test"),

    {'EXIT', {badarg, _}} = (catch dbus:message_new_signal("badpath", "test.signal.Type", "Test")),
    {'EXIT', {badarg, _}} = (catch dbus:message_new_signal("/test/signal/Object", "bad_if", "Test")),
    {'EXIT', {badarg, _}} = (catch dbus:message_new_signal("/test/signal/Object", "test.signal.Type", "Bad.Name")),

    ok.

call_test(_Config) ->
    {ok, _} = dbus:message_new_call("test.call.dest", "/test/call/Object", "test.call.Type", "Test"),
    %% {ok, _} = dbus:message_new_call("test.call.dest", "/test/call/Object", undefined, "Test"),

    {'EXIT', {badarg, _}} = (catch dbus:message_new_call("baddest", "/test/call/Object", "test.call.Type", "Test")),
    {'EXIT', {badarg, _}} = (catch dbus:message_new_call("test.call.dest", "badpath", "test.call.Type", "Test")),
    {'EXIT', {badarg, _}} = (catch dbus:message_new_call("test.call.dest", "/test/call/Object", "bad_if", "Test")),
    {'EXIT', {badarg, _}} = (catch dbus:message_new_call("test.call.dest", "/test/call/Object", "test.call.Type", "Bad.Name")),

    ok.


arg_int_test(Config) ->
    M = proplists:get_value(message, Config),

    %% basic ints
    ok = dbus_message:append_args(M, [byte, int16, uint16, int32, uint32, int64, uint64],
                                  [42, -42, 42, -42, 42, -42, 42]),

    %% bounds checking
    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [byte], [500])),
    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [int16], [32768])),
    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [int16], [-32769])),
    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [uint16], [-1])),
    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [uint16], [65536])),

    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [int32], [2147483648])),
    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [int32], [-2147483649])),
    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [uint32], [-1])),
    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [uint32], [4294967296])),

    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [uint64], [-1])),

    ok.

arg_string_test(Config) ->
    M = proplists:get_value(message, Config),

    ok = dbus_message:append_args(M, [string], ["hello"]),

    {'EXIT', {badarg, _}} = (catch dbus_message:append_args(M, [string], [-1])),

    ok.

arg_array_test(Config) ->
    M = proplists:get_value(message, Config),

    ok = dbus_message:append_args(M, [{array, int16}], [[1, 2, 3, 4]]),
    ok = dbus_message:append_args(M, [{array, byte}], [<<"hello">>]),

    ok = dbus_message:append_args(M, [{array, byte}], [<<"hello">>]),

    ok = dbus_message:append_args(M, [{array, string}], [["hello", "world"]]),

    ok.
