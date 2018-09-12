-module(message_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2]).
-export([signal_test/1,
         call_test/1,
         reply_test/1,
         arg_int_test/1,
         arg_double_test/1,
         arg_bool_test/1,
         arg_struct_test/1,
         arg_map_test/1,
         arg_array_int_test/1,
         arg_array_bin_test/1,
         arg_array_string_test/1,
         arg_string_test/1]).

all() ->
    [ signal_test,
      call_test,
      reply_test,
      arg_int_test,
      arg_double_test,
      arg_bool_test,
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
    0 = ebus_message:serial(M),

    lists:foreach(fun({Path, IFace, Member}) ->
                          ?assertMatch({'EXIT', {badarg, _}},
                                       (catch ebus_message:new_signal(Path, IFace, Member)))
                  end,
                  [
                   {"badpath", "test.signal.Type", "Test"},
                   {"/test/signal/Object", "bad_if", "Test"},
                   {"/test/signal/Object", "test.signal.Type", "Bad.Name"}
                  ]),
    ok.

call_test(_Config) ->
    {ok, M} = ebus_message:new_call("test.call.dest", "/test/call/Object", "test.call.Type", "Test"),
    ?assertEqual("test.call.dest", ebus_message:destination(M)),
    ?assertEqual("/test/call/Object", ebus_message:path(M)),
    ?assertEqual("test.call.Type", ebus_message:interface(M)),
    ?assertEqual("Test", ebus_message:member(M)),
    ?assertEqual(0, ebus_message:serial(M)),

    {ok, M2} = ebus_message:new_call(undefined, "/test/call/Object", undefined, "Test"),
    ?assertEqual(undefined, ebus_message:destination(M2)),
    ?assertEqual(undefined, ebus_message:interface(M2)),

    lists:foreach(fun({Dest, Path, IFace, Member}) ->
                          ?assertMatch({'EXIT', {badarg, _}},
                                       (catch ebus_message:new_call(Dest, Path, IFace, Member)))
                  end,
                  [
                   {"baddest", "/test/call/Object", "test.call.Type", "Test"},
                   {"test.call.dest", "badpath", "test.call.Type", "Test"},
                   {"test.call.dest", "/test/call/Object", "bad_if", "Test"},
                   {"test.call.dest", "/test/call/Object", "test.call.Type", "Bad.Name"}
                  ]),
    ok.

reply_test(_Config) ->
    {ok, M} = ebus_message:new_call("test.call.dest", "/test/call/Object", "test.call.Type", "Test"),
    ok = ebus_message:serial(M, 1),
    {ok, R} = ebus_message:new_reply(M),

    ?assertEqual(1, ebus_message:reply_serial(R)),
    ?assertEqual(undefined, ebus_message:interface(R)),
    ?assertEqual(undefined, ebus_message:path(R)),
    ?assertEqual(undefined, ebus_message:member(R)),
    ?assertEqual(call, ebus_message:type(M)),
    ?assertEqual(reply, ebus_message:type(R)),
    ?assertEqual(ok, ebus_message:error(R)),

    ok.

arg_int_test(Config) ->
    M = proplists:get_value(message, Config),

    %% basic ints
    Arg = [42, -43, 43, -44, 44, -45, 45],
    ?assertEqual(ok, ebus_message:append_args(M, [byte, int16, uint16, int32, uint32, int64, uint64], Arg)),
    ?assertEqual({ok, Arg}, ebus_message:args(M)),

    %% bounds checking
    lists:foreach(fun({Types, Args}) ->
                          ?assertMatch({'EXIT', {badarg, _}},
                                       (catch ebus_message:append_args(M, Types, Args)))
                  end,
                  [
                   {[byte], [500]},
                   {[int16], [32768]},
                   {[int16], [-32769]},
                   {[uint16], [-1]},
                   {[uint16], [65536]},

                   {[int32], [2147483648]},
                   {[int32], [-2147483649]},
                   {[uint32], [-1]},
                   {[uint32], [4294967296]},

                   {[uint64], [-1]}
                   ]),
    ok.

arg_bool_test(Config) ->
    M = proplists:get_value(message, Config),

    Arg = [true, false],
    ?assertEqual(ok, ebus_message:append_args(M, [bool, bool], Arg)),
    ?assertEqual({ok, Arg}, ebus_message:args(M)),

    ?assertMatch({'EXIT', {badarg, _}}, (catch ebus_message:append_args(M, [bool], [-1]))),

    ok.

arg_double_test(Config) ->
    M = proplists:get_value(message, Config),

    Arg = [22.23, -100.0],
    ?assertEqual(ok, ebus_message:append_args(M, [double, double], Arg)),
    ?assertEqual({ok, Arg}, ebus_message:args(M)),

    ?assertMatch({'EXIT', {badarg, _}}, (catch ebus_message:append_args(M, [double], ["break"]))),

    ok.


arg_string_test(Config) ->
    M = proplists:get_value(message, Config),

    Arg = ["hello"],
    ok = ebus_message:append_args(M, [string], Arg),
    {ok, Arg} = ebus_message:args(M),

    ?assertMatch({'EXIT', {badarg, _}}, (catch ebus_message:append_args(M, [string], [-1]))),

    ok.

arg_struct_test(Config) ->
    M = proplists:get_value(message, Config),

    Arg = [{401, "hello", <<"world">>}],
    ?assertEqual(ok, ebus_message:append_args(M, [{struct, [int16, string, {array, byte}]}], Arg)),
    ?assertEqual({ok, Arg}, ebus_message:args(M)),

    ok.

arg_map_test(Config) ->
    M = proplists:get_value(message, Config),

    Arg = [#{"hello" => {42, <<"world">>}}],
    ?assertEqual(ok, ebus_message:append_args(M, [{dict, string, {struct, [int16, {array, byte}]}}], Arg)),
    ?assertEqual({ok, Arg}, ebus_message:args(M)),

    ?assertMatch({'EXIT', {badarg, _}},
                 (catch ebus_message:append_args(M, [{dict, string, string}], [#{42 => "hello"}]))),
    ?assertMatch({'EXIT', {badarg, _}},
                 (catch ebus_message:append_args(M, [{dict, string, string}], [#{"hello" => <<"hello">>}]))),

    ok.

arg_array_int_test(Config) ->
    M = proplists:get_value(message, Config),

    IntArgs = [[1, 2, 3, 4]],
    ?assertEqual(ok, ebus_message:append_args(M, [{array, int16}], IntArgs)),
    ?assertEqual({ok, IntArgs}, ebus_message:args(M)),

    ok.

arg_array_bin_test(Config) ->
    M = proplists:get_value(message, Config),

    BinArgs = [<<"hello">>],
    ?assertEqual(ok, ebus_message:append_args(M, [{array, byte}], BinArgs)),
    ?assertEqual({ok, BinArgs}, ebus_message:args(M)),

    ok.


arg_array_string_test(Config) ->
    M = proplists:get_value(message, Config),

    ArrArgs =  [["hello", "world"]],
    ?assertEqual(ok, ebus_message:append_args(M, [{array, string}], ArrArgs)),
    ?assertEqual({ok, ArrArgs}, ebus_message:args(M)),

    ok.
