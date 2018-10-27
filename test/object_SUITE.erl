-module(object_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([call_test/1, cast_test/1, info_test/1, message_test/1]).

all() ->
    [ call_test,
      cast_test,
      info_test,
      message_test
    ].

init_per_suite(Config) ->
    application:ensure_all_started(ebus),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    {ok, B} = ebus:starter(),
    [{bus, B} | Config].

end_per_testcase(_, Config) ->
    case proplists:get_value(bus, Config, undefined) of
        undefined ->
            ok;
        B -> exit(B, normal)
    end.

call_test(Config) ->
    B = ?config(bus, Config),

    meck:new(call_test, [non_strict]),
    meck:expect(call_test, init, fun([init_arg]) -> {ok, init_state};
                                    (A) -> erlang:error({bad_init, A})
                                 end),
    meck:expect(call_test, handle_call,
                fun(_, _, State) when State /= init_state ->
                        erlang:error({bad_state, State});
                   ({reply, R}, _, State) ->
                        {reply, R, State};
                   ({reply, R, A}, _, State) ->
                        {reply, R, State, A};
                   ({noreply, R}, From, State) ->
                        spawn(fun() ->
                                      gen_server:reply(From, R)
                              end),
                        {noreply, State};
                   ({noreply, R, A}, From, State) ->
                        spawn(fun() ->
                                      gen_server:reply(From, R)
                              end),
                        {noreply, State, A};
                   ({stop, Reason, R}, _, State) ->
                        {stop, Reason, R, State}
                end),
    meck:expect(call_test, handle_info,
               fun({filter_match, _, Msg}, State) ->
                       ?assertEqual("com.helium.test.Call", ebus_message:interface(Msg)),
                       ?assertEqual("Notice", ebus_message:member(Msg)),
                       {noreply, State}
               end),
    meck:expect(call_test, handle_continue,
               fun(test_continue, State) ->
                       {noreply, State}
               end),
    meck:expect(call_test, terminate,
                fun(stop_reason, _State) -> ok end),

    Path = "/com/helium/test/Call",
    {ok, O} = ebus_object:start(B, Path, call_test, [init_arg], []),
    %% Validate that init was called with the start arguments
    ?assert(meck:called(call_test, init, [[init_arg]])),
    %% Check reply
    ?assertEqual(test_reply, gen_server:call(O, {reply, test_reply})),
    %% Check noreply
    ?assertEqual(later_reply, gen_server:call(O, {noreply, later_reply})),

    ?assertEqual(ok, ebus:add_match(B, #{type => signal})),

    SignalAction = {signal, "com.helium.test.Call", "Notice"},
    {ok, F} = ebus:add_filter(B, O, #{ path => Path }),

    %% Test reply result action
    ?assertEqual(ok, gen_server:call(O, {reply, ok, SignalAction})),
    meck:wait(1, call_test, handle_info, '_', 1000),

    %% Test noreply result action
    ?assertEqual(ok, gen_server:call(O, {noreply, ok, SignalAction})),
    meck:wait(2, call_test, handle_info, '_', 1000),

    %% Test reply continue action
    ?assertEqual(ok, gen_server:call(O, {reply, ok, {continue, test_continue}})),
    meck:wait(1, call_test, handle_continue, '_', 1000),

    %% Test noreply continue action
    ?assertEqual(ok, gen_server:call(O, {noreply, ok, {continue, test_continue}})),
    meck:wait(2, call_test, handle_continue, '_', 1000),

    ebus:remove_filter(B, F),

    %% Check stop
    ?assertEqual(test_stop, gen_server:call(O, {stop, stop_reason, test_stop})),

    meck:validate(call_test),
    meck:unload(call_test),

    ok.

cast_test(Config) ->
    B = ?config(bus, Config),

    meck:new(cast_test, [non_strict]),
    meck:expect(cast_test, init,
                fun([init_arg]) ->
                        {ok, init_state};
                   (A) ->
                        erlang:error({bad_init, A})
                end),
    meck:expect(cast_test, handle_cast,
                fun(_, State) when State /= init_state ->
                        erlang:error({bad_state, State});
                   (noreply, State) ->
                        {noreply, State};
                   ({noreply, A}, State) ->
                        {noreply, State, A};
                   ({stop, Reason}, State) ->
                        {stop, Reason, State};
                   (M, _) ->
                        erlang:error({unhandled, M})
                end),
    meck:expect(cast_test, handle_info,
               fun({filter_match, _, Msg}, State) ->
                       ?assertEqual("com.helium.test.Cast", ebus_message:interface(Msg)),
                       ?assertEqual("Notice", ebus_message:member(Msg)),
                       {noreply, State}
               end),
    meck:expect(cast_test, handle_continue,
               fun(test_continue, State) ->
                       {noreply, State}
               end),
    meck:expect(cast_test, terminate,
                fun(stop_reason, _State) -> ok end),

    Path = "/com/helium/test/Cast",
    {ok, O} = ebus_object:start(B, Path, cast_test, [init_arg], []),
    %% Validate that init was called with the start arguments
    ?assert(meck:called(cast_test, init, [[init_arg]])),

    %% Check noreply
    gen_server:cast(O, noreply),
    meck:wait(cast_test, handle_cast, [noreply, '_'], 1000),

    ?assertEqual(ok, ebus:add_match(B, #{ type => signal})),

    SignalAction = {signal, "com.helium.test.Cast", "Notice"},
    {ok, F} = ebus:add_filter(B, O, #{ path => Path }),

    %% Test result action
    gen_server:cast(O, {noreply, SignalAction}),
    meck:wait(cast_test, handle_info, [{filter_match, '_', '_'}, '_'], 1000),

    %% Test result continue
    gen_server:cast(O, {noreply, {continue, test_continue}}),

    ebus:remove_filter(B, F),

    %% Check stop
    gen_server:cast(O, {stop, stop_reason}),

    meck:validate(cast_test),
    meck:unload(cast_test),

    ok.


info_test(Config) ->
    B = ?config(bus, Config),

    meck:new(info_test, [non_strict]),
    meck:expect(info_test, init, fun([init_arg]) -> {ok, init_state};
                                    (A) -> erlang:error({bad_init, A})
                                 end),
    meck:expect(info_test, handle_info,
                fun(_, State) when State /= init_state ->
                        erlang:error({bad_state, State});
                   (noreply, State) ->
                        {noreply, State};
                   ({noreply, A}, State) ->
                        {noreply, State, A};
                   ({stop, Reason}, State) ->
                        {stop, Reason, State};
                   ({filter_match, _, Msg}, State) ->
                        ?assertEqual("com.helium.test.Info", ebus_message:interface(Msg)),
                        ?assertEqual("Notice", ebus_message:member(Msg)),
                        {noreply, State};
                   (M, _) ->
                        erlang:error({unhandled, M})
                end),
    meck:expect(info_test, handle_continue,
                fun(test_continue, State) ->
                        {noreply, State}
                end),
    meck:expect(info_test, terminate,
                fun(stop_reason, _State) -> ok end),

    Path = "/com/helium/test/Info",
    {ok, O} = ebus_object:start(B, Path, info_test, [init_arg], []),
    %% Validate that init was called with the start arguments
    ?assert(meck:called(info_test, init, [[init_arg]])),

    %% Check noreply
    erlang:send(O, noreply),
    meck:wait(info_test, handle_info, [noreply, '_'], 1000),

    ?assertEqual(ok, ebus:add_match(B, #{type => signal})),

    SignalAction = {signal, "com.helium.test.Info", "Notice"},
    {ok, F} = ebus:add_filter(B, O, #{ path => Path }),

    %% Test result action
    erlang:send(O, {noreply, SignalAction}),
    meck:wait(info_test, handle_info, [{filter_match, '_', '_'}, '_'], 1000),

    %% Test continue action
    erlang:send(O, {noreply, {continue, test_continue}}),

    ebus:remove_filter(B, F),

    %% Check stop
    erlang:send(O, {stop, stop_reason}),
    meck:wait(info_test, handle_info, [{stop, '_'}, '_'], 1000),
    meck:wait(info_test, terminate, '_', 1000),

    meck:validate(info_test),
    meck:unload(info_test),

    ok.

message_test(Config) ->
    B = ?config(bus, Config),

    Dest = "com.helium.test",
    ?assertEqual(ok, ebus:request_name(B, Dest, [{replace_existing, true}])),

    Path = "/com/helium/test/Message",
    ?assertEqual(ok, ebus:add_match(B, #{path => Path})),

    meck:new(message_test, [non_strict]),
    meck:expect(message_test, init,
                fun([init_arg]) -> {ok, init_state};
                   (A) -> erlang:error({bad_init, A})
                end),
    meck:expect(message_test, handle_message,
               fun("NoReply", Msg, State) ->
                       ?assertEqual(Path, ebus_message:path(Msg)),
                       {noreply, State};
                  ("Reply", Msg, State) ->
                       ?assertEqual(Path, ebus_message:path(Msg)),
                       {reply, [bool], [true], State};
                  ("Stop", _, State) ->
                       {stop, normal, State}
               end),
    meck:expect(message_test, terminate,
                fun(_, _State) -> ok end),

    {ok, O} = ebus_object:start(B, Path, message_test, [init_arg], []),
    %% Validate that init was called with the start arguments
    ?assert(meck:called(message_test, init, [[init_arg]])),

    {ok, NoReplyMsg} = ebus_message:new_call(Dest, Path, "NoReply"),
    ebus:send(B, NoReplyMsg),
    meck:wait(message_test, handle_message, ["NoReply", '_', '_'], 1000),

    {ok, ReplyMsg} = ebus_message:new_call(Dest, Path, "Reply"),
    ebus:send(B, ReplyMsg),
    meck:wait(message_test, handle_message, ["Reply", '_', '_'], 1000),

    {ok, StopMsg} = ebus_message:new_call(Dest, Path, "Stop"),
    ebus:send(B, StopMsg),
    meck:wait(message_test, handle_message, ["Stop", '_', '_'], 1000),
    meck:wait(message_test, terminate, '_', 1000),
    ?assert(not erlang:is_process_alive(O)),

    meck:validate(message_test),
    meck:unload(message_test),

    ok.
