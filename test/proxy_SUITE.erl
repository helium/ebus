-module(proxy_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([bus_test/1, call_test/1, call_async_test/1, signal_test/1]).

all() ->
    [ bus_test,
      call_test,
      call_async_test,
      signal_test
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

bus_test(Config) ->
    B = ?config(bus, Config),
    Dest = "com.helium.test",
    {ok, Proxy} = ebus_proxy:start(B, Dest, []),

    ?assertEqual(B, ebus_proxy:bus(Proxy)),

    ok.

call_test(Config) ->
    B = ?config(bus, Config),

    %% Mock up a service
    Dest = "com.helium.test",
    ?assertEqual(ok, ebus:request_name(B, Dest, [{replace_existing, true}])),

    Path = "/",
    ?assertEqual(ok, ebus:add_match(B, #{path => Path})),

    meck:new(call_test, [non_strict]),
    meck:expect(call_test, init,
                fun([init_arg]) -> {ok, init_state};
                   (A) -> erlang:error({bad_init, A})
                end),
    meck:expect(call_test, handle_message,
               fun("Echo", Msg, State) ->
                       ?assertEqual(Path, ebus_message:path(Msg)),
                       {ok, [Str]} = ebus_message:args(Msg),
                       {reply, [string], [Str], State};
                  ("True", _Msg, State) ->
                       {reply, [bool], [true], State};
                  ("Timeout", _Msg, State) ->
                       {noreply, State}
               end),

    {ok, Obj} = ebus_object:start(B, Path, call_test, [init_arg], []),
    %% Validate that init was called with the start arguments
    ?assert(meck:called(call_test, init, [[init_arg]])),

    %% Now call the Echo service
    {ok, Proxy} = ebus_proxy:start(B, Dest, []),
    Args = ["Hello World"],
    ?assertEqual({ok, Args}, ebus_proxy:call(Proxy, Path, "Echo", [string], Args)),
    ?assertEqual({ok, [true]}, ebus_proxy:call(Proxy, "True")),
    ?assertEqual({ok, [true]}, ebus_proxy:call(Proxy, Path, "True")),
    ?assertEqual({error, timeout}, ebus_proxy:call(Proxy, Path, "Timeout", [], [], 1000)),

    ?assertEqual(ok, ebus_object:stop(Obj, normal)),

    meck:validate(call_test),
    meck:unload(call_test),

    ok.

call_async_test(Config) ->
    B = ?config(bus, Config),

    %% Mock up a service
    Dest = "com.helium.async_test",
    ?assertEqual(ok, ebus:request_name(B, Dest, [{replace_existing, true}])),

    Path = "/",
    ?assertEqual(ok, ebus:add_match(B, #{path => Path})),

    meck:new(call_async_test, [non_strict]),
    meck:expect(call_async_test, init,
                fun([init_arg]) -> {ok, init_state};
                   (A) -> erlang:error({bad_init, A})
                end),
    meck:expect(call_async_test, handle_message,
               fun("Echo", Msg, State) ->
                       ?assertEqual(Path, ebus_message:path(Msg)),
                       {ok, [Str]} = ebus_message:args(Msg),
                       {reply, [string], [Str], State};
                  ("True", _Msg, State) ->
                       {reply, [bool], [true], State};
                  ("Timeout", _Msg, State) ->
                       {noreply, State}
               end),

    {ok, Obj} = ebus_object:start(B, Path, call_async_test, [init_arg], []),
    %% Validate that init was called with the start arguments
    ?assert(meck:called(call_async_test, init, [[init_arg]])),

    %% Now call the Echo service
    {ok, Proxy} = ebus_proxy:start(B, Dest, []),
    Args = ["Hello World"],
    ebus_proxy:call_async(Proxy, {self(), echo_result}, Path, "Echo", [string], Args),
    receive
        {echo_result, {ok, Args}} -> ok;
        Other -> ct:pal("~p", [Other])
    after 5000 -> erlang:exit(timeout_call_async_echo)
    end,

    ebus_proxy:call_async(Proxy, {self(), no_path_true_result}, "True"),
    receive
        {no_path_true_result, {ok, [true]}} -> ok
    after 5000 -> erlang:exit(timeout_call_async_no_path_true)
    end,

    ebus_proxy:call_async(Proxy, {self(), path_true_result}, Path, "True"),
    receive
        {path_true_result, {ok, [true]}} -> ok
    after 5000 -> erlang:exit(timeout_call_async_path_true)
    end,

    ebus_proxy:call_async(Proxy, {self(), timeout_result}, Path, "Timeout", [], [], 1000),
    receive
        {timeout_result, {error, timeout}} -> ok
    after 5000 -> erlang:exit(timeout_call_async_timeout)
    end,

    ?assertEqual(ok, ebus_object:stop(Obj, normal)),

    meck:validate(call_async_test),
    meck:unload(call_async_test),

    ok.

signal_test(Config) ->
    B = ?config(bus, Config),

    Dest = "com.helium.test",
    {ok, Proxy} = ebus_proxy:start(B, Dest, []),
    %% Check that signal handlers expect an interface
    ?assertEqual({error, no_interface},
                 ebus_proxy:add_signal_handler(Proxy, "Signal", self(), no_matter)),
    %% Add a couple of signal handlers
    {ok, SignalID} = ebus_proxy:add_signal_handler(Proxy, "com.helium.test.Signal",
                                                   self(), signal_info),
    %% Register for the same signal again to ensure multiple registrations work
    {ok, SignalID2} = ebus_proxy:add_signal_handler(Proxy, "com.helium.test.Signal",
                                                    self(), signal_info_2),
    {ok, Signal} = ebus_message:new_signal("/", "com.helium.test.Signal"),

    ok = ebus:send(B, Signal),
    %% Make sure we receive both signals
    receive
        {ebus_signal, signal_info, SignalID, Msg} ->
            ?assertEqual("Signal", ebus_message:member(Msg)),
            ?assertEqual("/", ebus_message:path(Msg))
    after 5000 -> erlang:exit(timeout_signal)
    end,

    receive
        {ebus_signal, signal_info_2, SignalID2, _} -> ok
    after 5000 -> erlang:exit(timeout_signal_2)
    end,

    %% Remove one signal handler
    ebus_proxy:remove_signal_handler(Proxy, SignalID, self(), signal_info),

    ok = ebus:send(B, Signal),
    %% Ensure that we don't receive it
    receive
        %% We should not receive this after removing the AddSignalID
        {ebus_signal, signal_info, SignalID, _} ->
            ?assert(received_post_remove_signal)
    after 750 -> ok
    end,

    %% But that we do receive the seond handler
    receive
        {ebus_signal, signal_info_2, SignalID2, _} -> ok
    after 5000 -> erlang:exit(timeout_post_remove_signal_2)
    end,

    %% Remove the second handler
    ebus_proxy:remove_signal_handler(Proxy, SignalID2, self(), signal_info_2),
    ok = ebus:send(B, Signal),
    %% Ensure that we don't receive the second handler
    receive
        {ebus_signal, signal_info_2, SignalID2, _} ->
            ?assert(false, received_post_remove_signal_2)
    after 750 -> ok
    end,

    ok.
