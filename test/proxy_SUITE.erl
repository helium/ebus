-module(proxy_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([call_test/1]).

all() ->
    [ call_test
    ].

init_per_testcase(_, Config) ->
    {ok, B} = ebus:start(),
    [{bus, B} | Config].

end_per_testcase(_, Config) ->
    case proplists:get_value(bus, Config, undefined) of
        undefined ->
            ok;
        B -> exit(B, normal)
    end.

call_test(Config) ->
    B = ?config(bus, Config),

    %% Mock up a service
    Dest = "com.helium.test",
    ?assertEqual(ok, ebus:request_name(B, Dest, [{replace_existing, true}])),

    Path = "/com/helium/test/Object",
    ebus:add_match(B, lists:flatten(io_lib:format("path=~s", [Path]))),

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

    {ok, _Obj} = ebus_object:start(B, Path, call_test, [init_arg], []),
    %% Validate that init was called with the start arguments
    ?assert(meck:called(call_test, init, [[init_arg]])),

    %% Now call the Echo service
    {ok, Proxy} = ebus_proxy:start(B, Dest, Path, []),
    Args = ["Hello World"],
    ?assertEqual({ok, Args}, ebus_proxy:call(Proxy, "Echo", [string], Args)),
    ?assertEqual({ok, [true]}, ebus_proxy:call(Proxy, "True")),
    ?assertEqual({error, timeout}, ebus_proxy:call(Proxy, "Timeout", [], [], 1000)),

    meck:validate(call_test),
    meck:unload(call_test),

    ok.
