-module(bus_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([bad_bus_test/1,
         name_test/1,
         unique_name_test/1,
         match_test/1,
         send_test/1]).

all() ->
    [ bad_bus_test,
      name_test,
      unique_name_test,
      match_test,
      send_test
    ].

init_per_testcase(bad_bus_test, Config) ->
    Config;
init_per_testcase(_, Config) ->
    {ok, B} = ebus:start_link(),
    unlink(B),
    [{bus, B} | Config].

end_per_testcase(_, Config) ->
    case proplists:get_value(bus, Config, undefined) of
        undefined ->
            ok;
        B -> exit(B, normal)
    end.

bad_bus_test(_Config) ->
    ?assertError(badarg, ebus:start_link(noway)),

    ok.

unique_name_test(Config) ->
    B = ?config(bus, Config),

    ?assert(is_list(ebus:unique_name(B))),
    ok.

name_test(Config) ->
    B = ?config(bus, Config),

    ?assertEqual(ok,
                ebus:request_name(B, "com.helium.test", [{replace_existing, true}])),

    ?assertEqual(ok, ebus:release_name(B, "com.helium.test")),
    ?assertEqual({error, not_found}, ebus:release_name(B, "com.helium.notthere")),

    ok.

match_test(Config) ->
    B = ?config(bus, Config),

    ?assertEqual(ok, ebus:add_match(B, "type=signal, interface='test.signal.Type'")),
    ?assertEqual({error, invalid}, ebus:add_match(B, "type=notthere")),

    ok.

send_test(Config) ->
    B = ?config(bus, Config),

    ok = ebus:add_match(B, "type=signal"),
    {ok, Filter} = ebus:add_filter(B, self(),
                                   #{
                                     path => "/test/signal/Object"
                                    }),

    {ok, M} = ebus_message:new_signal("/test/signal/Object", "test.signal.Type", "Test"),
    Args = [#{"lat" => 48.858845065,
              "lon" => 2.352656618,
              "acc" => 22.1}],
    ebus_message:append_args(M, [{dict, string, double}], Args),
    ok = ebus:send(B, M),

    Msg = receive
              {filter_match, M2} -> M2
          after 5000 -> erlang:exit(timeout_filter)
          end,

    ?assertEqual("test.signal.Type", ebus_message:interface(Msg)),
    ?assertEqual("Test", ebus_message:member(Msg)),
    ?assertEqual("/test/signal/Object", ebus_message:path(Msg)),
    ?assertEqual({ok, Args}, ebus_message:args(Msg)),

    ok = ebus:remove_filter(B, Filter),

    ok.
