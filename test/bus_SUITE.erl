-module(bus_SUITE).

-export([all/0, init_per_testcase/2]).
-export([bad_bus_test/1, name_test/1, match_test/1]).

all() ->
    [ bad_bus_test,
      name_test,
      match_test
    ].

init_per_testcase(bus_test, Config) ->
    Config;
init_per_testcase(_, Config) ->
    {ok, B} = ebus:start_link(),
    unlink(B),
    [{bus, B} | Config].


bad_bus_test(_Config) ->
    {'EXIT', {badarg, _}}= (catch ebus:start_link(noway)),

    ok.

name_test(Config) ->
    B = proplists:get_value(bus, Config),

    ok = ebus:request_name(B, "com.helium.test", [{replace_existing, true}]),

    ok = ebus:release_name(B, "com.helium.test"),
    {error, not_found} = ebus:release_name(B, "com.helium.notthere"),

    ok.

match_test(Config) ->
    B = proplists:get_value(bus, Config),

    ok = ebus:add_match(B, "type=signal, interface='test.signal.Type'"),

    {error, _} = ebus:add_match(B, "type=notthere"),

    ok.
