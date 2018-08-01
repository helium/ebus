-module(bus_SUITE).

-export([all/0, init_per_testcase/2]).
-export([bus_test/1, name_test/1, match_test/1]).

-include_lib("dbus.hrl").

all() ->
    [ bus_test,
      name_test,
      match_test
    ].

init_per_testcase(bus_test, Config) ->
    Config;
init_per_testcase(_, Config) ->
    {ok, B} = dbus:bus(),
    [{bus, B} | Config].


bus_test(_Config) ->
    {ok, _} = dbus:bus(),
    {ok, _} = dbus:bus(?BUS_SESSION),
    {ok, _} = dbus:bus(?BUS_STARTER),

    {'EXIT', {badarg, _}}= (catch dbus:bus(22)),

    ok.

name_test(Config) ->
    B = proplists:get_value(bus, Config),

    {ok, ?BUS_REQUEST_NAME_REPLY_PRIMARY_OWNER} =
        dbus:request_name(B, "com.helium.test", ?BUS_NAME_FLAG_REPLACE_EXISTING),

    {ok, ?BUS_RELEASE_NAME_REPLY_RELEASED} = dbus:release_name(B, "com.helium.test"),
    {ok, ?BUS_RELEASE_NAME_REPLY_NON_EXISTENT} = dbus:release_name(B, "com.helium.notthere"),

    ok.

match_test(Config) ->
    B = proplists:get_value(bus, Config),

    ok = dbus:add_match(B, "type=signal, interface='test.signal.Type'"),

    {error, _} = dbus:add_match(B, "type=notthere"),

    ok.
