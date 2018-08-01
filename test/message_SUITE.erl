-module(message_SUITE).

-export([all/0, init_per_testcase/2]).
-export([signal_test/1]).

-include_lib("dbus.hrl").

all() ->
    [ signal_test
    ].

init_per_testcase(signal_test, Config) ->
    Config;
init_per_testcase(_, Config) ->
    {ok, B} = dbus:bus(),
    [{bus, B} | Config].


signal_test(_Config) ->
    {ok, _} = dbus:message_new_signal("/test/signal/Object", "test.signal.Type", "Test"),

    {'EXIT', {badarg, _}} = (catch dbus:message_new_signal("badpath", "test.signal.Type", "Test")),
    {'EXIT', {badarg, _}} = (catch dbus:message_new_signal("/test/signal/Object", "bad_if", "Test")),
    {'EXIT', {badarg, _}} = (catch dbus:message_new_signal("/test/signal/Object", "test.signal.Type", "Bad.Name")),

    ok.
