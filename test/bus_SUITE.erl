-module(bus_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([bad_bus_test/1, name_test/1, match_test/1, send_test/1]).

all() ->
    [ bad_bus_test,
      name_test,
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
    {'EXIT', {badarg, _}}= (catch ebus:start_link(noway)),

    ok.

name_test(Config) ->
    B = ?config(bus, Config),

    ok = ebus:request_name(B, "com.helium.test", [{replace_existing, true}]),

    ok = ebus:release_name(B, "com.helium.test"),
    {error, not_found} = ebus:release_name(B, "com.helium.notthere"),

    ok.

match_test(Config) ->
    B = ?config(bus, Config),

    ok = ebus:add_match(B, "type=signal, interface='test.signal.Type'"),
    {error, _} = ebus:add_match(B, "type=notthere"),

    ok.

send_test(Config) ->
    B = ?config(bus, Config),

    %% ok = ebus:request_name(B, "com.helium.test", [{replace_existing, true}]),

    ok = ebus:add_match(B, "type=signal, interface='test.signal.Type'"),
    {ok, _Filter} = ebus:add_filter(B, self(),
                                    #{interface => "test.signal.Type"
                                     }),

    {ok, M} = ebus_message:new_signal("/test/signal/Object", "test.signal.Type", "Test"),
    ok = ebus:send(B, M),

    _Msg = receive
               {filter_match, M} -> M
           after 5000 -> error(timeout)
           end,
    ok.
