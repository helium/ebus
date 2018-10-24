-module(bus_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
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

init_per_suite(Config) ->
    application:ensure_all_started(ebus),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(bad_bus_test, Config) ->
    Config;
init_per_testcase(_, Config) ->
    {ok, B} = ebus:starter(),
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

    ?assertEqual(ok, ebus:add_match(B, #{type => signal, interface => "test.signal.Type"})),
    ?assertEqual({error, invalid}, ebus:add_match(B, #{type => notthere})),

    ok.

send_test(Config) ->
    B = ?config(bus, Config),

    Path = "/test/signal/Object",
    Interface = "test.signal.Interface",
    AddRule = #{ type => signal,
                 path => Path,
                 interface => Interface,
                 member => "Add"
               },
    RemoveRule = #{ type => signal,
                    path => Path,
                    interface => Interface,
                    member => "Remove"
                  },
    ok = ebus:add_match(B, AddRule),
    {ok, AddFilter} = ebus:add_filter(B, self(), AddRule),

    ok = ebus:add_match(B, RemoveRule),
    {ok, RemoveFilter} = ebus:add_filter(B, self(), RemoveRule),

    {ok, AddSignal} = ebus_message:new_signal(Path, Interface, "Add"),
    Args = [#{"lat" => 48.858845065,
              "lon" => 2.352656618,
              "acc" => 22.1}],
    ebus_message:append_args(AddSignal, [{dict, string, double}], Args),
    ok = ebus:send(B, AddSignal),

    receive
        {filter_match, AddFilter, Msg} ->
            ?assertEqual(Interface, ebus_message:interface(Msg)),
            ?assertEqual("Add", ebus_message:member(Msg)),
            ?assertEqual(Path, ebus_message:path(Msg)),
            ?assertEqual({ok, Args}, ebus_message:args(Msg));
        {filter_match, RemoveFilter, Msg} ->
            %% We should not reach here since we never send a Remove
            %% signal, but if we do it's likely we mismatched the
            %% filter so one of the following will fail.
            ?assertEqual(Interface, ebus_message:interface(Msg)),
            ?assertEqual("Remove", ebus_message:member(Msg)),
            ?assertEqual(Path, ebus_message:path(Msg))
    after 5000 -> erlang:exit(timeout_filter)
    end,

    ok = ebus:remove_filter(B, AddFilter),
    ok = ebus:remove_filter(B, RemoveFilter),

    ok.
