-module(ebus_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([system/0, session/0, starter/0]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 5}, []}}.

system() ->
    bus(ebus_system, system).


session() ->
    bus(ebus_session, session).

starter() ->
    bus(ebus_starter, starter).

bus(Name, Type) ->
    ChildSpec = #{ id => {local, Name},
                   start => {ebus, start_link, [Type]},
                   restart => temporary,
                   shutdown => 1000,
                   type => worker
                 },
    case supervisor:start_child(?MODULE, ChildSpec) of
        {error, {already_started, Pid}} -> {ok, Pid};
        {ok, Pid} -> {ok, Pid}
    end.
