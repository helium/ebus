-module(ebus_proxy).

-include("ebus.hrl").

-behavior(gen_server).

%% API
-export([call/2, call/3, call/5, call/6,
        bus/1, send/2, send/3, send/5,
        add_signal_handler/4, add_signal_handler/5, remove_signal_handler/4,
        stop/1]).
-export([managed_objects/1, managed_objects/2]).

%% gen_server
-export([start/3, start_link/3, init/1,
        handle_call/3, handle_cast/2, handle_info/2]).

-record(handler_entry, {
                         path :: ebus:object_path(),
                         member :: string(),
                         rule :: ebus:rule(),
                         handlers :: [{HandlerPid::pid(), HandlerInfo::any()}]
                        }).

-record(state, {
                bus :: pid(),
                dest :: string(),
                calls=#{} :: #{Serial::non_neg_integer() => From::term()},
                signal_handlers=#{} :: #{ebus:filter_id() => #handler_entry{}},
                active_filters=#{} :: #{ebus:filter_id() => Filter::ebus:rule()}
               }).

-define(PROXY_SEND_TIMEOUT, 5000).

%% API
%%


%% @doc Call a given interface/member on the object at the root path
%% "/" of the destination the proxy was started with.
%%
%% @see call/6 for more details
-spec call(ebus:proxy(), string()) -> {ok, Response::term()} | {error, term()}.
call(Pid, Member) ->
    call(Pid, "/", Member).

%% @doc Call a given interface/member on the object at the given path
%%
%% @see call/6 for more details
call(Pid, Path, Member) ->
    call(Pid, Path, Member, [], []).


%% @doc Call a given interface/member on the object at the given path
%%
%% @see call/6 for more details
-spec call(ebus:proxy(), string(), string(), ebus:signature(), [any()])
          -> {ok, Response::term()} | {error, term()}.
call(Pid, Path, Member, Types, Args) ->
    %% Using the dbus recommended `-1` means a 25 second (!) timeout
    call(Pid, Path, Member, Types, Args, 5000).

%% @doc Call the given interface/member on the object at the given
%% path passing in arguments that are typed using the given types. The
%% given timeout indicates how long to wait for a reults.
%%
%% To make it more convenient to call a member on specific interface,
%% the member argumen can include both the interface and the
%% member. For example, "net.connman.Manager.GetServices" can be used
%% to call the "GetServices" member on the "net.connman.Manager"
%% interface of the object at the given path.
%%
%% Arguments that are passed in have to have their types
%% specified. See the ebus:signature type on all supported types. For
%% example to call `net.connman.Manager.SetProperty', which takes a
%% string and a variant, pass in types: `[string, variant]' and
%% arguments: `["OfflineMode", true]'.
%%
%% Returns the response from the call. Note that a response from a
%% call is always a list even if only one value is returned.
-spec call(ebus:proxy(), Path::string(), Member::string(),
           Types::ebus:signature(), Args::[any()],
           Timeout::integer()) -> {ok, Response::term()} | {error, term()}.
call(Pid, Path, Member, Types, Args, Timeout) ->
    gen_server:call(Pid, {call, Path, Member, Types, Args, Timeout}, infinity).

-spec send(ebus:proxy(), string()) -> ok | {error, term()}.
send(Pid, Member) ->
    send(Pid, "/", Member).

-spec send(ebus:proxy(), Path::string(), Member::string()) -> {ok, Response::term()} | {error, term()}.
send(Pid, Path, Member) ->
    send(Pid, Path, Member, [], []).

-spec send(ebus:proxy(), Path::string(), Member::string(),
           Sig::ebus:signature(), Args::[any()]) -> ok | {error, term()}.
send(Pid, Path, Member, Types, Args) ->
    gen_server:call(Pid, {send, Path, Member, Types, Args}, infinity).

%% @doc Add a signal handler for the given Member and default root
%% path "/". Equivalent to calling add_signal_handler/5.
-spec add_signal_handler(ebus:proxy(), Member::string(), Handler::pid(), Info::any())
                        -> {ok, ebus:filter_id()} | {error, term()}.
add_signal_handler(Pid, Member, Handler, Info) ->
    add_signal_handler(Pid, "/", Member, Handler, Info).

%% @doc Add a signal handler for the given path, interface/member. The
%% handler is a pid which will be sent `{ebus_signal, Info,
%% SignalID::ebus:filter_id(), Message::ebus:message()' with the given
%% info when the handler was added and the signal id returned from
%% this function. The proxy will manage the underlying edbus filter
%% and match addition.
-spec add_signal_handler(ebus:proxy(), Path::string(), Member::string(), Handler::pid(), Info::any())
                        -> {ok, ebus:filter_id()} | {error, term()}.
add_signal_handler(Pid, Path, Member, Handler, Info) ->
    gen_server:call(Pid, {add_signal_handler, Path, Member, Handler, Info}).

%% %doc Remove a signal handler with the gien signal id, handler pid
%% and info. Duplicate handler/info combinations are allowed, but
%% calling remove_signal_handler will only remove _one_ of them for
%% the givne signal.
-spec remove_signal_handler(ebus:proxy(), SignalID::ebus:filter_id(), Handler::pid(), Info::any()) -> ok.
remove_signal_handler(Pid, Ref, Handler, Info) ->
    gen_server:cast(Pid, {remove_signal_handler, Ref, Handler, Info}).

%% @doc Returns the bus the proxy uses to communicate over.
-spec bus(ebus:proxy()) -> ebus:bus().
bus(Pid) ->
    gen_server:call(Pid, bus).

-spec stop(ebus:proxy()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec managed_objects(ebus:proxy()) -> {ok, term()} | {error, term()}.
managed_objects(Pid) ->
    managed_objects(Pid, "/").

-spec managed_objects(ebus:proxy(), Path::string()) -> {ok, term()} | {error, term()}.
managed_objects(Pid, Path) ->
    call(Pid, Path, ?DBUS_OBJECT_MANAGER("GetManagedObjects"), [], []).

%% gen_server
%%

start(Bus, Dest, Options) ->
    gen_server:start(?MODULE, [Bus, Dest], Options).

start_link(Bus, Dest, Options) ->
    gen_server:start_link(?MODULE, [Bus, Dest], Options).

init([Bus, Dest]) ->
    {ok, #state{bus=Bus, dest=Dest}}.



handle_call({call, Path, Member, Types, Args, Timeout}, From, State=#state{}) ->
    case ebus_message:new_call(State#state.dest, Path, Member) of
        {ok, Msg} ->
            case ebus_message:append_args(Msg, Types, Args) of
                ok ->
                    case ebus:call(State#state.bus, Msg, self(), Timeout) of
                        {ok, Serial} ->
                            NewCalls = maps:put(Serial, From, State#state.calls),
                            {noreply, State#state{calls=NewCalls}};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({send, Path, Member, Types, Args}, _From, State=#state{}) ->
    case ebus_message:new_call(State#state.dest, Path, Member) of
        {ok, Msg} ->
            case ebus_message:append_args(Msg, Types, Args) of
                ok ->
                    case ebus:call(State#state.bus, Msg, self(), ?PROXY_SEND_TIMEOUT) of
                        {ok, _} -> {reply, ok, State};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({add_signal_handler, Path, Member, Handler, Info}, _From, State=#state{}) ->
    {IFace, Name} = ebus_message:split_interface_member(Member),
    case signal_handler_add(Path, Member, {IFace, Name}, Handler, Info, State) of
        {ok, SignalID, NewState} ->
            {reply, {ok, SignalID}, NewState};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call(bus, _From, State=#state{}) ->
    {reply, State#state.bus, State};

handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {noreply, State}.


handle_cast({remove_signal_handler, SignalID, Handler, Info}, State=#state{}) ->
    {noreply, signal_handler_remove(SignalID, Handler, Info, State)};

handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info({handle_reply, Msg}, State=#state{}) ->
    case maps:take(ebus_message:reply_serial(Msg), State#state.calls) of
        {From, NewCalls} ->
            Reply = case ebus_message:type(Msg) of
                        error ->
                            ebus_message:error(Msg);
                        reply ->
                            ebus_message:args(Msg);
                        Other ->
                            {error, {bad_message, Other}}
                    end,
            gen_server:reply(From, Reply),
            {noreply, State#state{calls=NewCalls}};
        error ->
            {noreply, State}
    end;

handle_info({filter_match, SignalID, Msg}, State=#state{signal_handlers=SigHandlers}) ->
    case maps:get(SignalID, SigHandlers, false) of
        false -> {noreply, State};
        #handler_entry{handlers=Handlers} ->
            lists:foreach(fun({Handler, Info}) ->
                                  Handler ! {ebus_signal, Info, SignalID, Msg}
                          end, Handlers),
            {noreply, State}
    end;

handle_info(Msg, State=#state{}) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.


%%
%% Private
%%

signal_group_id(Path, Member) ->
    {ebus_signal, Path, Member}.

-spec signal_handler_add(ebus:object_path(), string(), {ebus:interface(), string()}, pid(), any(), #state{})
                        -> {ok, ebus:filter_id(), #state{}} | {error, term()}.
signal_handler_add(_Path, _Member, {undefined, _}, _Handler, _Info, _State=#state{}) ->
    {error, no_interface};
signal_handler_add(Path, Member, {IFace, Name}, Handler, Info, State=#state{signal_handlers=SigHandlers}) ->
    case maps:to_list(maps:filter(fun(_SignalID, #handler_entry{path=P, member=M}) ->
                                          P == Path andalso M == Member
                                  end, SigHandlers)) of
        [] ->
            Rule = #{ path => Path,
                      type => signal,
                      member => Name,
                      interface => IFace
                    },
            ok = ebus:add_match(State#state.bus, Rule),
            case ebus:add_filter(State#state.bus, self(), Rule) of
                {ok, SignalID} ->
                    GroupID = signal_group_id(Path, Member),
                    pg2:create(GroupID),
                    pg2:join(GroupID, Handler),
                    NewSigHandlers = maps:put(SignalID,
                                              #handler_entry{path=Path, member=Member, rule=Rule,
                                                             handlers=[{Handler, Info}]},
                                              SigHandlers),
                    {ok, SignalID, State#state{signal_handlers=NewSigHandlers}};
                {error, Error} ->
                    {error, Error}
            end;
        [{SignalID, Entry=#handler_entry{handlers=Handlers}}] ->
            pg2:join(signal_group_id(Path, Member), Handler),
            NewSigHandlers = maps:put(SignalID,
                                      Entry#handler_entry{handlers=[{Handler, Info} | Handlers]},
                                      SigHandlers),
            {ok, SignalID, State#state{signal_handlers=NewSigHandlers}}
    end.

-spec signal_handler_remove(ebus:filter_id(), pid(), any(), #state{}) -> #state{}.
signal_handler_remove(SignalID, Handler, Info, State=#state{signal_handlers=SigHandlers}) ->
    case maps:get(SignalID, SigHandlers, false) of
        false -> State;
        Entry=#handler_entry{path=Path, member=Member, rule=Rule, handlers=Handlers} ->
            case lists:delete({Handler, Info}, Handlers) of
                [] ->
                    ebus:remove_match(State#state.bus, Rule),
                    ebus:remove_filter(State#state.bus, SignalID),
                    pg2:delete(signal_group_id(Path, Member)),
                    NewSigHandlers = maps:remove(SignalID, SigHandlers),
                    State#state{signal_handlers=NewSigHandlers};
                NewHandlers ->
                    pg2:leave(signal_group_id(Path, Member), Handler),
                    NewSigHandlers = maps:put(SignalID,
                                              Entry#handler_entry{handlers=NewHandlers},
                                              SigHandlers),
                    State#state{signal_handlers=NewSigHandlers}
            end
    end.
