-module(ebus_proxy).

-include("ebus.hrl").

-behavior(gen_server).

%% API
-export([call/2, call/3, call/5, call/6,
        bus/1, send/2, send/3, send/5,
        add_signal_handler/3, add_signal_handler/4, remove_signal_handler/2,
        stop/1]).
-export([managed_objects/1, managed_objects/2]).

%% gen_server
-export([start/3, start_link/3, init/1,
        handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
                bus :: pid(),
                dest :: string(),
                calls=#{} :: #{Serial::non_neg_integer() => From::term()},
                active_filters=#{} :: #{ebus:filter_id() => Filter::ebus:rule()}
               }).

-define(PROXY_SEND_TIMEOUT, 5000).

%% API
%%


-spec call(ebus:proxy(), string()) -> {ok, Response::term()} | {error, term()}.
call(Pid, Member) ->
    call(Pid, "/", Member).

call(Pid, Path, Member) ->
    call(Pid, Path, Member, [], []).

-spec call(ebus:proxy(), string(), string(), ebus:signature(), [any()])
          -> {ok, Response::term()} | {error, term()}.
call(Pid, Path, Member, Types, Args) ->
    %% Using the dbus recommended `-1` means a 25 second (!) timeout
    call(Pid, Path, Member, Types, Args, 5000).

-spec call(ebus:proxy(), Path::string(), Member::string(),
           Sig::ebus:signature(), Args::[any()],
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

-spec add_signal_handler(ebus:proxy(), Member::string(), Handler::pid())
                        -> {ok, ebus:filter_id()} | {error, term()}.
add_signal_handler(Pid, Member, Handler) ->
    add_signal_handler(Pid, "/", Member, Handler).

-spec add_signal_handler(ebus:proxy(), Path::string(), Member::string(), Handler::pid())
                        -> {ok, ebus:filter_id()} | {error, term()}.
add_signal_handler(Pid, Path, Member, Handler) ->
    gen_server:call(Pid, {add_signal_handler, Path, Member, Handler}).

-spec remove_signal_handler(ebus:proxy(), ebus:filter_id()) -> ok.
remove_signal_handler(Pid, Ref) ->
    gen_server:cast(Pid, {remove_signal_handler, Ref}).

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

handle_call({add_signal_handler, Path, Member, Handler}, _From, State=#state{}) ->
    {IFace, Name} = ebus_message:split_interface_member(Member),
    case IFace of
        undefined -> {reply, {error, missing_interface}, State};
        IFace ->
            Filter = #{ path => Path,
                        type => signal,
                        member => Name,
                        interface => IFace
                      },
            ok = ebus:add_match(State#state.bus, Filter),
            case ebus:add_filter(State#state.bus, Handler, Filter) of
                {ok, FilterID} ->
                    NewFilters = maps:put(FilterID, Filter, State#state.active_filters),
                    {reply, {ok, FilterID}, State#state{active_filters=NewFilters}};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end
    end;

handle_call(bus, _From, State=#state{}) ->
    {reply, State#state.bus, State};

handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {noreply, State}.


handle_cast({remove_signal_handler, FilterID}, State=#state{}) ->
    case maps:take(FilterID, State#state.active_filters) of
        {Filter, NewFilters} ->
            ebus:remove_match(State#state.bus, Filter),
            ebus:remove_filter(State#state.bus, FilterID),
            {noreply, State#state{active_filters=NewFilters}};
        error ->
            {noreply, State}
    end;

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

handle_info(Msg, State=#state{}) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.
