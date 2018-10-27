-module(ebus_proxy).


-behavior(gen_server).

%% API
-export([call/2, call/3, call/5, call/6,
        send/2, send/3, send/5,
        add_signal_handler/3, add_signal_handler/4, remove_signal_handler/2]).

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


-spec call(pid(), string()) -> {ok, Response::term()} | {error, term()}.
call(Pid, Member) ->
    call(Pid, "/", Member).

call(Pid, Path, Member) ->
    call(Pid, Path, Member, [], []).

-spec call(pid(), string(), string(), ebus:signature(), [any()]) -> {ok, Response::term()} | {error, term()}.
call(Pid, Path, Member, Types, Args) ->
    %% Using the dbus recommended `-1` means a 25 second (!) timeout
    call(Pid, Path, Member, Types, Args, 5000).

-spec call(pid(), Path::string(), Member::string(),
           Sig::ebus:signature(), Args::[any()],
           Timeout::integer()) -> {ok, Response::term()} | {error, term()}.
call(Pid, Path, Member, Types, Args, Timeout) ->
    gen_server:call(Pid, {call, Path, Member, Types, Args, Timeout}, infinity).

-spec send(pid(), string()) -> ok | {error, term()}.
send(Pid, Member) ->
    send(Pid, "/", Member).

send(Pid, Path, Member) ->
    send(Pid, Path, Member, [], []).

-spec send(pid(), Path::string(), Member::string(),
           Sig::ebus:signature(), Args::[any()]) -> ok | {error, term()}.
send(Pid, Path, Member, Types, Args) ->
    gen_server:call(Pid, {send, Path, Member, Types, Args}, infinity).

-spec add_signal_handler(pid(), Member::string(), Handler::pid())
                        -> {ok, ebus:filter_id()} | {error, term()}.
add_signal_handler(Pid, Member, Handler) ->
    add_signal_handler(Pid, "/", Member, Handler).

-spec add_signal_handler(pid(), Path::string(), Member::string(), Handler::pid())
                        -> {ok, ebus:filter_id()} | {error, term()}.
add_signal_handler(Pid, Path, Member, Handler) ->
    gen_server:call(Pid, {add_signal_handler, Path, Member, Handler}).

-spec remove_signal_handler(pid(), ebus:filter_id()) -> ok.
remove_signal_handler(Pid, Ref) ->
    gen_server:cast(Pid, {remove_signal_handler, Ref}).

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
    {IFace, Name} = ebus_message:interface_member(Member),
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
