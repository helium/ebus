-module(ebus_proxy).


-behavior(gen_server).

%% API
-export([call/2, call/4, call/5]).

%% gen_server
-export([start/4, start/5, start_link/4, start_link/5, init/1,
        handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
                bus :: pid(),
                dest :: string(),
                path :: string(),
                interface=undefined :: string() | undefined,
                calls=#{} :: #{Serial::non_neg_integer() => From::term()}
               }).

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


%% gen_server
%%

start(Bus, Dest, Options) ->
    gen_server:start(?MODULE, [Bus, Dest], Options).

start_link(Bus, Dest, Options) ->
    gen_server:start_link(?MODULE, [Bus, Dest], Options).

init([Bus, Dest]) ->
    {ok, #state{bus=Bus, dest=Dest}}.



handle_call({call, IFace, Member, Types, Args, Timeout}, From, State=#state{}) ->
    SelectedIFace = case IFace of
                        undefined -> State#state.interface;
                        _ -> IFace
                    end,
    case ebus_message:new_call(State#state.dest, State#state.path, SelectedIFace, Member) of
        {ok, Msg} ->
            case ebus_message:append_args(Msg, Types, Args) of
                ok ->
                    case ebus:call(State#state.bus, Msg, self(), Timeout) of
                        ok ->
                            NewCalls = maps:put(ebus_message:serial(Msg), From, State#state.calls),
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


handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {noreply, State}.


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
