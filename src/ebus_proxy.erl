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


-spec call(pid(), string()) -> ok | {error, term()}.
call(Pid, Member) ->
    call(Pid, Member, [], []).

-spec call(pid(), string(), ebus:signature(), [any()]) -> ok | {error, term()}.
call(Pid, Member, Types, Args) ->
    %% Using the dbus recommended `-1` means a 25 second (!) timeout
    call(Pid, Member, Types, Args, 5000).

-spec call(pid(), string(), ebus:signature(), [any()], integer()) -> ok | {error, term()}.
call(Pid, Member, Types, Args, Timeout) ->
    gen_server:call(Pid, {call, Member, Types, Args, Timeout}, infinity).


%% gen_server
%%

start(Bus, Dest, Path, Options) ->
    start(Bus, Dest, Path, undefined, Options).

start(Bus, Dest, Path, IFace, Options) ->
    gen_server:start(?MODULE, [Bus, Dest, Path, IFace], Options).

start_link(Bus, Dest, Path, Options) ->
    start_link(Bus, Dest, Path, undefined, Options).

start_link(Bus, Dest, Path, IFace, Options) ->
    gen_server:start_link(?MODULE, [Bus, Dest, Path, IFace], Options).

init([Bus, Dest, Path, IFace]) ->
    {ok, #state{bus=Bus, dest=Dest, path=Path, interface=IFace}}.


handle_call({call, Member, Types, Args, Timeout}, From, State=#state{}) ->
    case ebus_message:new_call(State#state.dest, State#state.path, State#state.interface, Member) of
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
