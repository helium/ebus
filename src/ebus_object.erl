-module(ebus_object).

-type init_result() ::
        {ok, State :: any()} |
        {stop, Reason :: term()}.

-type action() ::
        {signal, Interface::string(), Member::string()} |
        {signal, Interface::string(), Member::string(), Types::ebus:signature(), Args::[any()]} |
        {send, Dest::string(), Interface::string(), Member::string()} |
        {send, Dest::string(), Interface::string(), Member::string(), Types::ebus:signature(), Args::[any()]} |
        {send, Message::ebus:message()}.

-type handle_call_result() ::
        {reply, Reply :: term(), State :: any()} |
        {reply, Reply :: term(), State :: any(), Action :: action()} |
        {noreply, State :: any()} |
        {noreply, State :: any(), Action :: action()} |
        {stop, Reason :: term(), State::any()} |
        {stop, Reason :: term(), Reply::term(), State::any()}.

-type handle_cast_result() ::
        {noreply, State::any()} |
        {noreply, State::any(), Action :: action()} |
        {stop, Reason::term(), State::any()}.

-type handle_info_result() ::
        {noreply, State :: any()} |
        {noreply, State :: any(), Action :: action()} |
        {stop, Reason :: term(), State::any()}.

-type handle_message_result() ::
        {reply, Reply :: ebus:message(), State::any()} |
        {noreply, State::any()} |
        {stop, Reason :: term(), State::any()}.


-callback init(Args::[any()]) -> init_result().
-callback handle_message(Member::string(), Message::ebus:message(), State::any()) -> handle_message_result().
-callback handle_call(Msg::term(), From::term(), State::any()) -> handle_call_result().
-callback handle_cast(Msg::term(), State::any()) -> handle_cast_result().
-callback handle_info(Msg::term(), State::any()) -> handle_info_result().

-optional_callbacks([handle_info/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

%% gen_server
-export([start/5, start_link/5, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {
                module :: atom(),
                state :: any(),
                path :: string(),
                bus :: pid()
               }).

start(Bus, Path, Module, Args, Options) ->
    gen_server:start(?MODULE, [Bus, Path, Module, Args], Options).

start_link(Bus, Path, Module, Args, Options) ->
    gen_server:start_link(?MODULE, [Bus, Path, Module, Args], Options).

init([Bus, Path, Module, Args]) ->
    case Module:init(Args) of
        {ok, MState} ->
            case ebus:register_object_path(Bus, Path, self()) of
                ok ->
                    {ok, #state{bus=Bus, path=Path, module=Module, state=MState}};
                Other -> Other
            end;
        Other -> Other
    end.

handle_call(Msg, From, State=#state{module=Module, state=ModuleState0}) ->
    case erlang:function_exported(Module, handle_call, 3) of
        true -> case Module:handle_call(Msg, From, ModuleState0) of
                    {reply, Reply, ModuleState} ->
                        {reply, Reply, State#state{state=ModuleState}};
                    {reply, Reply, ModuleState, Action} ->
                        {reply, Reply, handle_action(Action, State#state{state=ModuleState})};
                    {noreply, ModuleState}  ->
                        {noreply, State#state{state=ModuleState}};
                    {noreply, ModuleState, Action} ->
                        {noreply, handle_action(Action, State#state{state=ModuleState})};
                    {stop, Reason, ModuleState} ->
                        {stop, Reason, State#state{state=ModuleState}};
                    {stop, Reason, Reply, ModuleState} ->
                        {stop, Reason, Reply, State#state{state=ModuleState}}
                end;
        false -> {reply, ok, State}
    end.

handle_cast(Msg, State=#state{module=Module, state=ModuleState0}) ->
    case erlang:function_exported(Module, handle_cast, 2) of
        true -> case Module:handle_cast(Msg, ModuleState0) of
                    {noreply, ModuleState}  ->
                        {noreply, State#state{state=ModuleState}};
                    {noreply, ModuleState, Action} ->
                        {noreply, handle_action(Action, State#state{state=ModuleState})};
                    {stop, Reason, ModuleState} ->
                        {stop, Reason, State#state{state=ModuleState}}
                end;
        false -> {noreply, State}
    end.

handle_info({handle_message, Msg}, State=#state{module=Module, state=ModuleState0}) ->
    case erlang:function_exported(Module, handle_message, 3) of
        true ->
            case Module:handle_message(ebus_message:member(Msg), Msg, ModuleState0) of
                {reply, ReplyMsg, ModuleState} ->
                    {noreply, handle_action({send, ReplyMsg}, State#state{state=ModuleState})};
                {noreply, ModuleState}  ->
                    {noreply, State#state{state=ModuleState}};
                {stop, Reason, ModuleState} ->
                    {stop, Reason, State#state{state=ModuleState}}
            end;
        false -> {noreply, State}
    end;
handle_info(Msg, State=#state{module=Module, state=ModuleState0}) ->
    case erlang:function_exported(Module, handle_info, 2) of
        true -> case Module:handle_info(Msg, ModuleState0) of
                    {noreply, ModuleState}  ->
                        {noreply, State#state{state=ModuleState}};
                    {noreply, ModuleState, Action} ->
                        {noreply, handle_action(Action, State#state{state=ModuleState})};
                    {stop, Reason, ModuleState} ->
                        {stop, Reason, State#state{state=ModuleState}}
                end;
        false -> {noreply, State}
    end.

terminate(Reason, #state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Reason, ModuleState);
        false -> ok
    end.



%%
%% Internal
%%

handle_action({signal, Interface, Member}, State=#state{}) ->
    handle_action({signal, Interface, Member, [], []}, State);
handle_action({signal, Interface, Member, Types, Args}, State=#state{}) ->
    {ok, Msg}  = ebus_message:new_signal(State#state.path, Interface, Member),
    ok = ebus_message:append_args(Msg, Types, Args),
    ok = ebus:send(State#state.bus, Msg),
    State;
handle_action({send, Dest, Interface, Member}, State=#state{}) ->
    handle_action({send, Dest, Interface, Member, [], []}, State);
handle_action({send, Dest, Interface, Member, Types, Args}, State=#state{}) ->
    {ok, Msg}  = ebus_message:new_call(Dest, State#state.path, Interface, Member),
    ok = ebus_message:append_args(Msg, Types, Args),
    handle_action({send, Msg}, State);
handle_action({send, Msg}, State=#state{}) ->
    ok = ebus:send(State#state.bus, Msg),
    State;
handle_action(_, State=#state{}) ->
    State.
