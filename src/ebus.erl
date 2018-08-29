-module(ebus).

-behavior(gen_server).

%% gen_server
-export([start_link/0, start_link/1, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

%% API
-export([unique_name/1,
         request_name/3, release_name/2,
         add_match/2,
         add_filter/3, remove_filter/2,
         send/2]).

-type connection() :: reference().
-type watch() :: reference().
-type bus_type() :: session | system | starter.
-export_type([connection/0, bus_type/0, watch/0]).

-type filter() :: #{
                    type => signal | call,
                    destination => string(),
                    path => string(),
                    interface => string(),
                    member => string()
                   }.
-export_type([filter/0]).

-type message() :: reference().
-type signature() :: [value_type()].
-type value_type() :: byte
                    | bool
                    | int16
                    | uint16
                    | int32
                    | uint32
                    | int64
                    | uint64
                    | double
                    | string
                    | object_pathpe
                    | signature
                    | variant
                    | {array, value_type()}
                    | {struct, [value_type()]}
                    | {dict, KeyType::value_type(), ValueType::value_type}.
-export_type([message/0, signature/0, value_type/0]).

-type request_name_opts() :: [request_name_opt()].
-type request_name_opt() :: {replace_existing, boolean()}
                           | {allow_replacement, boolean()}
                           | {do_not_queue, boolean()}.
-type request_name_reply() :: ok
                            | {error, queued}
                            | {error, term()}.
-type release_name_reply() :: ok
                            | {error, not_found}
                            | {error, not_owner}.
-export_type([request_name_opts/0, request_name_reply/0, release_name_reply/0]).

-record(state, {
                connection :: connection(),
                timeouts=#{} :: #{Timeout::reference() => Timer::reference()},
                filters=[] :: [{reference(), filter()}],
                filter_targets=#{} :: #{reference() => pid()},
                filter_next_id=0 :: non_neg_integer()
               }).

-define(SESSION, 0).
-define(SYSTEM,  1).
-define(STARTER, 2).

%% Allow another service to become the primary owner if requested.
-define(NAME_FLAG_ALLOW_REPLACEMENT, 1).
%% Request to replace the current primary owner.
-define(NAME_FLAG_REPLACE_EXISTING,  2).
%% If we can not become the primary owner do not place us in the queue.
-define(NAME_FLAG_DO_NOT_QUEUE,      4).

%% Service has become the primary owner of the requested name.
-define(REQUEST_NAME_REPLY_PRIMARY_OWNER,  1).
%% Service could not become the primary owner and has been placed in the queue.
-define(REQUEST_NAME_REPLY_IN_QUEUE,       2).
%% Service is already in the queue.
-define(REQUEST_NAME_REPLY_EXISTS,         3).
%% Service is already the primary owner.
-define(REQUEST_NAME_REPLY_ALREADY_OWNER,  4).

%% Service was released from the given name.
-define(RELEASE_NAME_REPLY_RELEASED,       1).
%% The given name does not exist on the bus.
-define(RELEASE_NAME_REPLY_NON_EXISTENT,   2).
%% Service is not an owner of the given name.
-define(RELEASE_NAME_REPLY_NOT_OWNER,      3).

-define(WATCH_READABLE, 1).
-define(WATCH_WRITABLE, 2).
-define(WATCH_ERROR,    4).
-define(WATCH_HANGUP,   8).

-define(MESSAGE_TYPE_METHOD_CALL, 1).
-define(MESSAGE_TYPE_SIGNAL, 4).

%% There is more data to potentially convert to messages.
-define(DISPATCH_DATA_REMAINS, 0).
%% All currently available data has been processed.
-define(DISPATCH_COMPLETE,     1).
-define(DISPATCH_NEED_MEMORY,  2) .

%% API
%%

-spec unique_name(pid()) -> string().
unique_name(Pid) ->
    gen_server:call(Pid, unique_name).


-spec request_name(pid(), Name::string(), Opts::request_name_opts()) -> request_name_reply().
request_name(Pid, Name, Opts) when is_list(Opts) ->
    gen_server:call(Pid, {request_name, Name, Opts}).

-spec release_name(pid(), Name::string()) -> release_name_reply().
release_name(Pid, Name) ->
    gen_server:call(Pid, {release_name, Name}).

-spec add_match(pid(), Rule::string()) -> ok | {error, term()}.
add_match(Pid, Rule) ->
    gen_server:call(Pid, {add_match, Rule}).

-spec add_filter(pid(), pid(), filter()) -> {ok, reference()} | {error, term()}.
add_filter(Pid, Target, Filter) ->
    gen_server:call(Pid, {add_filter, Target, Filter}).

-spec remove_filter(pid(), reference()) -> ok.
remove_filter(Pid, Ref) ->
    gen_server:cast(Pid, {remove_filter, Ref}).

-spec send(pid(), message()) -> any().
send(Pid, Msg) ->
    gen_server:call(Pid, {send, Msg}).

%% gen_server
%%

start_link() ->
    start_link(starter).

start_link(Type) ->
    IntType = case Type of
                  session -> ?SESSION;
                  system -> ?SYSTEM;
                  starter -> ?STARTER;
                  _ -> error(badarg)
              end,
    gen_server:start_link(?MODULE, [IntType], []).

init([IntType]) ->
    erlang:process_flag(trap_exit, true),
    {ok, Conn} = ebus_nif:connection_get(IntType, self()),
    {ok, #state{connection=Conn}}.


handle_call(unique_name, _From, State=#state{connection=Conn}) ->
    {reply, ebus_nif:connection_unique_name(Conn), State};
handle_call({request_name, Name, Opts}, _From, State=#state{connection=Conn}) ->
    Flags = lists:foldl(fun({replace_existing, true}, Acc) ->
                                Acc bor ?NAME_FLAG_REPLACE_EXISTING;
                           ({allow_replacement, true}, Acc) ->
                                Acc bor ?NAME_FLAG_ALLOW_REPLACEMENT;
                           ({do_not_queue, true}, Acc) ->
                                Acc bor ?NAME_FLAG_DO_NOT_QUEUE;
                           (_, Acc) ->
                                Acc
                        end, 0, Opts),
    Reply = case ebus_nif:connection_request_name(Conn, Name, Flags) of
                {ok, ?REQUEST_NAME_REPLY_PRIMARY_OWNER} -> ok;
                {ok, ?REQUEST_NAME_REPLY_IN_QUEUE} -> {error, queued};
                {ok, ?REQUEST_NAME_REPLY_EXISTS} -> {error, queued};
                {ok, ?REQUEST_NAME_REPLY_ALREADY_OWNER} -> ok;
                {error, Msg} -> {error, Msg}
            end,
    {reply, Reply, State};
handle_call({release_name, Name}, _From, State=#state{connection=Conn}) ->
    Reply = case ebus_nif:connection_release_name(Conn, Name) of
                {ok, ?RELEASE_NAME_REPLY_RELEASED} -> ok;
                {ok, ?RELEASE_NAME_REPLY_NON_EXISTENT} -> {error, not_found};
                {ok, ?RELEASE_NAME_REPLY_NOT_OWNER} -> {error, not_owner}
            end,
    {reply, Reply, State};
handle_call({add_match, Rule}, _From, State=#state{connection=Conn}) ->
    {reply, ebus_nif:connection_add_match(Conn, Rule), State};
handle_call({send, Msg}, _From, State=#state{connection=Conn}) ->
    {ok, _Serial} = ebus_nif:connection_send(Conn, Msg),
    {reply, ok, State};
handle_call({add_filter, Target, Filter}, _From,
            State=#state{connection=Conn, filter_next_id=FilterId, filters=Filters, filter_targets=Targets}) ->
    io:format("SETTING FILTERS"),
    NewFilters = ebus_nif:connection_set_filters(Conn, [{FilterId, Filter} | Filters]),
    NewTargets = maps:put(FilterId, Target, Targets),
    io:format("FILTERS: ~p", [NewFilters]),
    {reply, {ok, FilterId}, State#state{filter_targets=NewTargets, filters=NewFilters,
                                        filter_next_id=FilterId + 1}};

handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {noreply, State}.


handle_cast({remove_filter, Ref},
            State=#state{connection=Conn, filters=Filters, filter_targets=Targets}) ->
    NewFilters = ebus_nif:connection_set_filters(Conn, lists:keydelete(Ref, 1, Filters)),
    NewTargets = maps:take(Ref, Targets),
    {noreply, State#state{filters=NewFilters, filter_targets=NewTargets}};

handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info({filter_match, Ref, Msg}, State=#state{filter_targets=Targets}) ->
    case maps:get(Ref, Targets, not_found) of
        not_found ->
            io:format("NO TARGET"),
            {noreply, State};
        Pid ->
            io:format("TARGET!"),
            case (catch Pid ! {filter_match, Msg}) of
                {'EXIT', Other} ->
                    io:format("DISPATCH FAIL ~p", [Other]),
                    {noreply, State#state{filter_targets=maps:remove(Ref, Targets)}};
                Res ->
                    io:format("DISPATCHED FILTER MATCH ~p ! ~p", [Pid, Res]),
                    {noreply, State}
            end
    end;
handle_info({select, Watch, undefined, Flags}, State=#state{connection=Conn}) ->
    io:format("SELECT ~p", [Flags]),
    BusFlag = case Flags of
                  ready_input -> ?WATCH_READABLE;
                  ready_output -> ?WATCH_WRITABLE
              end,
    ebus_nif:watch_handle(Watch, BusFlag),
    io:format("AFTER WATCH_HANDLE"),
    self() ! {dispatch_status, ebus_nif:connection_dispatch(Conn)},
    {noreply, State};
handle_info(wakeup_main, State=#state{connection=Conn}) ->
    self() ! {dispatch_status, ebus_nif:connection_dispatch(Conn)},
    {noreply, State};
handle_info({dispatch_status, Status}, State=#state{connection=Conn}) ->
    case Status of
        ?DISPATCH_DATA_REMAINS ->
            io:format("DISPATCH SOME MORE"),
            self() ! {dispatch_status, ebus_nif:connection_dispatch(Conn)};
        ?DISPATCH_COMPLETE -> ok;
        ?DISPATCH_NEED_MEMORY ->
            lager:error("DBus is asking for more memory")
    end,
    {noreply, State};
handle_info({add_timeout, Ref, Millis}, State=#state{timeouts=Timeouts}) ->
    Timer = erlang:send_after(Millis, self(), {imeout, Ref}),
    {noreply, State#state{timeouts=maps:put(Ref, Timer, Timeouts)}};
handle_info({remove_timeout, Ref}, State=#state{timeouts=Timeouts}) ->
    case maps:take(Ref, Timeouts) of
        error ->
            {noreply, State};
        {Timer, NewTimeouts} ->
            erlang:cancel_timer(Timer),
            {noreply, State#state{timeouts=NewTimeouts}}
    end;
handle_info({timeout, Ref}, State=#state{timeouts=Timeouts, connection=Conn}) ->
    case maps:take(Ref, Timeouts) of
        error ->
            %% Timer was already canceled by nif through a
            %% remove_timeout
            {noreply, State};
        {_, NewTimeouts} ->
            ebus_nif:timeout_handle(Ref),
            self() ! {dispatch_status, ebus_nif:connection_dispatch(Conn)},
            {noreply, State#state{timeouts=NewTimeouts}}
    end;

handle_info(Msg, State=#state{}) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.


terminate(_Reason, #state{connection=Conn}) ->
    ebus_nif:connection_close(Conn).
