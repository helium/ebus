-module(ebus).

-behavior(gen_server).

%% gen_server
-export([start_link/0, start_link/1, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

%% API
-export([unique_name/1,
         request_name/3, release_name/2,
         add_match/2]).

-type connection() :: reference().
-type message() :: reference().
-type watch() :: reference().
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
-type bus_type() :: session | system | starter.

-type request_name_opts() :: [request_name_opt()].
-type request_name_opt() :: {replace_existing, boolean()}
                           | {allow_replacement, boolean()}
                           | {do_not_queue, boolean()}.
-type request_name_reply() :: ok
                            | {error, queued}
                            | {error, string()}.

-type release_name_reply() :: ok
                            | {error, not_found}
                            | {error, not_owner}.

-export_type([connection/0, bus_type/0,
              message/0, signature/0, value_type/0,
              request_name_opts/0, request_name_reply/0,
              release_name_reply/0]).

-record(state, {
                connection :: connection(),
                watches=[] :: [watch()]
               }).

-define(BUS_SESSION, 0).
-define(BUS_SYSTEM,  1).
-define(BUS_STARTER, 2).

%% Allow another service to become the primary owner if requested.
-define(BUS_NAME_FLAG_ALLOW_REPLACEMENT, 1).
%% Request to replace the current primary owner.
-define(BUS_NAME_FLAG_REPLACE_EXISTING,  2).
%% If we can not become the primary owner do not place us in the queue.
-define(BUS_NAME_FLAG_DO_NOT_QUEUE,      4).

%% Service has become the primary owner of the requested name.
-define(BUS_REQUEST_NAME_REPLY_PRIMARY_OWNER,  1).
%% Service could not become the primary owner and has been placed in the queue.
-define(BUS_REQUEST_NAME_REPLY_IN_QUEUE,       2).
%% Service is already in the queue.
-define(BUS_REQUEST_NAME_REPLY_EXISTS,         3).
%% Service is already the primary owner.
-define(BUS_REQUEST_NAME_REPLY_ALREADY_OWNER,  4).

%% Service was released from the given name.
-define(BUS_RELEASE_NAME_REPLY_RELEASED,       1).
%% The given name does not exist on the bus.
-define(BUS_RELEASE_NAME_REPLY_NON_EXISTENT,   2).
%% Service is not an owner of the given name.
-define(BUS_RELEASE_NAME_REPLY_NOT_OWNER,      3).

-define(BUS_WATCH_READABLE, 1).
-define(BUS_WATCH_WRITABLE, 2).
-define(BUS_WATCH_ERROR,    4).
-define(BUS_WATCH_HANGUP,   8).

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

-spec add_match(pid(), Rule::string()) -> ok | {error, string()}.
add_match(Pid, Rule) ->
    gen_server:call(Pid, {add_match, Rule}).


%% gen_server
%%

start_link() ->
    start_link(starter).

start_link(Type) ->
    IntType = case Type of
                  session -> ?BUS_SESSION;
                  system -> ?BUS_SYSTEM;
                  starter -> ?BUS_STARTER;
                  _ -> error(badarg)
              end,
    gen_server:start_link(?MODULE, [IntType], []).

init([IntType]) ->
    {ok, Conn} = ebus_nif:bus(IntType, self()),
    {ok, #state{connection=Conn}}.


handle_call(unique_name, _From, State=#state{connection=Conn}) ->
    {reply, ebus_nif:unique_name(Conn), State};
handle_call({request_name, Name, Opts}, _From, State=#state{connection=Conn}) ->
    Flags = lists:foldl(fun({replace_existing, true}, Acc) -> Acc bor ?BUS_NAME_FLAG_REPLACE_EXISTING;
                           ({allow_replacement, true}, Acc) -> Acc bor ?BUS_NAME_FLAG_ALLOW_REPLACEMENT;
                           ({do_not_queue, true}, Acc) -> Acc bor ?BUS_NAME_FLAG_DO_NOT_QUEUE;
                           (_, Acc) -> Acc
                        end, 0, Opts),
    Reply = case ebus_nif:request_name(Conn, Name, Flags) of
                {ok, ?BUS_REQUEST_NAME_REPLY_PRIMARY_OWNER} -> ok;
                {ok, ?BUS_REQUEST_NAME_REPLY_IN_QUEUE} -> {error, queued};
                {ok, ?BUS_REQUEST_NAME_REPLY_EXISTS} -> {error, queued};
                {ok, ?BUS_REQUEST_NAME_REPLY_ALREADY_OWNER} -> ok;
                {error, Msg} -> {error, Msg}
            end,
    {reply, Reply, State};
handle_call({release_name, Name}, _From, State=#state{connection=Conn}) ->
    Reply = case ebus_nif:release_name(Conn, Name) of
                {ok, ?BUS_RELEASE_NAME_REPLY_RELEASED} -> ok;
                {ok, ?BUS_RELEASE_NAME_REPLY_NON_EXISTENT} -> {error, not_found};
                {ok, ?BUS_RELEASE_NAME_REPLY_NOT_OWNER} -> {error, not_owner}
            end,
    {reply, Reply, State};
handle_call({add_match, Rule}, _From, State=#state{connection=Conn}) ->
    {reply, ebus_nif:add_match(Conn, Rule), State};

handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info({add_watch, Watch}, State=#state{watches=Watches}) ->
    {noreply, State#state{watches=[Watch | Watches]}};
handle_info({remove_watch, Watch}, State=#state{watches=Watches}) ->
    NewWatches = lists:filter(fun(W) -> ebus_nif:watch_equals(W, Watch) end,
                             Watches),
    {noreply, State#state{watches=NewWatches}};
handle_info(Msg, State=#state{}) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.


terminate(_Reason, #state{watches=Watches}) ->
    lists:foreach(fun(W) ->
                          enif_bus:watch_handle(W, ?BUS_WATCH_HANGUP)
                  end, Watches).
