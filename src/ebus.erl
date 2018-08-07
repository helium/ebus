-module(ebus).

-on_load(init/0).

-export([bus/0, bus/1, unique_name/1,
         request_name/3, release_name/2,
         add_match/2,
         message_new_signal/3, message_new_call/4, message_append_args/3]).

-type connection() :: reference().
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
                    | object_path
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

-define(APPNAME, ebus).
-define(LIBNAME, 'ebus').

-define(BUS_SESSION, 0).
-define(BUS_SYSTEM,  1).
-define(BUS_STARTER, 2).

%% @doc Allow another service to become the primary owner if requested.
-define(BUS_NAME_FLAG_ALLOW_REPLACEMENT, 1).
%% @doc Request to replace the current primary owner.
-define(BUS_NAME_FLAG_REPLACE_EXISTING,  2).
%% @doc If we can not become the primary owner do not place us in the queue.
-define(BUS_NAME_FLAG_DO_NOT_QUEUE,      4).

%% @doc  Service has become the primary owner of the requested name.
-define(BUS_REQUEST_NAME_REPLY_PRIMARY_OWNER,  1).
%% @doc  Service could not become the primary owner and has been placed in the queue.
-define(BUS_REQUEST_NAME_REPLY_IN_QUEUE,       2).
%% @doc Service is already in the queue.
-define(BUS_REQUEST_NAME_REPLY_EXISTS,         3).
%% @doc Service is already the primary owner.
-define(BUS_REQUEST_NAME_REPLY_ALREADY_OWNER,  4).

%% @doc Service was released from the given name.
-define(BUS_RELEASE_NAME_REPLY_RELEASED,       1).
%% @doc The given name does not exist on the bus.
-define(BUS_RELEASE_NAME_REPLY_NON_EXISTENT,   2).
%% @doc Service is not an owner of the given name.
-define(BUS_RELEASE_NAME_REPLY_NOT_OWNER,      3).

-spec bus() -> connection().
bus() ->
    bus(starter).

-spec bus(bus_type()) -> connection().
bus(Type) ->
    IntType = case Type of
                  session -> ?BUS_SESSION;
                  system -> ?BUS_SYSTEM;
                  starter -> ?BUS_STARTER;
                  _ -> error(badarg)
              end,
    int_bus(IntType).

-spec unique_name(connection()) -> string().
unique_name(_) ->
    not_loaded(?LINE).

-spec request_name(connection(), Name::string(), Flags::request_name_opts()) -> request_name_reply().
request_name(Conn, Name, Opts) when is_list(Opts) ->
    Flags = lists:foldl(fun({replace_existing, true}, Acc) -> Acc bor ?BUS_NAME_FLAG_REPLACE_EXISTING;
                           ({allow_replacement, true}, Acc) -> Acc bor ?BUS_NAME_FLAG_ALLOW_REPLACEMENT;
                           ({do_not_queue, true}, Acc) -> Acc bor ?BUS_NAME_FLAG_DO_NOT_QUEUE
                        end, 0, Opts),
    case int_request_name(Conn, Name, Flags) of
        {ok, ?BUS_REQUEST_NAME_REPLY_PRIMARY_OWNER} -> ok;
        {ok, ?BUS_REQUEST_NAME_REPLY_IN_QUEUE} -> {error, queued};
        {ok, ?BUS_REQUEST_NAME_REPLY_EXISTS} -> {error, queued};
        {ok, ?BUS_REQUEST_NAME_REPLY_ALREADY_OWNER} -> ok;
        {error, Msg} -> {error, Msg}
    end.


-spec release_name(connection(), Name::string()) -> release_name_reply().
release_name(Conn, Name) ->
    case int_release_name(Conn, Name) of
        {ok, ?BUS_RELEASE_NAME_REPLY_RELEASED} -> ok;
        {ok, ?BUS_RELEASE_NAME_REPLY_NON_EXISTENT} -> {error, not_found};
        {ok, ?BUS_RELEASE_NAME_REPLY_NOT_OWNER} -> {error, not_owner}
    end.


-spec add_match(connection(), Rule::string()) -> ok | {error, string()}.
add_match(_,_) ->
    not_loaded(?LINE).

-spec message_new_signal(Path::string(), IFace::string(), Name::string()) -> {ok, message()}.
message_new_signal(_,_,_) ->
    not_loaded(?LINE).

-spec message_new_call(Dest::string(), Path::string(), IFace::string(), Name::string()) -> {ok, message()}.
message_new_call(_,_,_,_) ->
    not_loaded(?LINE).

-spec message_append_args(message(), Signature::string(), Args::[any()]) -> ok | {error, string()}.
message_append_args(Msg, Signature, Args) ->
    int_message_append_args(Msg, lists:flatten(encode_signature(Signature)), Args).

%%
%% Private
%%

-spec int_message_append_args(message(), Signature::string(), Args::[any()]) -> ok | {error, string()}.
int_message_append_args(_, _, _) ->
    not_loaded(?LINE).

-spec int_bus(pos_integer()) -> connection().
int_bus(_) ->
    not_loaded(?LINE).

-spec int_request_name(connection(), Name::string(), Flags::pos_integer())
                      -> {error, string()} | {ok, pos_integer()}.
int_request_name(_, _, _) ->
    not_loaded(?LINE).


-spec int_release_name(connection(), Name::string()) -> {ok, pos_integer()} | {error, string()}.
int_release_name(_, _) ->
    not_loaded(?LINE).

-spec encode_signature(dbus:signature()) -> iolist().
encode_signature(byte)                       -> "y";
encode_signature(bool)                       -> "b";
encode_signature(int16)                      -> "n";
encode_signature(uint16)                     -> "q";
encode_signature(int32)                      -> "i";
encode_signature(uint32)                     -> "u";
encode_signature(int64)                      -> "x";
encode_signature(uint64 )                    -> "t";
encode_signature(double)                     -> "d";
encode_signature(string)                     -> "s";
encode_signature(object_path)                -> "o";
encode_signature(signature)                  -> "g";
encode_signature({array, SubType})           -> [$a, encode_signature(SubType)];
encode_signature(variant)                    -> "v";
encode_signature({dict, KeyType, ValueType}) -> ["a{", encode_signature(KeyType), encode_signature(ValueType), "}"];
encode_signature({struct, SubTypes})         -> ["(", [encode_signature(T) || T <- SubTypes], ")"];
encode_signature(Type) when is_list(Type)    -> [encode_signature(T) || T <- Type].

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
