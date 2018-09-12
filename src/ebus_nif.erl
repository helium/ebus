-module(ebus_nif).

-define(APPNAME, ebus).
-define(LIBNAME, 'ebus').

-on_load(init/0).

-export([connection_get/2, connection_close/1, connection_unique_name/1,
         connection_request_name/3, connection_release_name/2,
         connection_add_match/2, connection_send/2, connection_call/4, connection_dispatch/1,
         connection_set_filters/2,
         connection_register_object_path/3, connection_unregister_object_path/2]).
-export([message_new_signal/3, message_new_call/4, message_new_reply/1,
         message_append_args/3, message_get_args/1, message_get_type/1,
         message_get_serial/1, message_set_serial/2, message_get_reply_serial/1,
         message_get_destination/1, message_get_path/1, message_get_interface/1, message_get_member/1,
         message_get_error/1]).
-export([watch_handle/2]).
-export([timeout_handle/1]).

%%
%% Messages
%%

-spec message_new_signal(Path::string(), IFace::string(), Name::string())
                        -> {ok, ebus:message()} | {error, term()}.
message_new_signal(_,_,_) ->
    not_loaded(?LINE).

-spec message_new_call(Dest::string(), Path::string(), IFace::string(), Name::string())
                      -> {ok, ebus:message()} | {error, term()}.
message_new_call(_,_,_,_) ->
    not_loaded(?LINE).

-spec message_new_reply(ebus:message()) -> {ok, ebus:message()} | {error, term()}.
message_new_reply(_) ->
    not_loaded(?LINE).

-spec message_append_args(ebus:message(), Signature::string(), Args::[any()]) -> ok | {error, string()}.
message_append_args(_, _, _) ->
    not_loaded(?LINE).

-spec message_get_args(ebus:message()) -> {ok, [any()]} | {error, term()}.
message_get_args(_) ->
    not_loaded(?LINE).

-spec message_get_type(ebus:message()) -> ebus:message_type().
message_get_type(_) ->
    not_loaded(?LINE).

-spec message_get_serial(ebus:message()) -> non_neg_integer().
message_get_serial(_) ->
    not_loaded(?LINE).

-spec message_set_serial(ebus:message(), non_neg_integer()) ->ok.
message_set_serial(_, _) ->
    not_loaded(?LINE).

-spec message_get_reply_serial(ebus:message()) -> non_neg_integer().
message_get_reply_serial(_) ->
    not_loaded(?LINE).

-spec message_get_destination(ebus:message()) -> string() | undefined.
message_get_destination(_) ->
    not_loaded(?LINE).

-spec message_get_path(ebus:message()) -> string() | undefined.
message_get_path(_) ->
    not_loaded(?LINE).

-spec message_get_interface(ebus:message()) -> string() | undefined.
message_get_interface(_) ->
    not_loaded(?LINE).

-spec message_get_member(ebus:message()) -> string() | undefined.
message_get_member(_) ->
    not_loaded(?LINE).

-spec message_get_error(ebus:message()) -> ok | {error, term()}.
message_get_error(_) ->
    not_loaded(?LINE).

%%
%% Connection
%%

-spec connection_get(pos_integer(), pid()) -> {ok, ebus:connection()} | {error, term()}.
connection_get(_,_) ->
    not_loaded(?LINE).

-spec connection_close(ebus:connection()) -> ok.
connection_close(_) ->
    not_loaded(?LINE).

-spec connection_unique_name(ebus:connection()) -> string().
connection_unique_name(_) ->
    not_loaded(?LINE).

-spec connection_add_match(ebus:connection(), Rule::string()) -> ok | {error, term()}.
connection_add_match(_,_) ->
    not_loaded(?LINE).

-spec connection_request_name(ebus:connection(), Name::string(), Flags::pos_integer())
                             -> {error, term()} | {ok, pos_integer()}.
connection_request_name(_, _, _) ->
    not_loaded(?LINE).

-spec connection_release_name(ebus:connection(), Name::string()) -> {ok, pos_integer()} | {error, term()}.
connection_release_name(_, _) ->
    not_loaded(?LINE).

-spec connection_send(ebus:connection(), ebus:message()) -> ok | {error, enomem}.
connection_send(_,_) ->
    not_loaded(?LINE).

-spec connection_call(ebus:connection(), ebus:message(), pid(), integer()) -> ok | {error, enomem}.
connection_call(_,_,_,_) ->
    not_loaded(?LINE).

-spec connection_dispatch(ebus:connection()) -> non_neg_integer().
connection_dispatch(_) ->
    not_loaded(?LINE).

-spec connection_set_filters(ebus:connection(), [{Ref::reference(), ebus:filter()}]) -> ok | {error, enomem}.
connection_set_filters(_,_) ->
    not_loaded(?LINE).

-spec connection_register_object_path(ebus:connection(), string(), pid()) -> ok | {error, enomem | already}.
connection_register_object_path(_,_,_) ->
    not_loaded(?LINE).

-spec connection_unregister_object_path(ebus:connection(), string()) -> ok | {error, enomem | already}.
connection_unregister_object_path(_,_) ->
    not_loaded(?LINE).


%%
%% Watches
%%

-spec watch_handle(ebus:watch(), non_neg_integer()) -> boolean().
watch_handle(_,_) ->
    not_loaded(?LINE).

%%
%% Timeout
%%

-spec timeout_handle(reference()) -> ok.
timeout_handle(_) ->
    not_loaded(?LINE).

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
