-module(ebus_nif).

-define(APPNAME, ebus).
-define(LIBNAME, 'ebus').

-on_load(init/0).

-export([connection_get/2, connection_close/1, connection_unique_name/1,
         connection_request_name/3, connection_release_name/2,
         connection_add_match/2, connection_send/2, connection_dispatch/1,
         connection_add_filter/2, connection_remove_filter/2]).
-export([message_new_signal/3, message_new_call/4,
         message_append_args/3, message_get_args/1, message_get_serial/1]).
-export([watch_handle/2]).
-export([timeout_handle/1]).

%%
%% Messages
%%

-spec message_new_signal(Path::string(), IFace::string(), Name::string()) -> {ok, ebus:message()}.
message_new_signal(_,_,_) ->
    not_loaded(?LINE).

-spec message_new_call(Dest::string(), Path::string(), IFace::string(), Name::string()) -> {ok, ebus:message()}.
message_new_call(_,_,_,_) ->
    not_loaded(?LINE).

-spec message_append_args(ebus:message(), Signature::string(), Args::[any()]) -> ok | {error, string()}.
message_append_args(_, _, _) ->
    not_loaded(?LINE).

-spec message_get_args(ebus:message()) -> {ok, [any()]} | {error, string()}.
message_get_args(_) ->
    not_loaded(?LINE).

-spec message_get_serial(ebus:message()) -> non_neg_integer().
message_get_serial(_) ->
    not_loaded(?LINE).

%%
%% Connection
%%

-spec connection_get(pos_integer(), pid()) -> {ok, ebus:connection()} | {error, string()}.
connection_get(_,_) ->
    not_loaded(?LINE).

-spec connection_close(ebus:connection()) -> ok.
connection_close(_) ->
    not_loaded(?LINE).

-spec connection_unique_name(ebus:connection()) -> string().
connection_unique_name(_) ->
    not_loaded(?LINE).

-spec connection_add_match(ebus:connection(), Rule::string()) -> ok | {error, string()}.
connection_add_match(_,_) ->
    not_loaded(?LINE).

-spec connection_request_name(ebus:connection(), Name::string(), Flags::pos_integer())
                             -> {error, string()} | {ok, pos_integer()}.
connection_request_name(_, _, _) ->
    not_loaded(?LINE).

-spec connection_release_name(ebus:connection(), Name::string()) -> {ok, pos_integer()} | {error, string()}.
connection_release_name(_, _) ->
    not_loaded(?LINE).

-spec connection_send(ebus:connection(), ebus:message()) -> non_neg_integer().
connection_send(_,_) ->
    not_loaded(?LINE).

-spec connection_dispatch(ebus:connection()) -> non_neg_integer().
connection_dispatch(_) ->
    not_loaded(?LINE).

-spec connection_add_filter(ebus:connection(), {Key::reference(), map()}) -> ok | {error, enomem}.
connection_add_filter(_,_) ->
    not_loaded(?LINE).

-spec connection_remove_filter(ebus:connection(), Key::reference()) -> ok.
connection_remove_filter(_,_) ->
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
