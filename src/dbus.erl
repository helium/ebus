-module(dbus).

-include_lib("dbus.hrl").

-on_load(init/0).

-export([bus/0, bus/1, unique_name/1,
         request_name/3, release_name/2,
         add_match/2,
         message_new_signal/3]).

-type dbus() :: reference().
-type dbus_message() :: reference().
-type dbus_type() :: ?BUS_SESSION | ?BUS_SYSTEM | ?BUS_STARTER.
-type request_name_flags() :: pos_integer().
-type request_name_reply() :: ?BUS_REQUEST_NAME_REPLY_PRIMARY_OWNER
                              | ?BUS_REQUEST_NAME_REPLY_IN_QUEUE
                              | ?BUS_REQUEST_NAME_REPLY_EXISTS
                              | ?BUS_REQUEST_NAME_REPLY_ALREADY_OWNER.
-type release_name_reply() :: ?BUS_RELEASE_NAME_REPLY_RELEASED
                              | ?BUS_RELEASE_NAME_REPLY_NON_EXISTENT
                              | ?BUS_RELEASE_NAME_REPLY_NOT_OWNER.
-export_type([dbus/0, dbus_type/0,
              request_name_flags/0, request_name_reply/0,
              release_name_reply/0]).

-define(APPNAME, dbus).
-define(LIBNAME, 'dbus').

-spec bus() -> dbus().
bus() ->
    bus(?BUS_STARTER).

-spec bus(dbus_type()) -> dbus().
bus(_) ->
    not_loaded(?LINE).

-spec unique_name(dbus()) -> string().
unique_name(_) ->
    not_loaded(?LINE).

-spec request_name(dbus(), Name::string(), Flags::request_name_flags())
                  -> {error, string()} | {ok, request_name_reply()}.
request_name(_, _, _) ->
    not_loaded(?LINE).

-spec release_name(dbus(), Name::string()) -> {error, string()} | {ok, release_name_reply()}.
release_name(_, _) ->
    not_loaded(?LINE).

-spec add_match(dbus(), Rule::string()) -> ok | {error, string()}.
add_match(_, _) ->
    not_loaded(?LINE).

-spec message_new_signal(Path::string(), IFace::string(), Name::string()) -> {ok, dbus_message()}.
message_new_signal(_,_,_) ->
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
