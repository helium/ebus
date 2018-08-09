-module(ebus_nif).

-define(APPNAME, ebus).
-define(LIBNAME, 'ebus').

-on_load(init/0).

-export([bus/2, unique_name/1,
         request_name/3, release_name/2,
         add_match/2]).
-export([message_new_signal/3, message_new_call/4,
         message_append_args/3, message_get_args/1]).
-export([watch_equals/2, watch_handle/2]).

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

%%
%% Connection
%%

-spec bus(pos_integer(), pid()) -> ebus:connection().
bus(_,_) ->
    not_loaded(?LINE).

-spec unique_name(ebus:connection()) -> string().
unique_name(_) ->
    not_loaded(?LINE).

-spec add_match(ebus:connection(), Rule::string()) -> ok | {error, string()}.
add_match(_,_) ->
    not_loaded(?LINE).

-spec request_name(ebus:connection(), Name::string(), Flags::pos_integer())
                  -> {error, string()} | {ok, pos_integer()}.
request_name(_, _, _) ->
    not_loaded(?LINE).

-spec release_name(ebus:connection(), Name::string()) -> {ok, pos_integer()} | {error, string()}.
release_name(_, _) ->
    not_loaded(?LINE).


%%
%% Watches
%%

-spec watch_equals(ebus:watch(), ebus:watch()) -> boolean().
watch_equals(_,_) ->
    not_loaded(?LINE).

-spec watch_handle(ebus:watch(), non_neg_integer()) -> boolean().
watch_handle(_,_) ->
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
