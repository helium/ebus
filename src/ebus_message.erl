-module(ebus_message).

-export([new_call/4, new_signal/3, append_args/3, get_args/1]).

-spec new_signal(Path::string(), IFace::string(), Name::string()) -> {ok, ebus:message()}.
new_signal(Path, IFace, Name) ->
    ebus:message_new_signal(Path, IFace, Name).

-spec new_call(Dest::string(), Path::string(), IFace::string(), Name::string()) -> {ok, ebus:message()}.
new_call(Dest, Path, IFace, Name) ->
    ebus:message_new_call(Dest, Path, IFace, Name).

-spec append_args(ebus:message(), ebus:signature(), [any()]) -> ok | {error, string()}.
append_args(Msg, Signature, Args) when length(Signature) == length(Args) ->
    ebus:message_append_args(Msg, Signature, Args).

-spec get_args(ebus:message()) -> {ok, [any()]} | {error, string()}.
get_args(Msg) ->
    ebus:message_get_args(Msg).
