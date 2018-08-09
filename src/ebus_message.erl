-module(ebus_message).

-export([new_call/4, new_signal/3, append_args/3, get_args/1]).

-spec new_signal(Path::string(), IFace::string(), Name::string()) -> {ok, ebus:message()}.
new_signal(Path, IFace, Name) ->
    ebus_nif:message_new_signal(Path, IFace, Name).

-spec new_call(Dest::string(), Path::string(), IFace::string(), Name::string()) -> {ok, ebus:message()}.
new_call(Dest, Path, IFace, Name) ->
    ebus_nif:message_new_call(Dest, Path, IFace, Name).

-spec append_args(ebus:message(), ebus:signature(), [any()]) -> ok | {error, string()}.
append_args(Msg, Signature, Args) when length(Signature) == length(Args) ->
    ebus_nif:message_append_args(Msg, lists:flatten(encode_signature(Signature)), Args).

-spec get_args(ebus:message()) -> {ok, [any()]} | {error, string()}.
get_args(Msg) ->
    ebus_nif:message_get_args(Msg).

%%
%% Private
%%

-spec encode_signature(ebus:signature()) -> iolist().
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
