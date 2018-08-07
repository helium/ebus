-module(dbus_message).

-export([new_call/4, new_signal/3, append_args/3]).

-spec new_signal(Path::string(), IFace::string(), Name::string()) -> {ok, dbus:message()}.
new_signal(Path, IFace, Name) ->
    dbus:message_new_signal(Path, IFace, Name).

-spec new_call(Dest::string(), Path::string(), IFace::string(), Name::string()) -> {ok, dbus:message()}.
new_call(Dest, Path, IFace, Name) ->
    dbus:message_new_call(Dest, Path, IFace, Name).

-spec append_args(dbus:message(), dbus:signature(), [any()]) -> ok | {error, string()}.
append_args(Msg, Signature, Args) when length(Signature) == length(Args) ->
    dbus:message_append_args(Msg, Signature, Args).



%% infer_type(V) when is_binary(V)          -> {array, byte};
%% infer_type(true)                         -> boolean;
%% infer_type(false)                        -> boolean;
%% infer_type(V) when is_integer(V), V < 0  -> infer_int(V);
%% infer_type(V) when is_integer(V), V >= 0 -> infer_uint(V);
%% infer_type(V) when is_tuple(V)           -> {struct, [infer_type(T) || T <- tuple_to_list(V)]};
%% infer_type(V) when is_atom(V)            -> string;
%% infer_type(V) when is_list(V)            -> string;
%% infer_type(V) when is_map(V)             -> {dict, variant, variant}.

%% infer_int(V) when V >= -32767     -> int16;
%% infer_int(V) when V >= -214748364 -> int32;
%% infer_int(_V)                     -> int64.

%% infer_uint(V) when V < 32768      -> uint16;
%% infer_uint(V) when V < 4294967296 -> uint32;
%% infer_uint(_V)                    -> uint64.

%% encode_arg(T=byte, V) when is_integer(V)                             -> {encode_type(T), V};
%% encode_arg(T=bool, V) when V == true orelse V == false               -> {encode_type(T), V};
%% encode_arg(T=int16, V) when V > -32767 andalso V =< 32767            -> {encode_type(T), V};
%% encode_arg(T=uint16, V)when V >= 0 andalso V =< 65535                -> {encode_type(T), V};
%% encode_arg(T=int32, V) when V >= -2147483647 andalso V =< 2147483647 -> {encode_type(T), V};
%% encode_arg(T=uint32, V) when V >= 0 andalso V =< 4294967295          -> {encode_type(T), V};
%% encode_arg(T=int64, V)                                               -> {encode_type(T), V};
%% encode_arg(T=uint64, V ) when V >= 0                                 -> {encode_type(T), V};
%% encode_arg(T=double, V) when is_integer(V)                           -> {encode_type(T), float(V)};
%% encode_arg(T=double, V) when is_number(V)                            -> {encode_type(T), V};
%% encode_arg(T=string, V) when is_atom(V)                              -> {encode_type(T), atom_to_list(V)};
%% encode_arg(T=string, V) when is_list(V)                              -> {encode_type(T), V};
%% encode_arg(T=object_path, V) when is_list(V)                         -> {encode_type(T), V};
%% encode_arg(T=signature, V) when is_list(V)                           -> {encode_type(T), V};
%% encode_arg(T={array, _}, V) when is_list(V)                          -> {encode_type(T), V};
%% encode_arg(T=variant, V)                                             -> {encode_type(T), V};
%% encode_arg(T={struct, _}, V) when is_tuple(V)                        -> {encode_type(T), tuple_to_list(V)};
%% encode_arg(T={struct, _}, V) when is_list(V)                         -> {encode_type(T), V}.
