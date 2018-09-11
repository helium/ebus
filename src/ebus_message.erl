-module(ebus_message).

-export([new_call/3, new_call/4, new_reply/1, new_reply/3,
         new_signal/3, append_args/3, args/1, to_map/1,
         serial/1, serial/2, reply_serial/1,
         destination/1, interface/1, path/1, member/1]).

-spec new_signal(Path::string(), IFace::string(), Name::string()) -> {ok, ebus:message()} | {error, term()}.
new_signal(Path, IFace, Name) ->
    ebus_nif:message_new_signal(Path, IFace, Name).

-spec new_call(Dest::string(), Path::string(), Name::string()) -> {ok, ebus:message()} | {error, term()}.
new_call(Dest, Path, Name) ->
    new_call(Dest, Path, undefined, Name).

-spec new_call(Dest::string() | undefined, Path::string(), IFace::string() | undefined, Name::string())
              -> {ok, ebus:message()} | {error, term()}.
new_call(Dest, Path, IFace, Name) ->
    MaybeStr = fun(undefined) -> "";
                  (Str) -> Str
               end,
    ebus_nif:message_new_call(MaybeStr(Dest), Path, MaybeStr(IFace), Name).

-spec new_reply(ebus:message()) -> {ok, ebus:message()} | {error, term()}.
new_reply(Msg) ->
    ebus_nif:message_new_reply(Msg).

-spec new_reply(ebus:message(), ebus:signature(), [any()]) -> {ok, ebus:message()} | {error, term()}.
new_reply(Msg, Types, Args) ->
    case new_reply(Msg)  of
        {ok, Reply} ->
           case ebus_message:append_args(Reply, Types, Args) of
               ok -> {ok, Reply};
               {error, Reason} -> {error, Reason}
           end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec append_args(ebus:message(), ebus:signature(), [any()]) -> ok | {error, string()}.
append_args(Msg, Signature, Args) when length(Signature) == length(Args) ->
    ebus_nif:message_append_args(Msg, lists:flatten(encode_signature(Signature)), Args).

-spec args(ebus:message()) -> {ok, [any()]} | {error, string()}.
args(Msg) ->
    ebus_nif:message_get_args(Msg).

-spec serial(ebus:message()) -> non_neg_integer().
serial(Msg) ->
    ebus_nif:message_get_serial(Msg).

-spec serial(ebus:message(), non_neg_integer()) -> ok.
serial(Msg, Value) ->
    ebus_nif:message_set_serial(Msg, Value).

-spec reply_serial(ebus:message()) -> non_neg_integer().
reply_serial(Msg) ->
    ebus_nif:message_get_reply_serial(Msg).

-spec interface(ebus:message()) -> string() | undefined.
interface(Msg) ->
    ebus_nif:message_get_interface(Msg).

-spec destination(ebus:message()) -> string() | undefined.
destination(Msg) ->
    ebus_nif:message_get_destination(Msg).

-spec path(ebus:message()) -> string() | undefined.
path(Msg) ->
    ebus_nif:message_get_path(Msg).

-spec member(ebus:message()) -> string() | undefined.
member(Msg) ->
    ebus_nif:message_get_member(Msg).

to_map(Msg) ->
    #{
      serial => serial(Msg),
      path => path(Msg),
      interface => interface(Msg),
      member => member(Msg)
     }.

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
