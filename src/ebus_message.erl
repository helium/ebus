-module(ebus_message).

-export([new_call/3, new_call/4, new_reply/1, new_reply/3,
         new_signal/2, new_signal/3, append_args/3, args/1, to_map/1,
         type/1, serial/1, serial/2, reply_serial/1,
         destination/1, interface/1, path/1, member/1, error/1, infer_signature/1,
         interface_member/1]).

-spec new_signal(Path::string(), Member::string()) -> {ok, ebus:message()} | {error, term()}.
new_signal(Path, Member) ->
    {IFace, Name} = interface_member(Member),
    new_signal(Path, IFace, Name).

-spec new_signal(Path::string(), IFace::string(), Name::string()) -> {ok, ebus:message()} | {error, term()}.
new_signal(Path, IFace, Name) ->
    ebus_nif:message_new_signal(Path, IFace, Name).

-spec new_call(Dest::string(), Path::string(), Member::string()) -> {ok, ebus:message()} | {error, term()}.
new_call(Dest, Path, Member) ->
    {IFace, Name} = interface_member(Member),
    new_call(Dest, Path, IFace, Name).

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

-spec append_args(ebus:message(), ebus:signature(), [any()]) -> ok | {error, term()}.
append_args(Msg, Signature, Args) when length(Signature) == length(Args) ->
    ebus_nif:message_append_args(Msg, lists:flatten(encode_signature(Signature)), Args).

-spec args(ebus:message()) -> {ok, [any()]} | {error, string()}.
args(Msg) ->
    ebus_nif:message_get_args(Msg).

-spec type(ebus:message()) -> ebus:message_type().
type(Msg) ->
    ebus_nif:message_get_type(Msg).

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

-spec error(ebus:message()) -> ok | {error, term()}.
error(Msg) ->
    ebus_nif:message_get_error(Msg).

to_map(Msg) ->
    #{
      type => type(Msg),
      serial => serial(Msg),
      path => path(Msg),
      interface => interface(Msg),
      member => member(Msg)
     }.

-spec infer_signature(term()) -> ebus:signature().
infer_signature(Term) ->
    decode_signature(ebus_nif:message_infer_signature(Term)).

-spec interface_member(string()) -> {string() | undefined, string()}.
interface_member(Member) ->
    case string:split(Member, ".", trailing) of
        [Member] -> {undefined, Member};
        [Prefix, Suffix] -> {Prefix, Suffix}
    end.

%%
%% Private
%%

-spec encode_signature(ebus:signature()) -> string().
encode_signature(Signature) ->
    encode_signature(Signature, []).

-spec encode_signature(ebus:signature(), list()) -> string().
encode_signature([], Acc) ->
    lists:flatten(lists:reverse(Acc));
encode_signature([{array, SubType} | Rest], Acc) ->
    Entry = [$a, encode_signature([SubType], [])],
    encode_signature(Rest, [Entry | Acc]);
encode_signature([{dict, KeyType, ValueType} | Rest], Acc) ->
    Entry = [$a, ${, encode_type_code(KeyType), encode_signature([ValueType], []), $}],
    encode_signature(Rest, [Entry | Acc]);
encode_signature([{struct, SubTypes} | Rest], Acc) ->
    Entry = [$(,
             encode_signature(SubTypes, []),
             $)],
    encode_signature(Rest, [Entry | Acc]);
encode_signature([Type | Rest], Acc) ->
    Entry = encode_type_code(Type),
    encode_signature(Rest, [Entry | Acc]).

-spec encode_type_code(atom()) -> string().
encode_type_code(byte)        -> "y";
encode_type_code(bool)        -> "b";
encode_type_code(int16)       -> "n";
encode_type_code(uint16)      -> "q";
encode_type_code(int32)       -> "i";
encode_type_code(uint32)      -> "u";
encode_type_code(int64)       -> "x";
encode_type_code(uint64 )     -> "t";
encode_type_code(double)      -> "d";
encode_type_code(string)      -> "s";
encode_type_code(object_path) -> "o";
encode_type_code(signature)   -> "g";
encode_type_code(variant)     -> "v";
encode_type_code(T)           -> erlang:error({bad_type, T}).


-spec decode_signature(string()) -> ebus:signature().
decode_signature(Str) ->
    {Type, []} = decode_signature(Str, []),
    Type.

-spec decode_signature(string(), list()) -> {ebus:signature(), Rest::string()}.
decode_signature([], Acc) ->
    {lists:reverse(Acc), []};
decode_signature([$a, ${, KeySig | Rest], Acc) ->
    KeyType = decode_type_code(KeySig),
    {[ValueType], Rest2} = decode_signature(Rest, []),
    decode_signature(Rest2, [{dict, KeyType, ValueType} | Acc]);
decode_signature([$a | Rest], Acc) ->
    {SubType, Rest2} = decode_array_signature(Rest),
    decode_signature(Rest2, [{array, SubType} | Acc]);
decode_signature([$( | Rest], Acc) ->
    {Types, Rest2} = decode_signature(Rest, []),
    decode_signature(Rest2, [{struct, Types} | Acc]);
decode_signature([$) | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
decode_signature([$} | Rest], Acc) ->
    {Acc, Rest};
decode_signature([C | Rest], Acc) ->
    decode_signature(Rest, [decode_type_code(C) | Acc]).



-spec decode_array_signature(string()) -> {ebus:value_type(), Rest::string()}.
decode_array_signature([$a | Rest]) ->
    {[ValueType], Rest2} = decode_signature([$a | Rest], []),
    {ValueType, Rest2};
decode_array_signature([$( | Rest]) ->
    {Types, Rest2} = decode_signature(Rest, []),
    {{struct, Types}, Rest2};
decode_array_signature([C | Rest]) ->
    {decode_type_code(C), Rest}.

decode_type_code($y) -> byte;
decode_type_code($b) -> bool;
decode_type_code($n) -> int16;
decode_type_code($q) -> uint16;
decode_type_code($i) -> int32;
decode_type_code($u) -> uint32;
decode_type_code($x) -> int64;
decode_type_code($t) -> uint64;
decode_type_code($d) -> double;
decode_type_code($s) -> string;
decode_type_code($o) -> object_path;
decode_type_code($g) -> signature;
decode_type_code($v) -> variant;
decode_type_code(C) -> erlang:error({bad_type, C}).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_type_code_test() ->
    ?assertEqual("yb", encode_signature([byte, bool])),
    ?assertEqual("nq", encode_signature([int16, uint16])),
    ?assertEqual("iu", encode_signature([int32, uint32])),
    ?assertEqual("xt", encode_signature([int64, uint64])),
    ?assertEqual("d", encode_signature([double])),
    ?assertEqual("sog", encode_signature([string, object_path, signature])),
    ?assertEqual("v", encode_signature([variant])),

    ?assertError({bad_type, grenade}, encode_signature([grenade])),

    ok.

encode_signature_array_test() ->
    ?assertEqual("ai", encode_signature([{array, int32}])),
    ?assertEqual("aiau", encode_signature([{array, int32}, {array, uint32}])),
    ?assertEqual("a(sx)", encode_signature([{array, {struct, [string, int64]}}])),
    ?assertEqual("a{sd}", encode_signature([{dict, string, double}])),
    ?assertEqual("aav", encode_signature([{array, {array, variant}}])),

    ok.

encode_signature_struct_test() ->
    ?assertEqual("(iav)", encode_signature([{struct, [int32, {array, variant}]}])),

    ok.

decode_type_code_test() ->
    ?assertEqual([byte, bool], decode_signature("yb")),
    ?assertEqual([int16, uint16], decode_signature("nq")),
    ?assertEqual([int32, uint32], decode_signature("iu")),
    ?assertEqual([int64, uint64], decode_signature("xt")),
    ?assertEqual([double], decode_signature("d")),
    ?assertEqual([string, object_path, signature], decode_signature("sog")),
    ?assertEqual([variant], decode_signature("v")),

    ?assertError({bad_type, $z}, decode_signature("z")),

    ok.

decode_signature_array_test() ->
    ?assertEqual([{array, int32}], decode_signature("ai")),
    ?assertEqual([{array, int32}, {array, uint32}], decode_signature("aiau")),
    ?assertEqual([{array, {struct, [string, int64]}}], decode_signature("a(sx)")),
    ?assertEqual([{dict, string, double}], decode_signature("a{sd}")),
    ?assertEqual([{array, {array, variant}}], decode_signature("aav")),
    ok.

decode_signature_struct_test() ->
    ?assertEqual([{struct, [int32, {array, variant}]}], decode_signature("(iav)")),

    ok.

infer_signature_type_code_test() ->
    %% Basic types
    ?assertEqual([byte], infer_signature(32)),
    ?assertEqual([bool], infer_signature(true)),
    ?assertEqual([int16], infer_signature(-16#8000)),
    ?assertEqual([uint16], infer_signature(16#8000)),
    ?assertEqual([int32], infer_signature(-2147483648)),
    ?assertEqual([uint32], infer_signature(4294967295)),
    ?assertEqual([int64], infer_signature(-9223372036854775808)),
    ?assertEqual([uint64], infer_signature(16#ffffffffffffffff)),
    ?assertEqual([double], infer_signature(42.0)),
    ?assertEqual([string], infer_signature("hello")),

    ok.

infer_signature_array_test() ->
    ?assertEqual([string], infer_signature([32])),
    ?assertEqual([{array, byte}], infer_signature(<<32>>)),
    ?assertEqual([{array, string}], infer_signature(["string"])),
    ?assertEqual([{array, string}], infer_signature([[]])),
    ?assertEqual([{array, {struct, [byte, bool]}}], infer_signature([{32, true}])),
    ?assertEqual([{dict, string, double}], infer_signature(#{"hello" => 42.0})),
    ?assertEqual([{dict, variant, variant}], infer_signature(#{})),

    ok.

infer_struct_test() ->
    ?assertEqual([{struct, [byte, string]}], infer_signature({32, "hello"})),

    ok.

-endif.
