#include "ebus_message.h"
#include "ebus_error.h"
#include "ebus_message_append_arg.h"
#include "ebus_message_list_args.h"

ERL_NIF_TERM ATOM_CALL;
ERL_NIF_TERM ATOM_SIGNAL;
ERL_NIF_TERM ATOM_REPLY;

static ErlNifResourceType * DBUS_MESSAGE_RESOURCE;

typedef struct
{
    DBusMessage * message;
} dbus_message;


static void
dbus_message_dtor(ErlNifEnv * env, void * obj)
{
    (void)env;
    dbus_message * msg = (dbus_message *)obj;
    dbus_message_unref(msg->message);
}

ERL_NIF_TERM
mk_dbus_message(ErlNifEnv * env, DBusMessage * msg)
{
    dbus_message * res = enif_alloc_resource(DBUS_MESSAGE_RESOURCE, sizeof(dbus_message));
    res->message       = msg;
    ERL_NIF_TERM res_term = enif_make_resource(env, res);
    enif_release_resource(res);
    return res_term;
}

bool
get_dbus_message(ErlNifEnv * env, ERL_NIF_TERM term, DBusMessage ** dest)
{
    dbus_message * msg;
    if (!enif_get_resource(env, term, DBUS_MESSAGE_RESOURCE, (void **)&msg))
    {
        return false;
    }
    *dest = msg->message;
    return true;
}

int
get_dbus_message_type(ErlNifEnv * env, ERL_NIF_TERM term, unsigned int * dest)
{
    unsigned int result = DBUS_MESSAGE_TYPE_INVALID;
    if (term == ATOM_ERROR)
        result = DBUS_MESSAGE_TYPE_ERROR;
    else if (term == ATOM_CALL)
        result = DBUS_MESSAGE_TYPE_METHOD_CALL;
    else if (term == ATOM_SIGNAL)
        result = DBUS_MESSAGE_TYPE_SIGNAL;
    else if (term == ATOM_REPLY)
        result = DBUS_MESSAGE_TYPE_METHOD_RETURN;

    *dest = result;
    return result != DBUS_MESSAGE_TYPE_INVALID;
}

static ERL_NIF_TERM
mk_dbus_message_type(ErlNifEnv * env, int type)
{
    switch (type)
    {
    case DBUS_MESSAGE_TYPE_ERROR:
        return ATOM_ERROR;
    case DBUS_MESSAGE_TYPE_METHOD_CALL:
        return ATOM_CALL;
    case DBUS_MESSAGE_TYPE_SIGNAL:
        return ATOM_SIGNAL;
    case DBUS_MESSAGE_TYPE_METHOD_RETURN:
        return ATOM_REPLY;
    default:
        return ATOM_UNDEFINED;
    }
}

ERL_NIF_TERM
ebus_message_new_signal(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 3)
    {
        return enif_make_badarg(env);
    }

    GET_STR(path, argv[0]);
    if (!dbus_validate_path(path, NULL))
    {
        return enif_make_badarg(env);
    }

    GET_STR(iface, argv[1]);
    if (!dbus_validate_interface(iface, NULL))
    {
        return enif_make_badarg(env);
    }

    GET_STR(name, argv[2]);
    if (!dbus_validate_member(name, NULL))
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message = dbus_message_new_signal(path, iface, name);
    if (message == NULL)
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    return enif_make_tuple2(env, ATOM_OK, mk_dbus_message(env, message));
}


ERL_NIF_TERM
ebus_message_new_call(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 4)
    {
        return enif_make_badarg(env);
    }

    GET_STR(maybe_destination, argv[0]);
    const char * destination = maybe_destination;

    if (maybe_destination_len == 0)
    {
        destination = NULL;
    }
    else if (!dbus_validate_bus_name(maybe_destination, NULL))
    {
        return enif_make_badarg(env);
    }

    GET_STR(path, argv[1]);
    if (!dbus_validate_path(path, NULL))
    {
        return enif_make_badarg(env);
    }

    GET_STR(maybe_iface, argv[2]);
    const char * iface = maybe_iface;
    if (maybe_iface_len == 0)
    {
        iface = NULL;
    }
    else if (!dbus_validate_interface(maybe_iface, NULL))
    {
        return enif_make_badarg(env);
    }

    GET_STR(method, argv[3]);
    if (!dbus_validate_member(method, NULL))
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message = dbus_message_new_method_call(destination, path, iface, method);
    if (message == NULL)
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    return enif_make_tuple2(env, ATOM_OK, mk_dbus_message(env, message));
}


ERL_NIF_TERM
ebus_message_new_reply(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message;
    if (!get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    DBusMessage * reply = dbus_message_new_method_return(message);
    if (!reply)
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    return enif_make_tuple2(env, ATOM_OK, mk_dbus_message(env, reply));
}


ERL_NIF_TERM
ebus_message_append_args(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 3)
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message;
    if (!get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    GET_STR(signature, argv[1]);
    if (!dbus_signature_validate(signature, NULL))
    {
        return enif_make_badarg(env);
    }

    if (!enif_is_list(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    if (signature[0] == '\0')
    {
        return ATOM_OK;
    }

    DBusSignatureIter sig_iter;
    dbus_signature_iter_init(&sig_iter, signature);

    DBusMessageIter appender;
    dbus_message_iter_init_append(message, &appender);

    dbus_bool_t  more     = TRUE;
    ERL_NIF_TERM arg_list = argv[2];
    ERL_NIF_TERM arg;
    while (more)
    {
        if (!enif_get_list_cell(env, arg_list, &arg, &arg_list))
        {
            // {error, not_enough_args}
            return enif_make_badarg(env);
        }
        if (!ebus_message_append_arg(env, arg, &appender, &sig_iter, &more))
        {
            // {error, enomem}
            return enif_make_badarg(env);
        }
    }

    unsigned arg_len;
    enif_get_list_length(env, arg_list, &arg_len);
    if (arg_len > 0)
    {
        // {error,too_many_args}
        return enif_make_badarg(env);
    }

    return ATOM_OK;
}

ERL_NIF_TERM
ebus_message_get_args(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message;
    if (!get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    return ebus_message_list_args(env, message);
}

ERL_NIF_TERM
ebus_message_get_type(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message;
    if (!get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    return mk_dbus_message_type(env, dbus_message_get_type(message));
}

ERL_NIF_TERM
ebus_message_get_serial(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message;
    if (!get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, dbus_message_get_serial(message));
}

ERL_NIF_TERM
ebus_message_set_serial(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message;
    if (!get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    unsigned int val;
    enif_get_uint(env, argv[1], &val);

    dbus_message_set_serial(message, val);

    return ATOM_OK;
}

ERL_NIF_TERM
ebus_message_get_reply_serial(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message;
    if (!get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, dbus_message_get_reply_serial(message));
}


static ERL_NIF_TERM
mk_str_maybe(ErlNifEnv * env, const char * str)
{
    if (str)
    {
        return enif_make_string(env, str, ERL_NIF_LATIN1);
    }
    else
    {
        return ATOM_UNDEFINED;
    }
}

ERL_NIF_TERM
ebus_message_get_destination(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    DBusMessage * message;
    if (argc != 1 || !get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    const char * str = dbus_message_get_destination(message);
    return mk_str_maybe(env, str);
}

ERL_NIF_TERM
ebus_message_get_path(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    DBusMessage * message;
    if (argc != 1 || !get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    const char * str = dbus_message_get_path(message);
    return mk_str_maybe(env, str);
}

ERL_NIF_TERM
ebus_message_get_interface(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    DBusMessage * message;
    if (argc != 1 || !get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    const char * str = dbus_message_get_interface(message);
    return mk_str_maybe(env, str);
}

ERL_NIF_TERM
ebus_message_get_member(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    DBusMessage * message;
    if (argc != 1 || !get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    const char * str = dbus_message_get_member(message);
    return mk_str_maybe(env, str);
}

ERL_NIF_TERM
ebus_message_get_error(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    DBusMessage * message;
    if (argc != 1 || !get_dbus_message(env, argv[0], &message))
    {
        return enif_make_badarg(env);
    }

    DBusError error;
    dbus_error_init(&error);
    if (!dbus_set_error_from_message(&error, message))
    {
        return ATOM_OK;
    }

    return handle_dbus_error(env, &error);
}

ERL_NIF_TERM
ebus_message_infer_signature(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    char sig[DBUS_MAXIMUM_SIGNATURE_LENGTH];
    sig[0]          = DBUS_TYPE_INVALID;
    size_t sig_size = DBUS_MAXIMUM_SIGNATURE_LENGTH;

    if (!ebus_message_infer_type(env, argv[0], sig, &sig_size))
    {
        return enif_make_badarg(env);
    }

    return enif_make_string(env, sig, ERL_NIF_LATIN1);
}

void
ebus_message_load(ErlNifEnv * env)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    DBUS_MESSAGE_RESOURCE =
        enif_open_resource_type(env, NULL, "dbus_message", dbus_message_dtor, flags, NULL);

    ATOM(ATOM_CALL, "call");
    ATOM(ATOM_SIGNAL, "signal");
    ATOM(ATOM_REPLY, "reply");
}
