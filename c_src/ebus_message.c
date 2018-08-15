#include "ebus_message.h"
#include "ebus_message_append_arg.h"
#include "ebus_message_list_args.h"

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
    dbus_message * res =
        enif_alloc_resource(DBUS_MESSAGE_RESOURCE, sizeof(dbus_message));
    res->message          = msg;
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

    GET_STR(destination, argv[0]);
    if (!dbus_validate_bus_name(destination, NULL))
    {
        return enif_make_badarg(env);
    }

    GET_STR(path, argv[1]);
    if (!dbus_validate_path(path, NULL))
    {
        return enif_make_badarg(env);
    }

    GET_STR(iface, argv[2]);
    if (!dbus_validate_interface(iface, NULL))
    {
        return enif_make_badarg(env);
    }

    GET_STR(method, argv[3]);
    if (!dbus_validate_member(method, NULL))
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message =
        dbus_message_new_method_call(destination, path, iface, method);
    if (message == NULL)
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    return enif_make_tuple2(env, ATOM_OK, mk_dbus_message(env, message));
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

void
ebus_message_load(ErlNifEnv * env)
{
    int flags             = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    DBUS_MESSAGE_RESOURCE = enif_open_resource_type(env,
                                                    NULL,
                                                    "dbus_message",
                                                    dbus_message_dtor,
                                                    flags,
                                                    NULL);
}
