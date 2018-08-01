#include "ebus_shared.h"
#include "ebus_message.h"

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

static ERL_NIF_TERM
mk_dbus_message(ErlNifEnv * env, DBusMessage * msg)
{
    dbus_message * res =
        enif_alloc_resource(DBUS_MESSAGE_RESOURCE, sizeof(dbus_message));
    res->message          = msg;
    ERL_NIF_TERM res_term = enif_make_resource(env, res);
    enif_release_resource(res);
    return res_term;
}

static bool
get_dbus_message(ErlNifEnv * env, ERL_NIF_TERM term, DBusMessage ** dest)
{
    return enif_get_resource(env, term, DBUS_MESSAGE_RESOURCE, (void **)dest);
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

    DBusMessage *message = dbus_message_new_signal(path, iface, name);
    if (message == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    }

    return enif_make_tuple2(env, ATOM_OK, mk_dbus_message(env, message));
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
