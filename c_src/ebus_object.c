#include "ebus_object.h"
#include "ebus_message.h"

ERL_NIF_TERM ATOM_HANDLE_MESSAGE;
ERL_NIF_TERM ATOM_HANDLE_REPLY;


static ErlNifResourceType * DBUS_OBJECT_RESOURCE;

dbus_object *
mk_dbus_object_resource(ErlNifEnv * env, ErlNifPid * pid, void * user_resource)
{
    dbus_object * res = enif_alloc_resource(DBUS_OBJECT_RESOURCE, sizeof(dbus_object));
    res->pid          = *pid;
    res->user         = user_resource;
    return res;
}

void
cb_object_unregister(DBusConnection * connection, void * data)
{
    dbus_object * object = (dbus_object *)data;
    enif_release_resource(object);
}


static void
cb_object_dtor(ErlNifEnv * env, void * data)
{
    dbus_object * object = (dbus_object *)data;
    if (object->user)
    {
        enif_release_resource(object->user);
        object->user = NULL;
    }
}

DBusHandlerResult
cb_object_handle_message(DBusConnection * connection, DBusMessage * message, void * data)
{
    dbus_object * state   = (dbus_object *)data;
    ErlNifEnv *   msg_env = enif_alloc_env();
    ERL_NIF_TERM  msg =
        enif_make_tuple2(msg_env, ATOM_HANDLE_MESSAGE, mk_dbus_message(msg_env, message));
    dbus_message_ref(message);
    enif_send(NULL, &state->pid, msg_env, msg);
    enif_free_env(msg_env);

    return DBUS_HANDLER_RESULT_HANDLED;
}


void
cb_object_handle_reply(DBusPendingCall * pending, void * data)
{
    dbus_object * object = (dbus_object *)data;
    // Take the reply which should give us the ref for the returned
    // message
    DBusMessage * reply   = dbus_pending_call_steal_reply(pending);
    ErlNifEnv *   msg_env = enif_alloc_env();
    ERL_NIF_TERM  msg =
        enif_make_tuple2(msg_env, ATOM_HANDLE_REPLY, mk_dbus_message(msg_env, reply));
    enif_send(NULL, &object->pid, msg_env, msg);

    enif_free_env(msg_env);

    // HACK_ATTACK: With both pending calls and standard messages, if
    // there is a timeout in a connection_call, the dbus library
    // messes up it's serial and serial_reply numbers. Just asking the
    // bus for anything else seems to clear it up. So here's a
    // terrible hack that checks for a no-reply and then calls
    // dbus_bus_get_id to get the library to line up again.
    DBusError error;
    dbus_error_init(&error);
    if (dbus_set_error_from_message(&error, reply)
        && dbus_error_has_name(&error, DBUS_ERROR_NO_REPLY))
    {
        dbus_connection * connection = (dbus_connection *)object->user;
        dbus_bus_get_id(connection->connection, NULL);
    }

    // let go of the pending call
    dbus_pending_call_unref(pending);
}

void
ebus_object_load(ErlNifEnv * env)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    DBUS_OBJECT_RESOURCE =
        enif_open_resource_type(env, NULL, "dbus_object", cb_object_dtor, flags, NULL);

    ATOM(ATOM_HANDLE_MESSAGE, "handle_message");
    ATOM(ATOM_HANDLE_REPLY, "handle_reply");
}
