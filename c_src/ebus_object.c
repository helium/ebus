#include "ebus_object.h"
#include "ebus_message.h"

ERL_NIF_TERM ATOM_HANDLE_MESSAGE;
ERL_NIF_TERM ATOM_HANDLE_REPLY;


static ErlNifResourceType * DBUS_OBJECT_RESOURCE;

dbus_object *
mk_dbus_object_resource(ErlNifEnv * env, ErlNifPid * pid)
{
    dbus_object * res = enif_alloc_resource(DBUS_OBJECT_RESOURCE, sizeof(dbus_object));
    res->pid          = *pid;
    return res;
}

void
cb_object_unregister(DBusConnection * connection, void * data)
{
    dbus_object * state = (dbus_object *)data;
    enif_release_resource(state);
}

DBusHandlerResult
cb_object_handle_message(DBusConnection * connection, DBusMessage * message, void * data)
{
    dbus_object * state = (dbus_object *)data;
    ErlNifEnv *  msg_env = enif_alloc_env();
    ERL_NIF_TERM msg =
        enif_make_tuple2(msg_env, ATOM_HANDLE_MESSAGE, mk_dbus_message(msg_env, message));
    dbus_message_ref(message);
    enif_send(NULL, &state->pid, msg_env, msg);
    enif_free_env(msg_env);

    return DBUS_HANDLER_RESULT_HANDLED;
}


void
cb_object_handle_reply(DBusPendingCall * pending, void * data)
{
    dbus_object * state = (dbus_object *)data;
    // Take the reply which should give us the ref for the returned
    // message
    DBusMessage * reply   = dbus_pending_call_steal_reply(pending);
    ErlNifEnv *   msg_env = enif_alloc_env();
    ERL_NIF_TERM  msg =
        enif_make_tuple2(msg_env, ATOM_HANDLE_REPLY, mk_dbus_message(msg_env, reply));
    enif_send(NULL, &state->pid, msg_env, msg);
    enif_free_env(msg_env);
    // let go of the pending call
    dbus_pending_call_unref(pending);
}

void
ebus_object_load(ErlNifEnv * env)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    DBUS_OBJECT_RESOURCE =
        enif_open_resource_type(env, NULL, "dbus_object", NULL, flags, NULL);

    ATOM(ATOM_HANDLE_MESSAGE, "handle_message");
    ATOM(ATOM_HANDLE_REPLY, "handle_reply");
}
