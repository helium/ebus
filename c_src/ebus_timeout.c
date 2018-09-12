#include "ebus_timeout.h"
#include "ebus_connection.h"
#include <stdio.h>

ERL_NIF_TERM ATOM_ADD_TIMEOUT;
ERL_NIF_TERM ATOM_REMOVE_TIMEOUT;

typedef struct
{
    DBusTimeout * timeout;
    unsigned long timeout_id;
} dbus_timeout;

static ErlNifResourceType * DBUS_TIMEOUT_RESOURCE;

static dbus_timeout *
mk_dbus_timeout_resource(ErlNifEnv * env, DBusTimeout * timeout)
{
    dbus_timeout * res = enif_alloc_resource(DBUS_TIMEOUT_RESOURCE, sizeof(dbus_timeout));
    res->timeout       = timeout;
    return res;
}

static bool
get_dbus_timeout_resource(ErlNifEnv * env, ERL_NIF_TERM term, dbus_timeout ** dest)
{
    return enif_get_resource(env, term, DBUS_TIMEOUT_RESOURCE, (void **)dest);
}

ERL_NIF_TERM
ebus_timeout_handle(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    dbus_timeout * res;
    if (!get_dbus_timeout_resource(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    // Ensre the the timeout was not already removed.
    if (res->timeout)
    {
        dbus_timeout_handle(res->timeout);
    }

    return ATOM_OK;
}

dbus_bool_t
cb_add_timeout(DBusTimeout * timeout, void * data)
{
    if (!dbus_timeout_get_enabled(timeout))
    {
        return TRUE;
    }
    dbus_connection * state = (dbus_connection *)data;
    dbus_timeout *    res   = mk_dbus_timeout_resource(state->env, timeout);
    res->timeout_id         = state->next_timeout_id++;

    int          ms      = dbus_timeout_get_interval(timeout);
    ErlNifEnv *  msg_env = enif_alloc_env();
    ERL_NIF_TERM msg     = enif_make_tuple4(msg_env,
                                        ATOM_ADD_TIMEOUT,
                                        enif_make_uint64(msg_env, res->timeout_id),
                                        enif_make_resource(msg_env, res),
                                        enif_make_uint(msg_env, ms));

    enif_send(NULL, &state->handler, msg_env, msg);
    enif_free_env(msg_env);
    // Keep a reference to the connection so the timeout can be cleaned
    // up on removal.
    enif_keep_resource(state);
    // Hang the created resource on timeout itself. Cleanup is done in
    // cb_remove_timer
    dbus_timeout_set_data(timeout, res, NULL);
    return TRUE;
}

void
cb_remove_timeout(DBusTimeout * timeout, void * data)
{
    dbus_connection * state = (dbus_connection *)data;
    dbus_timeout *    res   = (dbus_timeout *)dbus_timeout_get_data(timeout);
    if (!res || !state)
    {
        return;
    }

    ErlNifEnv *  msg_env = enif_alloc_env();
    ERL_NIF_TERM msg     = enif_make_tuple2(msg_env,
                                        ATOM_REMOVE_TIMEOUT,
                                        enif_make_uint64(msg_env, res->timeout_id));

    // Send the removal message to the handler. We safeguard the
    // handler dispatching a handle_timeout to a removed timer by
    // setting the timeout to NULL below.
    enif_send(NULL, &state->handler, msg_env, msg);
    enif_free_env(msg_env);

    // Release the timer resource and state
    res->timeout = NULL;
    enif_release_resource(res);
    enif_release_resource(state);
}

void
cb_toggle_timeout(DBusTimeout * timeout, void * data)
{
    if (dbus_timeout_get_enabled(timeout))
    {
        cb_add_timeout(timeout, data);
    }
    else
    {
        cb_remove_timeout(timeout, data);
    }
}


void
ebus_timeout_load(ErlNifEnv * env)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    DBUS_TIMEOUT_RESOURCE =
        enif_open_resource_type(env, NULL, "dbus_timeout", NULL, flags, NULL);

    ATOM(ATOM_ADD_TIMEOUT, "add_timeout");
    ATOM(ATOM_REMOVE_TIMEOUT, "remove_timeout");
}
