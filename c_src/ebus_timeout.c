#include "ebus_timeout.h"
#include "ebus_connection.h"
#include <stdio.h>

ERL_NIF_TERM ATOM_ADD_TIMEOUT;
ERL_NIF_TERM ATOM_REMOVE_TIMEOUT;

extern DBusTimeout *
_dbus_timeout_ref(DBusTimeout * watch);
extern void
_dbus_timeout_unref(DBusTimeout * watch);

typedef struct
{
    DBusTimeout * timeout;
    ERL_NIF_TERM  timeout_ref;
} dbus_timeout;

static ErlNifResourceType * DBUS_TIMEOUT_RESOURCE;

static void
dbus_timeout_dtor(ErlNifEnv * env, void * obj)
{
    (void)env;
    dbus_timeout * res = (dbus_timeout *)obj;
    printf("UNREFFING TIMEOUT\n");
    _dbus_timeout_unref(res->timeout);
    printf("UNREFFED TIMEOUT\n");
}

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

    if (res)
    {
        printf("HANDLE TIMEOUT %p!\n", res->timeout);
        dbus_timeout_handle(res->timeout);
    }

    return ATOM_OK;
}

dbus_bool_t
cb_add_timeout(DBusTimeout * timeout, void * data)
{
    printf("ADD TIMEOUT %p!\n", timeout);
    if (!dbus_timeout_get_enabled(timeout))
    {
        return TRUE;
    }
    dbus_connection * state = (dbus_connection *)data;
    dbus_timeout *    res   = mk_dbus_timeout_resource(state->env, timeout);

    int          ms  = dbus_timeout_get_interval(timeout);
    ERL_NIF_TERM ref = enif_make_resource(state->env, res);
    ERL_NIF_TERM msg =
        enif_make_tuple3(state->env, ATOM_ADD_TIMEOUT, ref, enif_make_int(state->env, ms));

    enif_send(state->env, &state->handler, NULL, msg);
    // Keep a reference to the connection so the timeout can be cleaned
    // up on removal.
    enif_keep_resource(state);
    // Hang the created resource onto to timeout itself. Cleanup is done
    // in cb_remove_timer
    res->timeout_ref = ref;
    dbus_timeout_set_data(timeout, res, NULL);

    printf("ADDED TIMEOUT\n");
    return TRUE;
}

void
cb_remove_timeout(DBusTimeout * timeout, void * data)
{
    printf("REMOVE TIMEOUT %p!\n", timeout);

    dbus_connection * state = (dbus_connection *)data;
    dbus_timeout *    res   = (dbus_timeout *)dbus_timeout_get_data(timeout);

    ERL_NIF_TERM ref = res->timeout_ref;
    ERL_NIF_TERM msg = enif_make_tuple2(state->env, ATOM_REMOVE_TIMEOUT, ref);

    enif_send(state->env, &state->handler, NULL, msg);

    // Release the timer resource and state
    enif_release_resource(res);
    enif_release_resource(state);
    printf("REMOVED TIMEOUT!\n");
}

void
cb_toggle_timeout(DBusTimeout * timeout, void * data)
{
    printf("TOGGLE TIMEOUT %p!\n", timeout);

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
        enif_open_resource_type(env, NULL, "dbus_timeout", dbus_timeout_dtor, flags, NULL);

    ATOM(ATOM_ADD_TIMEOUT, "add_timeout");
    ATOM(ATOM_REMOVE_TIMEOUT, "remove_timeout");
}
