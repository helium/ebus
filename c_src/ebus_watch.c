#include "ebus_watch.h"
#include "ebus_connection.h"
#include <stdio.h>

typedef struct
{
    DBusWatch * watch;
} dbus_watch;

static ErlNifResourceType * DBUS_WATCH_RESOURCE;

static dbus_watch *
mk_dbus_watch_resource(ErlNifEnv * env, DBusWatch * watch)
{
    dbus_watch * res = enif_alloc_resource(DBUS_WATCH_RESOURCE, sizeof(dbus_watch));
    res->watch       = watch;
    return res;
}

static bool
get_dbus_watch_resource(ErlNifEnv * env, ERL_NIF_TERM term, dbus_watch ** dest)
{
    return enif_get_resource(env, term, DBUS_WATCH_RESOURCE, (void **)dest);
}

ERL_NIF_TERM
ebus_watch_handle(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    dbus_watch * res;
    if (!get_dbus_watch_resource(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }
    unsigned int flags;
    if (!enif_get_uint(env, argv[1], &flags))
    {
        return enif_make_badarg(env);
    }

    if (!res->watch)
    {
        // The watch was already removed
        return ATOM_FALSE;
    }

    int ret = dbus_watch_handle(res->watch, flags) ? ATOM_TRUE : ATOM_FALSE;
    return ret ? ATOM_TRUE : ATOM_FALSE;
}

static void
ebus_watch_enable(dbus_connection * state, dbus_watch * res)
{
    int                    fd          = dbus_watch_get_unix_fd(res->watch);
    enum ErlNifSelectFlags mode        = 0;
    int                    watch_flags = dbus_watch_get_flags(res->watch);
    if (watch_flags & DBUS_WATCH_READABLE)
    {
        mode |= ERL_NIF_SELECT_READ;
    }
    else if (watch_flags & DBUS_WATCH_WRITABLE)
    {
        mode |= ERL_NIF_SELECT_WRITE;
    }
    // TODO: Per the erl_nif:enif_select documentation we should
    // probably monitor the state->handler pid and remove the select
    // when the handler pid goes away.
    enif_select(state->env, fd, mode, res, &state->handler, ATOM_UNDEFINED);
}

static void
ebus_watch_disable(dbus_connection * state, dbus_watch * res)
{
    int                    fd   = dbus_watch_get_unix_fd(res->watch);
    enum ErlNifSelectFlags mode = ERL_NIF_SELECT_STOP;
    enif_select(state->env, fd, mode, res, &state->handler, ATOM_UNDEFINED);
}

dbus_bool_t
cb_add_watch(DBusWatch * watch, void * data)
{
    dbus_connection * state = (dbus_connection *)data;
    dbus_watch *      res   = mk_dbus_watch_resource(state->env, watch);

    // Keep a reference to the connection so the watch can be cleaned
    // up on removal.
    enif_keep_resource(state);
    // Hang the watch resource onto to watch itself. Cleanup is done
    // in cb_remove_watch
    dbus_watch_set_data(watch, res, NULL);
    if (dbus_watch_get_enabled(watch))
    {
        ebus_watch_enable(state, res);
    }
    return TRUE;
}

void
cb_remove_watch(DBusWatch * watch, void * data)
{
    if (!data)
    {
        return;
    }
    dbus_connection * state = (dbus_connection *)data;
    dbus_watch *      res   = (dbus_watch *)dbus_watch_get_data(watch);

    if (dbus_watch_get_enabled(watch))
    {
        ebus_watch_disable(state, res);
    }

    // Release the watch resource and connection state after disabling
    // the watch
    res->watch = NULL;
    enif_release_resource(res);
    enif_release_resource(state);
}

void
cb_toggle_watch(DBusWatch * watch, void * data)
{
    dbus_connection * state = (dbus_connection *)data;
    dbus_watch *      res   = (dbus_watch *)dbus_watch_get_data(watch);

    if (dbus_watch_get_enabled(watch))
    {
        ebus_watch_enable(state, res);
    }
    else
    {
        ebus_watch_disable(state, res);
    }
}


static void
dbus_watch_select_stop(ErlNifEnv * env, void * obj, ErlNifEvent event, int is_direct_call)
{
    // Nothing to do when a watch is stopped. The actual watch removal
    // is done when dbus calls us to remove the watch.
}


void
ebus_watch_load(ErlNifEnv * env)
{
    int                    flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceTypeInit init  = {.dtor = NULL, .stop = dbus_watch_select_stop};
    DBUS_WATCH_RESOURCE = enif_open_resource_type_x(env, "dbus_watch", &init, flags, NULL);
}
