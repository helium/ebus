#include "ebus_watch.h"
#include "ebus_connection.h"
#include <stdio.h>

typedef struct
{
    DBusWatch *  watch;
} dbus_watch;

extern DBusWatch *
_dbus_watch_ref(DBusWatch * watch);
extern void
_dbus_watch_unref(DBusWatch * watch);

static ErlNifResourceType * DBUS_WATCH_RESOURCE;

static void
dbus_watch_dtor(ErlNifEnv * env, void * obj)
{
    (void)env;
    dbus_watch * res = (dbus_watch *)obj;
    printf("UNREFFING WATCH\n");
    _dbus_watch_unref(res->watch);
    printf("UNREFFED WATCH\n");
}

static void
dbus_watch_stop(ErlNifEnv * env, void * obj, ErlNifEvent event, int is_direct_call)
{
    enif_fprintf(stdout, "WATCH STOPPED %d\n", event);
}


static dbus_watch *
mk_dbus_watch_resource(ErlNifEnv * env, DBusWatch * watch)
{
    dbus_watch * res = enif_alloc_resource(DBUS_WATCH_RESOURCE, sizeof(dbus_watch));
    res->watch       = watch;
    _dbus_watch_ref(res->watch);
    return res;
}

static bool
get_dbus_watch_resource(ErlNifEnv * env, ERL_NIF_TERM term, dbus_watch ** dest)
{
    return enif_get_resource(env, term, DBUS_WATCH_RESOURCE, (void **)dest);
}

static bool
get_dbus_watch(ErlNifEnv * env, ERL_NIF_TERM term, DBusWatch ** dest)
{
    dbus_watch * res;
    if (!get_dbus_watch_resource(env, term, &res))
    {
        return false;
    }
    *dest = res->watch;
    return true;
}

ERL_NIF_TERM
ebus_watch_handle(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    DBusWatch * watch;
    if (!get_dbus_watch(env, argv[0], &watch))
    {
        enif_fprintf(stderr, "NOT A WATCH %T\n", argv[0]);
        return ATOM_FALSE;
        return enif_make_badarg(env);
    }
    unsigned int flags;
    if (!enif_get_uint(env, argv[1], &flags))
    {
        printf("NOT FLAGS\n");
        return enif_make_badarg(env);
    }
    if (flags == DBUS_WATCH_HANGUP)
    {
        printf("HANGING UP %p\n", watch);
    }

    int ret = dbus_watch_handle(watch, flags) ? ATOM_TRUE : ATOM_FALSE;
    enif_fprintf(stdout, "WATCH_HANDLED %s\n", ret ? "true" : "false");
    return ATOM_TRUE;
}

static void
ebus_watch_enable(ErlNifEnv * env, ErlNifPid * handler, dbus_watch * watch_res)
{
    int                    fd          = dbus_watch_get_unix_fd(watch_res->watch);
    enum ErlNifSelectFlags mode        = 0;
    int                    watch_flags = dbus_watch_get_flags(watch_res->watch);
    if (watch_flags & DBUS_WATCH_READABLE)
    {
        mode |= ERL_NIF_SELECT_READ;
    }
    else if (watch_flags & DBUS_WATCH_WRITABLE)
    {
        mode |= ERL_NIF_SELECT_WRITE;
    }
    enif_fprintf(stdout, "ENABLE ENIF_SELECT %d MODE %d\n", fd, mode);
    int ret = enif_select(env, fd, mode, watch_res, handler, ATOM_UNDEFINED);
    enif_fprintf(stdout, "ENABLE ENIF_SELECT DONE %d\n", ret);
}

static void
ebus_watch_disable(ErlNifEnv * env, ErlNifPid * handler, dbus_watch * watch_res)
{
    int                    fd   = dbus_watch_get_unix_fd(watch_res->watch);
    enum ErlNifSelectFlags mode = ERL_NIF_SELECT_STOP;
    enif_fprintf(stdout, "DISABLE ENIF_SELECT %d MODE %d\n", fd, mode);
    int ret = enif_select(env, fd, mode, watch_res, handler, ATOM_UNDEFINED);
    enif_fprintf(stdout, "DISABLE ENIF_SELECT DONE %d\n", ret);
}

dbus_bool_t
cb_add_watch(DBusWatch * watch, void * data)
{
    dbus_connection * state     = (dbus_connection *)data;
    dbus_watch *      watch_res = mk_dbus_watch_resource(state->env, watch);
    printf("ADD %p!\n", watch);

    // Keep a reference to the connection so the watch can be cleaned
    // up on removal.
    enif_keep_resource(state);
    // Hang the watch resource onto to watch itself. Cleanup is done
    // in cb_remove_watch
    dbus_watch_set_data(watch, watch_res, NULL);
    if (dbus_watch_get_enabled(watch))
    {
        ebus_watch_enable(state->env, &state->handler, watch_res);
    }
    else
    {
        printf("ADDED DISABLED\n");
    }
    return TRUE;
}

void
cb_remove_watch(DBusWatch * watch, void * data)
{
    dbus_connection * state     = (dbus_connection *)data;
    dbus_watch *      watch_res = (dbus_watch *)dbus_watch_get_data(watch);
    printf("REMOVE!\n");

    if (dbus_watch_get_enabled(watch))
    {
        ebus_watch_disable(state->env, &state->handler, watch_res);
    }

    // Release the watch resource and connection state after disabling
    // the watch
    enif_release_resource(watch_res);
    enif_release_resource(state);
    printf("REMOVED!\n");
}

void
cb_toggle_watch(DBusWatch * watch, void * data)
{
    dbus_connection * state     = (dbus_connection *)data;
    dbus_watch *      watch_res = (dbus_watch *)dbus_watch_get_data(watch);

    if (dbus_watch_get_enabled(watch))
    {
        ebus_watch_enable(state->env, &state->handler, watch_res);
    }
    else
    {
        ebus_watch_disable(state->env, &state->handler, watch_res);
    }
}


void
ebus_watch_load(ErlNifEnv * env)
{
    int                    flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceTypeInit init  = {.dtor = dbus_watch_dtor, .stop = dbus_watch_stop};
    DBUS_WATCH_RESOURCE = enif_open_resource_type_x(env, "dbus_watch", &init, flags, NULL);
}
