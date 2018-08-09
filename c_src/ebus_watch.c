#include "ebus_watch.h"

extern DBusWatch *
_dbus_watch_ref(DBusWatch * watch);
extern DBusWatch *
_dbus_watch_unref(DBusWatch * watch);

static ErlNifResourceType * DBUS_WATCH_RESOURCE;

typedef struct
{
    DBusWatch * watch;
} dbus_watch;

static void
dbus_watch_dtor(ErlNifEnv * env, void * obj)
{
    (void)env;
    dbus_watch * res = (dbus_watch *)obj;
    _dbus_watch_unref(res->watch);
}

ERL_NIF_TERM
mk_dbus_watch(ErlNifEnv * env, DBusWatch * watch)
{
    dbus_watch * res      = enif_alloc_resource(DBUS_WATCH_RESOURCE, sizeof(dbus_watch));
    res->watch            = watch;
    ERL_NIF_TERM res_term = enif_make_resource(env, res);
    enif_release_resource(res);
    return res_term;
}

static bool
get_dbus_watch(ErlNifEnv * env, ERL_NIF_TERM term, DBusWatch ** dest)
{
    dbus_watch * res;
    if (!enif_get_resource(env, term, DBUS_WATCH_RESOURCE, (void **)&res))
    {
        return false;
    }
    *dest = res->watch;
    return true;
}


ERL_NIF_TERM
ebus_watch_equals(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    DBusWatch * lhs;
    if (!get_dbus_watch(env, argv[0], &lhs))
    {
        return enif_make_badarg(env);
    }

    DBusWatch * rhs;
    if (!get_dbus_watch(env, argv[1], &rhs))
    {
        return enif_make_badarg(env);
    }

    return lhs == rhs ? ATOM_TRUE : ATOM_FALSE;
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
        return enif_make_badarg(env);
    }

    unsigned int flags;
    if (!enif_get_uint(env, argv[1], &flags))
    {
        return enif_make_badarg(env);
    }

    return dbus_watch_handle(watch, flags) ? ATOM_TRUE : ATOM_FALSE;
}


void
ebus_watch_load(ErlNifEnv * env)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    DBUS_WATCH_RESOURCE =
        enif_open_resource_type(env, NULL, "dbus_watch", dbus_watch_dtor, flags, NULL);
}
