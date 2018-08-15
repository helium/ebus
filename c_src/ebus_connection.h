#ifndef EBUS_CONNECTION_H
#define EBUS_CONNECTION_H

#include "ebus_shared.h"

void
ebus_connection_load(ErlNifEnv * env);

typedef struct
{
    ErlNifEnv *      env;
    DBusConnection * connection;
    ErlNifPid        handler;
    // Filters
    ErlNifEnv *  filter_env;
    ERL_NIF_TERM filters;
} dbus_connection;

ERL_NIF_TERM
mk_dbus_connection(ErlNifEnv * env, DBusWatch * watch);

bool
get_dbus_connection_resource(ErlNifEnv * env, ERL_NIF_TERM term, dbus_connection ** dest);

ERL_NIF_TERM
ebus_connection_get(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_close(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_unique_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_request_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_release_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_add_match(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_send(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_dispatch(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_add_filter(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_remove_filter(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);


#endif /* EBUS_CONNECTION_H */
