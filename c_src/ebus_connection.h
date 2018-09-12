#ifndef EBUS_CONNECTION_H
#define EBUS_CONNECTION_H

#include "ebus_filter.h"
#include "ebus_shared.h"

typedef struct
{
    ErlNifEnv *      env;
    DBusConnection * connection;
    ErlNifPid        handler;
    ebus_filter *    filters;
    unsigned long    next_timeout_id;
} dbus_connection;

void
ebus_connection_load(ErlNifEnv * env);

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
ebus_connection_call(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_dispatch(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_set_filters(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_register_object_path(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_connection_unregister_object_path(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);


#endif /* EBUS_CONNECTION_H */
