#ifndef EBUS_WATCH_H
#define EBUS_WATCH_H

#include "ebus_shared.h"

void
ebus_watch_load(ErlNifEnv * env);

ERL_NIF_TERM
mk_dbus_watch(ErlNifEnv * env, DBusWatch * watch);

ERL_NIF_TERM
ebus_watch_equals(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_watch_handle(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

#endif /* EBUS_WATCH_H */
