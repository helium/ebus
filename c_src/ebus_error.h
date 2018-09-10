#ifndef EBUS_ERROR_H
#define EBUS_ERROR_H

#include "ebus_shared.h"

void
ebus_error_load(ErlNifEnv * env);

ERL_NIF_TERM
mk_dbus_error(ErlNifEnv * env, DBusError *error);

ERL_NIF_TERM
handle_dbus_error(ErlNifEnv * env, DBusError * error);


#endif /* EBUS_ERROR_H */
