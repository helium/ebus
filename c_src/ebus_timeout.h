#ifndef EBUS_TIMEOUT_H
#define EBUS_TIMEOUT_H

#include "ebus_connection.h"
#include "ebus_shared.h"

void
ebus_timeout_load(ErlNifEnv * env);

ERL_NIF_TERM
ebus_timeout_handle(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

//
// Connection timeout callbacks
//

dbus_bool_t
cb_add_timeout(DBusTimeout * timeout, void * data);

void
cb_remove_timeout(DBusTimeout * timeout, void * data);

void
cb_toggle_timeout(DBusTimeout * timeout, void * data);


#endif /* EBUS_TIMEOUT_H */
