#ifndef EBUS_WATCH_H
#define EBUS_WATCH_H

#include "ebus_connection.h"
#include "ebus_shared.h"

void
ebus_watch_load(ErlNifEnv * env);

ERL_NIF_TERM
ebus_watch_handle(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

//
// Connection watch callbacks
//

dbus_bool_t
cb_add_watch(DBusWatch * watch, void * data);

void
cb_remove_watch(DBusWatch * watch, void * data);

void
cb_toggle_watch(DBusWatch * watch, void * data);


#endif /* EBUS_WATCH_H */
