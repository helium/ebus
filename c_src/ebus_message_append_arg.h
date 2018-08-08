#ifndef EBUS_MESSAGE_APPEND_H
#define EBUS_MESSAGE_APPEND_H

#include "ebus_shared.h"

int
ebus_message_append_arg(ErlNifEnv *         env,
                        ERL_NIF_TERM        term,
                        DBusMessageIter *   appender,
                        DBusSignatureIter * sig_iter,
                        dbus_bool_t *       more);

#endif /* EBUS_MESSAGE_APPEND_H */
