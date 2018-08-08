#ifndef EBUS_MESSAGE_GET_ARGS_H
#define EBUS_MESSAGE_GET_ARGS_H

#include "ebus_shared.h"

ERL_NIF_TERM
ebus_message_list_args(ErlNifEnv * env, DBusMessage *message);


#endif /* EBUS_MESSAGE_GET_ARGS_H */
