#ifndef EBUS_MESSAGE_H
#define EBUS_MESSAGE_H

#include "ebus_shared.h"

void
ebus_message_load(ErlNifEnv * env);

ERL_NIF_TERM
ebus_message_new_signal(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_new_call(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_append_args(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_args(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);


#endif /* EBUS_MESSAGE_H */
