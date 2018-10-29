#ifndef EBUS_MESSAGE_H
#define EBUS_MESSAGE_H

#include "ebus_shared.h"

void
ebus_message_load(ErlNifEnv * env);

ERL_NIF_TERM
mk_dbus_message(ErlNifEnv * env, DBusMessage * msg);

bool
get_dbus_message(ErlNifEnv * env, ERL_NIF_TERM term, DBusMessage ** dest);

int
get_dbus_message_type(ErlNifEnv * env, ERL_NIF_TERM term, unsigned int * dest);

ERL_NIF_TERM
ebus_message_new_signal(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_new_call(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_new_reply(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_new_reply_error(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_append_args(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_args(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_type(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_serial(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_set_serial(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_reply_serial(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_destination(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_path(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_interface(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_member(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_get_error(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
ebus_message_infer_signature(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);


#endif /* EBUS_MESSAGE_H */
