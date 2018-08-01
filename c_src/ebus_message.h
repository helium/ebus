#ifndef EBUS_MESSAGE_H
#define EBUS_MESSAGE_H

#include "erl_nif.h"

#define EBUS_MESSAGE_FUNCS                                                     \
    {                                                                          \
        "message_new_signal", 3, ebus_message_new_signal, 0                    \
    }


void
ebus_message_load(ErlNifEnv * env);


ERL_NIF_TERM
ebus_message_new_signal(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]);


#endif /* EBUS_MESSAGE_H */
