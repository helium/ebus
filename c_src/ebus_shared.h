#ifndef EBUS_SHARED_H
#define EBUS_SHARED_H

#include "dbus/dbus.h"
#include "erl_nif.h"
#include <stdbool.h>

extern ERL_NIF_TERM ATOM_OK;
extern ERL_NIF_TERM ATOM_ERROR;
extern ERL_NIF_TERM ATOM_ENOMEM;

#define GET_STR(N, A)                                                          \
    unsigned N##_len;                                                          \
    if (!enif_get_list_length(env, A, &N##_len))                               \
    {                                                                          \
        return enif_make_badarg(env);                                          \
    }                                                                          \
                                                                               \
    char N[N##_len + 1];                                                       \
    enif_get_string(env, A, N, N##_len + 1, ERL_NIF_LATIN1);


#endif /* EBUS_SHARED_H */
