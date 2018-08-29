#ifndef EBUS_FILTER_H
#define EBUS_FILTER_H

#include "ebus_shared.h"

#define EBUS_FILTER_ID_END (unsigned int)(-1)

typedef struct
{
    unsigned     id;
    int          type;
    const char * destination;
    const char * path;
    const char * interface;
    const char * member;
} ebus_filter;

void
ebus_filter_load(ErlNifEnv * env);

ERL_NIF_TERM
mk_ebus_filters(ErlNifEnv * env, ERL_NIF_TERM filters, ebus_filter ** dest);

ebus_filter *
ebus_filters_free(ebus_filter * filters);

DBusHandlerResult
cb_filter_handle_message(DBusConnection * connection, DBusMessage * message, void * data);


#endif /* EBUS_FILTER_H */
