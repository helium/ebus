#include "ebus_error.h"
#include <string.h>

typedef struct
{
    const char * name;
    ERL_NIF_TERM atom;
} error_entry;

static error_entry * error_map;
static ERL_NIF_TERM  ATOM_UNKNOWN;
static ERL_NIF_TERM  ATOM_TIMEOUT;
static ERL_NIF_TERM  ATOM_NO_ACCESS;
static ERL_NIF_TERM  ATOM_NAMED;

ERL_NIF_TERM
mk_dbus_error(ErlNifEnv * env, DBusError * error)
{
    error_entry * entry = error_map;
    ERL_NIF_TERM  term  = ATOM_UNDEFINED;
    while (entry->name)
    {
        if (dbus_error_has_name(error, entry->name))
        {
            term = entry->atom;
            break;
        }
        entry += 1;
    }

    if (term == ATOM_UNDEFINED)
    {
        term = enif_make_string(env, error->name, ERL_NIF_LATIN1);
    }

    return enif_make_tuple2(env, ATOM_ERROR, term);
}

ERL_NIF_TERM
handle_dbus_error(ErlNifEnv * env, DBusError * error)
{
    ERL_NIF_TERM result = ATOM_OK;
    if (error && dbus_error_is_set(error))
    {
        result = mk_dbus_error(env, error);
        dbus_error_free(error);
    }
    return result;
}


void
ebus_error_load(ErlNifEnv * env)
{
    ATOM(ATOM_UNKNOWN, "unknown");
    ATOM(ATOM_TIMEOUT, "timeout");
    ATOM(ATOM_NO_ACCESS, "no_access");
    ATOM(ATOM_NAMED, "named");

    error_entry errors[] = {{DBUS_ERROR_NO_MEMORY, ATOM_ENOMEM},
                            {DBUS_ERROR_ADDRESS_IN_USE, ATOM_ALREADY},
                            {DBUS_ERROR_OBJECT_PATH_IN_USE, ATOM_ALREADY},
                            {DBUS_ERROR_TIMEOUT, ATOM_TIMEOUT},
                            {DBUS_ERROR_TIMED_OUT, ATOM_TIMEOUT},
                            {DBUS_ERROR_NO_REPLY, ATOM_TIMEOUT},
                            {DBUS_ERROR_ACCESS_DENIED, ATOM_NO_ACCESS},

                            {DBUS_ERROR_UNKNOWN_INTERFACE, ATOM_UNKNOWN},
                            {DBUS_ERROR_UNKNOWN_METHOD, ATOM_UNKNOWN},
                            {DBUS_ERROR_UNKNOWN_OBJECT, ATOM_UNKNOWN},
                            {DBUS_ERROR_UNKNOWN_PROPERTY, ATOM_UNKNOWN},

                            {NULL, ATOM_UNDEFINED}};

    error_map = enif_alloc(sizeof(errors));
    memcpy(error_map, errors, sizeof(errors));
}
