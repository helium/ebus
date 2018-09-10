#include "ebus_error.h"

static ERL_NIF_TERM error_map;

ERL_NIF_TERM
mk_dbus_error(ErlNifEnv * env, DBusError * error)
{
    ERL_NIF_TERM key  = enif_make_string(env, error->name, ERL_NIF_LATIN1);
    ERL_NIF_TERM term = ATOM_UNKNOWN;
    enif_get_map_value(env, error_map, key, &term);
    return enif_make_tuple2(env, ATOM_ERROR, term);
}

ERL_NIF_TERM
handle_dbus_error(ErlNifEnv * env, DBusError * error)
{
    ERL_NIF_TERM result = ATOM_OK;
    if (dbus_error_is_set(error))
    {
        result = mk_dbus_error(env, error);
        dbus_error_free(error);
    }
    return result;
}


void
ebus_error_load(ErlNifEnv * env)
{
    error_map = enif_make_new_map(env);

    struct entry
    {
        const char * name;
        ERL_NIF_TERM term;
    } errors[] = {{DBUS_ERROR_NO_MEMORY, ATOM_ENOMEM},
                  {DBUS_ERROR_ADDRESS_IN_USE, ATOM_ALREADY},
                  {DBUS_ERROR_OBJECT_PATH_IN_USE, ATOM_ALREADY},

                  {DBUS_ERROR_UNKNOWN_INTERFACE, ATOM_UNKNOWN},
                  {DBUS_ERROR_UNKNOWN_METHOD, ATOM_UNKNOWN},
                  {DBUS_ERROR_UNKNOWN_OBJECT, ATOM_UNKNOWN},
                  {DBUS_ERROR_UNKNOWN_PROPERTY, ATOM_UNKNOWN},

                  {NULL, ATOM_UNKNOWN}};

    struct entry * entry = errors;
    while (entry->name)
    {
        ERL_NIF_TERM key = enif_make_string(env, entry->name, ERL_NIF_LATIN1);
        enif_make_map_put(env, error_map, key, entry->term, &error_map);
        entry += 1;
    }
}
