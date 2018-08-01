/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "dbus/dbus.h"
#include "erl_nif.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>


static ErlNifResourceType * DBUS_CONNECTION_RESOURCE;

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;

typedef struct
{
    DBusConnection * connection;
} dbus_connection;

static void
dbus_connection_dtor(ErlNifEnv * env, void * obj)
{
    (void)env;
    dbus_connection * conn = (dbus_connection *)obj;
    dbus_connection_unref(conn->connection);
}

static ERL_NIF_TERM
mk_dbus_connection(ErlNifEnv * env, DBusConnection * conn)
{
    dbus_connection * res =
        enif_alloc_resource(DBUS_CONNECTION_RESOURCE, sizeof(dbus_connection));
    res->connection       = conn;
    ERL_NIF_TERM res_term = enif_make_resource(env, res);
    enif_release_resource(res);
    return res_term;
}

static bool
get_dbus_connection(ErlNifEnv * env, ERL_NIF_TERM term, DBusConnection ** dest)
{
    dbus_connection * conn;
    if (!enif_get_resource(env, term, DBUS_CONNECTION_RESOURCE, (void **)&conn))
    {
        return false;
    }
    *dest = conn->connection;
    return true;
}

static ERL_NIF_TERM
erl_bus(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int bus_type;
    if (argc < 1 || !enif_get_int(env, argv[0], &bus_type) || bus_type < 0
        || bus_type > DBUS_BUS_STARTER)
    {
        return enif_make_badarg(env);
    }

    DBusError error;
    dbus_error_init(&error);

    DBusConnection * connection = dbus_bus_get(bus_type, &error);
    if (dbus_error_is_set(&error))
    {
        ERL_NIF_TERM message =
            enif_make_string(env, error.message, ERL_NIF_LATIN1);
        dbus_error_free(&error);
        return enif_make_tuple2(env, ATOM_ERROR, message);
    }

    dbus_connection_set_exit_on_disconnect(connection, FALSE);
    return enif_make_tuple2(env, ATOM_OK, mk_dbus_connection(env, connection));
}

#define GET_STR(N, A)                                                          \
    unsigned N##_len;                                                          \
    if (!enif_get_list_length(env, A, &N##_len))                               \
    {                                                                          \
        return enif_make_badarg(env);                                          \
    }                                                                          \
                                                                               \
    char N[N##_len + 1];                                                       \
    enif_get_string(env, A, N, N##_len + 1, ERL_NIF_LATIN1);


#define CHECK_INT_ERR(F)                                                       \
    {                                                                          \
        DBusError error;                                                       \
        dbus_error_init(&error);                                               \
        int result = (F);                                                      \
        if (dbus_error_is_set(&error))                                         \
        {                                                                      \
            ERL_NIF_TERM message =                                             \
                enif_make_string(env, error.message, ERL_NIF_LATIN1);          \
            dbus_error_free(&error);                                           \
            return enif_make_tuple2(env, ATOM_ERROR, message);                 \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            return enif_make_tuple2(env, ATOM_OK, enif_make_int(env, result)); \
        }                                                                      \
    }

static ERL_NIF_TERM
erl_bus_unique_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    DBusConnection * connection;
    if (argc < 1 || !get_dbus_connection(env, argv[0], &connection))
    {
        return enif_make_badarg(env);
    }

    const char * name = dbus_bus_get_unique_name(connection);
    return enif_make_string(env, name, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM
erl_bus_release_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    DBusConnection * connection;
    if (!get_dbus_connection(env, argv[0], &connection))
    {
        return enif_make_badarg(env);
    }

    GET_STR(name, argv[1]);

    if (!dbus_validate_bus_name(name, NULL))
    {
        return enif_make_badarg(env);
    }

    CHECK_INT_ERR(dbus_bus_release_name(connection, name, &error));
}

static ERL_NIF_TERM
erl_bus_request_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 3)
    {
        return enif_make_badarg(env);
    }

    DBusConnection * connection;
    if (!get_dbus_connection(env, argv[0], &connection))
    {
        return enif_make_badarg(env);
    }

    GET_STR(name, argv[1]);

    if (!dbus_validate_bus_name(name, NULL))
    {
        return enif_make_badarg(env);
    }

    int flags;
    if (!enif_get_int(env, argv[2], &flags))
    {
        return enif_make_badarg(env);
    }

    CHECK_INT_ERR(dbus_bus_request_name(connection, name, flags, &error));
}

static ERL_NIF_TERM
erl_bus_add_match(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    DBusConnection * connection;
    if (!get_dbus_connection(env, argv[0], &connection))
    {
        return enif_make_badarg(env);
    }

    GET_STR(rule, argv[1]);

    DBusError error;
    dbus_error_init(&error);
    dbus_bus_add_match(connection, rule, &error);
    if (dbus_error_is_set(&error))
    {
        ERL_NIF_TERM message =
            enif_make_string(env, error.message, ERL_NIF_LATIN1);
        dbus_error_free(&error);
        return enif_make_tuple2(env, ATOM_ERROR, message);
    }
    else
    {
        return ATOM_OK;
    }
}

static ErlNifFunc nif_funcs[] =
    {{"bus", 1, erl_bus, 0},
     {"unique_name", 1, erl_bus_unique_name, 0},
     {"request_name", 3, erl_bus_request_name, 0},
     {"release_name", 2, erl_bus_release_name, 0},
     {"add_match", 2, erl_bus_add_match, ERL_NIF_DIRTY_JOB_IO_BOUND}};

#define ATOM(Id, Value)                                                        \
    {                                                                          \
        Id = enif_make_atom(env, Value);                                       \
    }

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;


    int flags                = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    DBUS_CONNECTION_RESOURCE = enif_open_resource_type(env,
                                                       NULL,
                                                       "dbus_connection",
                                                       dbus_connection_dtor,
                                                       flags,
                                                       NULL);
    if (DBUS_CONNECTION_RESOURCE == NULL)
    {
        return -1;
    }

    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error");
    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");
    return 0;
}

ERL_NIF_INIT(dbus, nif_funcs, load, NULL, NULL, NULL);
