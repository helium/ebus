#include "ebus_connection.h"
#include "ebus_message.h"
#include "ebus_timeout.h"
#include "ebus_watch.h"
#include <stdio.h>
#include <string.h>

static ErlNifResourceType * DBUS_CONNECTION_RESOURCE;

ERL_NIF_TERM ATOM_TYPE;
ERL_NIF_TERM ATOM_DESTINATION;
ERL_NIF_TERM ATOM_INTERFACE;
ERL_NIF_TERM ATOM_PATH;
ERL_NIF_TERM ATOM_MEMBER;
ERL_NIF_TERM ATOM_FILTER_MATCH;
ERL_NIF_TERM ATOM_DISPATCH_STATUS;

static void
dbus_connection_dtor(ErlNifEnv * env, void * obj)
{
    (void)env;
    dbus_connection * res = (dbus_connection *)obj;
    if (res->filter_env)
    {
        printf("FREEING FILTER_ENV\n");
        enif_free_env(res->filter_env);
    }
}

bool
get_dbus_connection_resource(ErlNifEnv * env, ERL_NIF_TERM term, dbus_connection ** dest)
{
    return enif_get_resource(env, term, DBUS_CONNECTION_RESOURCE, (void **)dest);
}

static bool
get_dbus_connection(ErlNifEnv * env, ERL_NIF_TERM term, DBusConnection ** dest)
{
    dbus_connection * conn;
    if (!get_dbus_connection_resource(env, term, &conn))
    {
        return false;
    }
    *dest = conn->connection;
    return true;
}

//
// Connection interface
//

static void
cb_dispatch_status(DBusConnection * c, DBusDispatchStatus status, void * data)
{
    dbus_connection * res = (dbus_connection *)data;
    ERL_NIF_TERM      msg =
        enif_make_tuple2(res->env, ATOM_DISPATCH_STATUS, enif_make_uint(res->env, status));
    enif_fprintf(stderr, "CB DISPATCH %T\n", msg);
    enif_send(res->env, &res->handler, NULL, msg);
}

ERL_NIF_TERM
ebus_connection_get(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    int bus_type;
    if (!enif_get_int(env, argv[0], &bus_type) || bus_type < 0
        || bus_type > DBUS_BUS_STARTER)
    {
        return enif_make_badarg(env);
    }

    ErlNifPid handler_pid;
    if (!enif_get_local_pid(env, argv[1], &handler_pid))
    {
        return enif_make_badarg(env);
    }

    DBusError error;
    dbus_error_init(&error);

    DBusConnection * connection = dbus_bus_get_private(bus_type, &error);
    if (dbus_error_is_set(&error))
    {
        ERL_NIF_TERM message = enif_make_string(env, error.message, ERL_NIF_LATIN1);
        dbus_error_free(&error);
        return enif_make_tuple2(env, ATOM_ERROR, message);
    }

    dbus_connection_set_exit_on_disconnect(connection, FALSE);
    dbus_connection * res =
        enif_alloc_resource(DBUS_CONNECTION_RESOURCE, sizeof(dbus_connection));
    res->connection = connection;
    res->handler    = handler_pid;
    res->env        = env;
    res->filter_env = NULL;

    if (!dbus_connection_set_watch_functions(
            connection, cb_add_watch, cb_remove_watch, cb_toggle_watch, res, NULL))
    {
        enif_release_resource(res);
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    if (!dbus_connection_set_timeout_functions(
            connection, cb_add_timeout, cb_remove_timeout, cb_toggle_timeout, res, NULL))
    {
        enif_release_resource(res);
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    enif_keep_resource(res);
    dbus_connection_set_dispatch_status_function(connection,
                                                 cb_dispatch_status,
                                                 res,
                                                 enif_release_resource);

    return enif_make_tuple2(env, ATOM_OK, enif_make_resource(env, res));
}

ERL_NIF_TERM
ebus_connection_close(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    dbus_connection * res;
    if (argc != 1 || !get_dbus_connection_resource(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    printf("CLOSING\n");
    dbus_connection_flush(res->connection);
    dbus_connection_close(res->connection);
    enif_release_resource(res);

    return ATOM_OK;
}

#define CHECK_INT_ERR(F)                                                                 \
    {                                                                                    \
        DBusError error;                                                                 \
        dbus_error_init(&error);                                                         \
        int result = (F);                                                                \
        if (dbus_error_is_set(&error))                                                   \
        {                                                                                \
            ERL_NIF_TERM message = enif_make_string(env, error.message, ERL_NIF_LATIN1); \
            dbus_error_free(&error);                                                     \
            return enif_make_tuple2(env, ATOM_ERROR, message);                           \
        }                                                                                \
        else                                                                             \
        {                                                                                \
            return enif_make_tuple2(env, ATOM_OK, enif_make_int(env, result));           \
        }                                                                                \
    }

ERL_NIF_TERM
ebus_connection_unique_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    DBusConnection * connection;
    if (argc < 1 || !get_dbus_connection(env, argv[0], &connection))
    {
        return enif_make_badarg(env);
    }

    const char * name = dbus_bus_get_unique_name(connection);
    return enif_make_string(env, name, ERL_NIF_LATIN1);
}

ERL_NIF_TERM
ebus_connection_request_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
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

ERL_NIF_TERM
ebus_connection_release_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
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

ERL_NIF_TERM
ebus_connection_add_match(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
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
        ERL_NIF_TERM message = enif_make_string(env, error.message, ERL_NIF_LATIN1);
        dbus_error_free(&error);
        return enif_make_tuple2(env, ATOM_ERROR, message);
    }
    else
    {
        return ATOM_OK;
    }
}

ERL_NIF_TERM
ebus_connection_send(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
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

    DBusMessage * message;
    if (!get_dbus_message(env, argv[1], &message))
    {
        return enif_make_badarg(env);
    }

    unsigned int serial;
    printf("SENDING\n");
    if (!dbus_connection_send(connection, message, &serial))
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    dbus_connection_flush(connection);
    return enif_make_tuple2(env, ATOM_OK, enif_make_uint(env, serial));
}


ERL_NIF_TERM
ebus_connection_dispatch(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    DBusConnection * connection;
    if (!get_dbus_connection(env, argv[0], &connection))
    {
        return enif_make_badarg(env);
    }

    printf("DISPATCH\n");

    return enif_make_int(env, dbus_connection_dispatch(connection));
}

static int
filter_match_string(ErlNifEnv * env, ERL_NIF_TERM value, const char * str)
{
    if (str != NULL && enif_is_list(env, value))
    {
        GET_STR(f_str, value);
        return strcmp(f_str, str);
    }
    return FALSE;
}

static int
filter_matches(ErlNifEnv * env, ERL_NIF_TERM filter, DBusMessage * message)
{
    ERL_NIF_TERM f_val;
    int          match = FALSE;
    if (enif_get_map_value(env, filter, ATOM_TYPE, &f_val))
    {
        int f_type;
        int m_type = dbus_message_get_type(message);
        if (enif_get_int(env, f_val, &f_type) && f_type == m_type)
        {
            match = TRUE;
        }
    }

    if (enif_get_map_value(env, filter, ATOM_DESTINATION, &f_val))
    {
        match = filter_match_string(env, f_val, dbus_message_get_destination(message));
    }

    if (enif_get_map_value(env, filter, ATOM_PATH, &f_val))
    {
        match = filter_match_string(env, f_val, dbus_message_get_path(message));
    }

    if (enif_get_map_value(env, filter, ATOM_INTERFACE, &f_val))
    {
        match = filter_match_string(env, f_val, dbus_message_get_interface(message));
    }

    if (enif_get_map_value(env, filter, ATOM_INTERFACE, &f_val))
    {
        match = filter_match_string(env, f_val, dbus_message_get_interface(message));
    }

    if (enif_get_map_value(env, filter, ATOM_MEMBER, &f_val))
    {
        match = filter_match_string(env, f_val, dbus_message_get_member(message));
    }

    return match;
}

static DBusHandlerResult
filter_handle_message(DBusConnection * connection, DBusMessage * message, void * data)
{
    dbus_connection * state   = (dbus_connection *)data;
    ErlNifEnv *       f_env   = state->filter_env;
    ERL_NIF_TERM      filters = state->filters;

    ERL_NIF_TERM filter;
    while (enif_get_list_cell(f_env, filters, &filter, &filters))
    {
        const ERL_NIF_TERM * filter_tuple;
        int                  arity;
        enif_get_tuple(f_env, filter, &arity, &filter_tuple);

        ERL_NIF_TERM f_ref   = filter_tuple[0];
        ERL_NIF_TERM f_entry = filter_tuple[1];
        if (filter_matches(f_env, f_entry, message))
        {
            ERL_NIF_TERM handler_msg =
                enif_make_tuple3(state->env,
                                 ATOM_FILTER_MATCH,
                                 f_ref,
                                 mk_dbus_message(state->env, message));
            enif_send(state->env, &state->handler, NULL, handler_msg);
            return DBUS_HANDLER_RESULT_HANDLED;
        }
    }
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

ERL_NIF_TERM
ebus_connection_add_filter(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    dbus_connection * res;
    if (!get_dbus_connection_resource(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    int                  arity;
    const ERL_NIF_TERM * filter_tuple;
    if (!enif_get_tuple(env, argv[1], &arity, &filter_tuple) || arity != 2)
    {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM f_ref    = filter_tuple[0];
    ERL_NIF_TERM f_filter = filter_tuple[1];
    if (!enif_is_ref(env, f_ref) || !enif_is_map(env, f_filter))
    {
        return enif_make_badarg(env);
    }

    ErlNifEnv * f_env = res->filter_env;
    if (f_env == NULL)
    {
        res->filter_env = enif_alloc_env();
        if (!dbus_connection_add_filter(res->connection,
                                        filter_handle_message,
                                        res,
                                        enif_release_resource))
        {
            enif_free_env(res->filter_env);
            res->filter_env = NULL;
            return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
        }
        enif_keep_resource(res);
        f_env        = res->filter_env;
        res->filters = enif_make_list(f_env, 0);
        printf("ADDED FILTER\n");
    }

    ERL_NIF_TERM f_tuple = enif_make_tuple2(f_env, f_ref, f_filter);
    res->filters         = enif_make_list_cell(f_env, f_tuple, res->filters);

    return ATOM_OK;
}

ERL_NIF_TERM
ebus_connection_remove_filter(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    dbus_connection * res;
    if (!get_dbus_connection_resource(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM f_ref = argv[1];
    if (!enif_is_ref(env, f_ref))
    {
        return enif_make_badarg(env);
    }

    ErlNifEnv * f_env = res->filter_env;
    if (f_env == NULL)
    {
        return ATOM_OK;
    }

    ERL_NIF_TERM filters     = res->filters;
    ERL_NIF_TERM new_filters = enif_make_list(f_env, 0);
    ERL_NIF_TERM filter;
    while (enif_get_list_cell(f_env, filters, &filter, &filters))
    {
        const ERL_NIF_TERM * filter_tuple;
        int                  arity;
        enif_get_tuple(f_env, filter, &arity, &filter_tuple);
        if (arity == 2 && !enif_is_identical(filter_tuple[0], f_ref))
        {
            new_filters = enif_make_list_cell(f_env, filter, new_filters);
        }
    }
    enif_make_reverse_list(f_env, new_filters, &res->filters);
    enif_fprintf(stderr, "FILTERS %T\n", res->filters);

    if (enif_is_empty_list(f_env, res->filters))
    {
        dbus_connection_remove_filter(res->connection, filter_handle_message, res);
        enif_free_env(f_env);
        res->filter_env = NULL;
    }

    return ATOM_OK;
}


void
ebus_connection_load(ErlNifEnv * env)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    DBUS_CONNECTION_RESOURCE =
        enif_open_resource_type(env, NULL, "dbus_connection", dbus_connection_dtor, flags, NULL);

    ATOM(ATOM_TYPE, "type");
    ATOM(ATOM_DESTINATION, "destination");
    ATOM(ATOM_PATH, "path");
    ATOM(ATOM_INTERFACE, "interface");
    ATOM(ATOM_MEMBER, "member");
    ATOM(ATOM_FILTER_MATCH, "filter_match");
    ATOM(ATOM_DISPATCH_STATUS, "dispatch_status");
}
