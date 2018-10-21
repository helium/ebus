#include "ebus_connection.h"
#include "ebus_error.h"
#include "ebus_message.h"
#include "ebus_object.h"
#include "ebus_timeout.h"
#include "ebus_watch.h"
#include <stdio.h>
#include <string.h>

static ErlNifResourceType * DBUS_CONNECTION_RESOURCE;

ERL_NIF_TERM ATOM_DISPATCH_STATUS;
ERL_NIF_TERM ATOM_WAKEUP_MAIN;

static void
dbus_connection_dtor(ErlNifEnv * env, void * obj)
{
    (void)env;
    dbus_connection * res = (dbus_connection *)obj;
    ebus_filters_free(res->filters);
    enif_free_env(res->env);
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
    dbus_connection * state   = (dbus_connection *)data;
    ErlNifEnv *       msg_env = enif_alloc_env();
    ERL_NIF_TERM      msg =
        enif_make_tuple2(msg_env, ATOM_DISPATCH_STATUS, enif_make_uint(msg_env, status));

    enif_send(NULL, &state->handler, msg_env, msg);
    enif_free_env(msg_env);
}

static void
cb_wakeup_main(void * data)
{
    dbus_connection * state = (dbus_connection *)data;
    enif_send(NULL, &state->handler, NULL, ATOM_WAKEUP_MAIN);
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

    DBusConnection * connection = dbus_bus_get(bus_type, &error);
    if (dbus_error_is_set(&error))
    {
        return handle_dbus_error(env, &error);
    }

    dbus_connection_set_exit_on_disconnect(connection, FALSE);
    dbus_connection * res =
        enif_alloc_resource(DBUS_CONNECTION_RESOURCE, sizeof(dbus_connection));
    res->connection      = connection;
    res->handler         = handler_pid;
    res->env             = enif_alloc_env();
    res->next_timeout_id = 0;
    res->filters         = NULL;

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

    if (!dbus_connection_add_filter(connection, cb_filter_handle_message, res, NULL))
    {
        enif_release_resource(res);
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    dbus_connection_set_dispatch_status_function(connection, cb_dispatch_status, res, NULL);
    dbus_connection_set_wakeup_main_function(connection, cb_wakeup_main, res, NULL);

    ERL_NIF_TERM term_res = enif_make_resource(env, res);
    enif_release_resource(res);
    return enif_make_tuple2(env, ATOM_OK, term_res);
}

ERL_NIF_TERM
ebus_connection_close(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    dbus_connection * res;
    if (argc != 1 || !get_dbus_connection_resource(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    dbus_connection_remove_filter(res->connection, cb_filter_handle_message, res);

    return ATOM_OK;
}

#define CHECK_INT_ERR(F)                                                                 \
    {                                                                                    \
        DBusError error;                                                                 \
        dbus_error_init(&error);                                                         \
        int result = (F);                                                                \
        if (dbus_error_is_set(&error))                                                   \
        {                                                                                \
            return handle_dbus_error(env, &error);                                       \
        }                                                                                \
        else                                                                             \
        {                                                                                \
            return enif_make_tuple2(env, ATOM_OK, enif_make_int(env, result));           \
        }                                                                                \
    }

ERL_NIF_TERM
ebus_connection_unique_name(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
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

    const char * name = dbus_bus_get_unique_name(connection);
    return enif_make_string(env, name, ERL_NIF_LATIN1);
}

ERL_NIF_TERM
ebus_connection_bus_id(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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

    const char * name = dbus_bus_get_id(connection, NULL);
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

    return handle_dbus_error(env, &error);
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

    if (!dbus_connection_send(connection, message, NULL))
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    dbus_connection_read_write(connection, 1);
    return ATOM_OK;
}

ERL_NIF_TERM
ebus_connection_call(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 4)
    {
        return enif_make_badarg(env);
    }

    dbus_connection * connection;
    if (!get_dbus_connection_resource(env, argv[0], &connection))
    {
        return enif_make_badarg(env);
    }

    DBusMessage * message;
    if (!get_dbus_message(env, argv[1], &message))
    {
        return enif_make_badarg(env);
    }

    ErlNifPid pid;
    if (!enif_get_local_pid(env, argv[2], &pid))
    {
        return enif_make_badarg(env);
    }

    int timeout;
    if (!enif_get_int(env, argv[3], &timeout))
    {
        return enif_make_badarg(env);
    }

    DBusPendingCall * pending;
    if (!dbus_connection_send_with_reply(connection->connection, message, &pending, timeout))
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    unsigned int serial = dbus_message_get_serial(message);

    dbus_object * obj = mk_dbus_object_resource(env, &pid, connection);
    if (!dbus_pending_call_set_notify(pending, cb_object_handle_reply, obj, NULL))
    {
        enif_release_resource(obj);
        dbus_pending_call_unref(pending);
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    dbus_connection_read_write(connection->connection, 1);
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

    return enif_make_int(env, dbus_connection_dispatch(connection));
}

ERL_NIF_TERM
ebus_connection_set_filters(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
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

    return mk_ebus_filters(env, argv[1], &res->filters);
}

ERL_NIF_TERM
ebus_connection_register_object_path(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 3)
    {
        return enif_make_badarg(env);
    }

    dbus_connection * state;
    if (!get_dbus_connection_resource(env, argv[0], &state))
    {
        return enif_make_badarg(env);
    }

    GET_STR(path, argv[1]);
    if (!dbus_validate_path(path, NULL))
    {
        return enif_make_badarg(env);
    }

    ErlNifPid pid;
    if (!enif_get_local_pid(env, argv[2], &pid))
    {
        return enif_make_badarg(env);
    }

    DBusError error;
    dbus_error_init(&error);
    DBusObjectPathVTable vtable = {.unregister_function = cb_object_unregister,
                                   .message_function    = cb_object_handle_message};

    dbus_object * obj = mk_dbus_object_resource(env, &pid, NULL);
    if (!dbus_connection_try_register_object_path(state->connection, path, &vtable, obj, &error))
    {
        enif_release_resource(obj);
        return handle_dbus_error(env, &error);
    }

    return ATOM_OK;
}

ERL_NIF_TERM
ebus_connection_unregister_object_path(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
    {
        return enif_make_badarg(env);
    }

    dbus_connection * state;
    if (!get_dbus_connection_resource(env, argv[0], &state))
    {
        return enif_make_badarg(env);
    }

    GET_STR(path, argv[1]);
    if (!dbus_validate_path(path, NULL))
    {
        return enif_make_badarg(env);
    }

    if (!dbus_connection_unregister_object_path(state->connection, path))
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    return ATOM_OK;
}


void
ebus_connection_load(ErlNifEnv * env)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    DBUS_CONNECTION_RESOURCE =
        enif_open_resource_type(env, NULL, "dbus_connection", dbus_connection_dtor, flags, NULL);

    ATOM(ATOM_DISPATCH_STATUS, "dispatch_status");
    ATOM(ATOM_WAKEUP_MAIN, "wakeup_main");
}
