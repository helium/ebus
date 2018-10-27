#include "ebus_filter.h"
#include "ebus_connection.h"
#include "ebus_message.h"
#include <stdio.h>
#include <string.h>

ERL_NIF_TERM ATOM_FILTER_MATCH;
ERL_NIF_TERM ATOM_TYPE;
ERL_NIF_TERM ATOM_DESTINATION;
ERL_NIF_TERM ATOM_INTERFACE;
ERL_NIF_TERM ATOM_PATH;
ERL_NIF_TERM ATOM_MEMBER;

static int
ebus_filter_match_string(const char * filter_str, const char * msg_str)
{
    if (filter_str != NULL && msg_str != NULL)
    {
        return strcmp(filter_str, msg_str) == 0;
    }
    return FALSE;
}

static int
ebus_filter_matches(ebus_filter * filter, DBusMessage * message)
{
    if (filter->type != DBUS_MESSAGE_TYPE_INVALID
        && dbus_message_get_type(message) != filter->type)
    {
        return FALSE;
    }

    if (filter->destination
        && !ebus_filter_match_string(filter->destination,
                                     dbus_message_get_destination(message)))
    {
        return FALSE;
    }

    if (filter->path
        && !ebus_filter_match_string(filter->path, dbus_message_get_path(message)))
    {
        return FALSE;
    }

    if (filter->interface && !ebus_filter_match_string(filter->interface, dbus_message_get_interface(message)))
    {
        return FALSE;
    }

    if (filter->member
        && !ebus_filter_match_string(filter->member, dbus_message_get_member(message)))
    {
        return FALSE;
    }

    return TRUE;
}

DBusHandlerResult
cb_filter_handle_message(DBusConnection * connection, DBusMessage * message, void * data)
{
    dbus_connection * state   = (dbus_connection *)data;
    ebus_filter *     filters = state->filters;

    if (!filters)
    {
        return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    }

    ebus_filter * filter = filters;
    while (filter->id != EBUS_FILTER_ID_END)
    {
        if (ebus_filter_matches(filter, message))
        {
            dbus_message_ref(message);
            ErlNifEnv *  msg_env = enif_alloc_env();
            ERL_NIF_TERM msg     = enif_make_tuple3(msg_env,
                                                ATOM_FILTER_MATCH,
                                                enif_make_uint(msg_env, filter->id),
                                                mk_dbus_message(msg_env, message));
            enif_send(NULL, &state->handler, msg_env, msg);
            enif_free_env(msg_env);
            return DBUS_HANDLER_RESULT_HANDLED;
        }
        filter += 1;
    }
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

ebus_filter *
ebus_filters_free(ebus_filter * filters)
{
    if (!filters)
    {
        return NULL;
    }

    ebus_filter * filter = filters;
#define FFREE(F)                                                                         \
    if (filter->F)                                                                       \
    {                                                                                    \
        enif_free((void *)filter->F);                                                    \
        filter->F = NULL;                                                                \
    }
    while (filter->id != EBUS_FILTER_ID_END)
    {
        FFREE(destination);
        FFREE(path);
        FFREE(interface);
        FFREE(member);
        filter += 1;
    }
#undef FFREE
    enif_free(filters);
    return NULL;
}

static int
mk_ebus_filter_string(ErlNifEnv * env, ERL_NIF_TERM map, ERL_NIF_TERM key, const char ** dest)
{
    ERL_NIF_TERM term;
    if (!enif_get_map_value(env, map, key, &term))
    {
        *dest = NULL;
        return TRUE;
    }

    unsigned str_len;
    if (!enif_get_list_length(env, term, &str_len))
        return FALSE;

    char * str = enif_alloc(str_len + 1);
    if (!str)
        return FALSE;

    if (enif_get_string(env, term, str, str_len + 1, ERL_NIF_LATIN1) == 0)
    {
        enif_free(str);
        return FALSE;
    }

    *dest = str;
    return TRUE;
}

static int
mk_ebus_filter(ErlNifEnv * env, ERL_NIF_TERM filter, ebus_filter * dest)
{
    const ERL_NIF_TERM * filter_tuple;
    int                  arity;
    if (!enif_get_tuple(env, filter, &arity, &filter_tuple) || arity != 2)
        return FALSE;

    ERL_NIF_TERM ref = filter_tuple[0];
    ERL_NIF_TERM map = filter_tuple[1];

    if (!enif_get_uint(env, ref, &dest->id))
        return FALSE;

    dest->type = DBUS_MESSAGE_TYPE_INVALID;
    ERL_NIF_TERM term;
    if (enif_get_map_value(env, map, ATOM_TYPE, &term)
        && !get_dbus_message_type(env, term, &dest->type))
        return FALSE;

    if (!mk_ebus_filter_string(env, map, ATOM_DESTINATION, &dest->destination))
        return FALSE;

    if (!mk_ebus_filter_string(env, map, ATOM_PATH, &dest->path))
        return FALSE;

    if (!mk_ebus_filter_string(env, map, ATOM_INTERFACE, &dest->interface))
        return FALSE;

    if (!mk_ebus_filter_string(env, map, ATOM_MEMBER, &dest->member))
        return FALSE;

    return TRUE;
}

ERL_NIF_TERM
mk_ebus_filters(ErlNifEnv * env, ERL_NIF_TERM filters, ebus_filter ** dest)
{
    unsigned filters_len;
    if (!enif_get_list_length(env, filters, &filters_len))
    {
        return enif_make_badarg(env);
    }

    ebus_filter * new_filters = enif_alloc((filters_len + 1) * sizeof(ebus_filter));
    if (!new_filters)
    {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    ERL_NIF_TERM filter;
    for (int i = 0; i < filters_len; i++)
    {
        enif_get_list_cell(env, filters, &filter, &filters);
        mk_ebus_filter(env, filter, &new_filters[i]);
    }
    new_filters[filters_len].id = EBUS_FILTER_ID_END;

    ebus_filters_free(*dest);
    *dest = new_filters;

    return ATOM_OK;
}

void
ebus_filter_load(ErlNifEnv * env)
{
    ATOM(ATOM_TYPE, "type");
    ATOM(ATOM_DESTINATION, "destination");
    ATOM(ATOM_PATH, "path");
    ATOM(ATOM_INTERFACE, "interface");
    ATOM(ATOM_MEMBER, "member");
    ATOM(ATOM_FILTER_MATCH, "filter_match");
}
