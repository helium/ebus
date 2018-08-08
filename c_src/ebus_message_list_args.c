#include "ebus_message_list_args.h"
#include <string.h>

static int
ebus_message_get_arg(ErlNifEnv * env, DBusMessageIter * iter, ERL_NIF_TERM * dest);


static int
iter_get_list(ErlNifEnv * env, DBusMessageIter * iter, ERL_NIF_TERM * dest)
{
    DBusMessageIter sub_iter;
    ERL_NIF_TERM    list = enif_make_list(env, 0);
    int             type;

    dbus_message_iter_recurse(iter, &sub_iter);

    while ((type = dbus_message_iter_get_arg_type(&sub_iter)) != DBUS_TYPE_INVALID)
    {
        ERL_NIF_TERM entry;

        if (!ebus_message_get_arg(env, &sub_iter, &entry))
        {
            *dest = entry;
            return FALSE;
        }

        list = enif_make_list_cell(env, entry, list);

        dbus_message_iter_next(&sub_iter);
    }

    enif_make_reverse_list(env, list, dest);
    return TRUE;
}

static int
iter_get_struct(ErlNifEnv * env, DBusMessageIter * iter, ERL_NIF_TERM * dest)
{
    ERL_NIF_TERM list;

    if (!iter_get_list(env, iter, &list))
    {
        *dest = list;
        return FALSE;
    }

    unsigned list_len;
    enif_get_list_length(env, list, &list_len);

    ERL_NIF_TERM *term = enif_alloc(sizeof(ERL_NIF_TERM) * list_len);
    if (term == NULL) {
        *dest = ATOM_ENOMEM;
        return FALSE;
    }

    *dest = enif_make_tuple_from_array(env, term, list_len);

    enif_free(term);
    return TRUE;
}

static int
iter_get_binary(ErlNifEnv * env, DBusMessageIter * iter, ERL_NIF_TERM * dest)
{
    DBusMessageIter sub_iter;
    int size;
    void *data;

    dbus_message_iter_recurse(iter, &sub_iter);
    dbus_message_iter_get_fixed_array(&sub_iter, &data, &size);

    ErlNifBinary    bin;
    if (!enif_alloc_binary(size, &bin)) {
        *dest = ATOM_ENOMEM;
        return FALSE;
    }

    memcpy(bin.data, data, size);
    *dest = enif_make_binary(env, &bin);
    return TRUE;
}

static int
iter_get_map(ErlNifEnv * env, DBusMessageIter * iter, ERL_NIF_TERM * dest)
{
    DBusMessageIter entries;
    ERL_NIF_TERM    map = enif_make_new_map(env);

    dbus_message_iter_recurse(iter, &entries);
    while (dbus_message_iter_get_arg_type(&entries) == DBUS_TYPE_DICT_ENTRY)
    {
        ERL_NIF_TERM    key;
        ERL_NIF_TERM    value;
        DBusMessageIter kv;
        dbus_message_iter_recurse(&entries, &kv);

        if (!ebus_message_get_arg(env, &kv, &key))
        {
            *dest = key;
            return FALSE;
        }

        if (!ebus_message_get_arg(env, &kv, &value))
        {
            *dest = value;
            return FALSE;
        }

        enif_make_map_put(env, map, key, value, &map);

        dbus_message_iter_next(&entries);
    }

    *dest = map;
    return TRUE;
}

static int
ebus_message_get_arg(ErlNifEnv * env, DBusMessageIter * iter, ERL_NIF_TERM * dest)
{
    DBusBasicValue v;
    int            sig_type = dbus_message_iter_get_arg_type(iter);

    switch (sig_type)
    {
    case DBUS_TYPE_BOOLEAN:
        dbus_message_iter_get_basic(iter, &v.bool_val);
        *dest = v.bool_val ? ATOM_TRUE : ATOM_FALSE;
        return TRUE;
    case DBUS_TYPE_DOUBLE:
        dbus_message_iter_get_basic(iter, &v.dbl);
        *dest = enif_make_double(env, v.dbl);
        return TRUE;
    case DBUS_TYPE_BYTE:
        dbus_message_iter_get_basic(iter, &v.byt);
        *dest = enif_make_int(env, v.byt);
        return TRUE;
    case DBUS_TYPE_INT16:
        dbus_message_iter_get_basic(iter, &v.i16);
        *dest = enif_make_int(env, v.i16);
        return TRUE;
    case DBUS_TYPE_INT32:
        dbus_message_iter_get_basic(iter, &v.i32);
        *dest = enif_make_int(env, v.i32);
        return TRUE;
    case DBUS_TYPE_UINT16:
        dbus_message_iter_get_basic(iter, &v.u16);
        *dest = enif_make_uint(env, v.u16);
        return TRUE;
    case DBUS_TYPE_UINT32:
        dbus_message_iter_get_basic(iter, &v.u32);
        *dest = enif_make_uint(env, v.u32);
        return TRUE;
    case DBUS_TYPE_INT64:
        dbus_message_iter_get_basic(iter, &v.i64);
        *dest = enif_make_int64(env, v.i64);
        return TRUE;
    case DBUS_TYPE_UINT64:
        dbus_message_iter_get_basic(iter, &v.i64);
        *dest = enif_make_uint64(env, v.i64);
        return TRUE;
    case DBUS_TYPE_STRING:
    case DBUS_TYPE_SIGNATURE:
    case DBUS_TYPE_OBJECT_PATH:
        dbus_message_iter_get_basic(iter, &v.str);
        *dest = enif_make_string(env, v.str, ERL_NIF_LATIN1);
        return TRUE;
    case DBUS_TYPE_ARRAY:
        sig_type = dbus_message_iter_get_element_type(iter);
        if (sig_type == DBUS_TYPE_DICT_ENTRY)
        {
            return iter_get_map(env, iter, dest);
        }
        else if (sig_type == DBUS_TYPE_BYTE)
        {
            return iter_get_binary(env, iter, dest);
        }
        else
        {
            return iter_get_list(env, iter, dest);
        }
    case DBUS_TYPE_STRUCT:
        return iter_get_struct(env, iter, dest);
    }

    return FALSE;
}


ERL_NIF_TERM
ebus_message_list_args(ErlNifEnv * env, DBusMessage * message)
{
    ERL_NIF_TERM    list = enif_make_list(env, 0);
    DBusMessageIter iter;

    if (!dbus_message_iter_init(message, &iter))
    {
        return list;
    }

    int current_type;
    while ((current_type = dbus_message_iter_get_arg_type(&iter)) != DBUS_TYPE_INVALID)
    {
        ERL_NIF_TERM term;
        if (!ebus_message_get_arg(env, &iter, &term))
        {
            return enif_make_tuple2(env, ATOM_ERROR, term);
        }
        else
        {
            list = enif_make_list_cell(env, term, list);
        }
        dbus_message_iter_next(&iter);
    }

    ERL_NIF_TERM out_list;
    enif_make_reverse_list(env, list, &out_list);
    return enif_make_tuple2(env, ATOM_OK, out_list);
}
