#include "ebus_message_append_arg.h"

static int
iter_close_container(DBusMessageIter * iter, DBusMessageIter * sub, int is_ok)
{
    if (!is_ok)
    {
        dbus_message_iter_abandon_container(iter, sub);
        return is_ok;
    }
    return dbus_message_iter_close_container(iter, sub);
}

static int
iter_append_list(ErlNifEnv *               env,
                 ERL_NIF_TERM              term_list,
                 DBusMessageIter *         appender,
                 const DBusSignatureIter * sig_iter)
{
    DBusMessageIter   sub_appender;
    DBusSignatureIter sub_sig_iter;
    char *            sig = NULL;
    int               ret = FALSE;

    if (!enif_is_list(env, term_list))
    {
        goto out;
    }

    dbus_signature_iter_recurse(sig_iter, &sub_sig_iter);
    sig = dbus_signature_iter_get_signature(&sub_sig_iter);
    if (sig == NULL)
    {
        goto out;
    }

    if (!dbus_message_iter_open_container(appender, DBUS_TYPE_ARRAY, sig, &sub_appender))
    {
        goto out;
    }

    ERL_NIF_TERM term;
    while (enif_get_list_cell(env, term_list, &term, &term_list))
    {
        // reset the signature iterator and append the term. We ignore
        // the "more" result since list entries are of a single
        // complete type.
        dbus_signature_iter_recurse(sig_iter, &sub_sig_iter);
        ret = ebus_message_append_arg(env, term, &sub_appender, &sub_sig_iter, NULL);
        if (!ret)
        {
            break;
        }
    }

    ret = iter_close_container(appender, &sub_appender, (ret == TRUE));

out:
    dbus_free(sig);
    return ret;
}

static int
iter_append_struct(ErlNifEnv *               env,
                   ERL_NIF_TERM              tuple,
                   DBusMessageIter *         appender,
                   const DBusSignatureIter * sig_iter)
{
    DBusMessageIter   sub_appender;
    DBusSignatureIter sub_sig_iter;
    int               ret = FALSE;

    if (!enif_is_tuple(env, tuple))
    {
        goto out;
    }

    dbus_signature_iter_recurse(sig_iter, &sub_sig_iter);

    if (!dbus_message_iter_open_container(appender, DBUS_TYPE_STRUCT, NULL, &sub_appender))
    {
        goto out;
    }

    int                  arity;
    const ERL_NIF_TERM * entries;
    enif_get_tuple(env, tuple, &arity, &entries);

    dbus_bool_t more = FALSE;
    for (int i = 0; i <= arity - 1; i++)
    {
        ret = ebus_message_append_arg(env, entries[i], &sub_appender, &sub_sig_iter, &more);
        if (!ret)
        {
            break;
        }
        if (more && i == arity)
        {
            // No more in terms but signature declares more
            // {error, struct_type_mismatch}
            ret = FALSE;
            break;
        }
    }
    if (more)
    {
        // More in terms but no  more in signature
        // {error, struct_type_mismatch}
        ret = FALSE;
    }

    ret = iter_close_container(appender, &sub_appender, (ret == TRUE));

out:
    return ret;
}


static int
iter_append_dict_entry(ErlNifEnv *               env,
                       ERL_NIF_TERM              key,
                       ERL_NIF_TERM              value,
                       DBusMessageIter *         appender,
                       const DBusSignatureIter * sig_iter)
{
    DBusSignatureIter sub_sig_iter;
    DBusMessageIter   sub_appender;
    int               ret = FALSE;

    dbus_signature_iter_recurse(sig_iter, &sub_sig_iter);
    if (!dbus_message_iter_open_container(appender, DBUS_TYPE_DICT_ENTRY, NULL, &sub_appender))
    {
        goto out;
    }

    ret = ebus_message_append_arg(env, key, &sub_appender, &sub_sig_iter, NULL);
    if (ret == TRUE)
    {
        dbus_signature_iter_next(&sub_sig_iter);
        ret = ebus_message_append_arg(env, value, &sub_appender, &sub_sig_iter, NULL);
    }

    ret = iter_close_container(appender, &sub_appender, (ret == TRUE));

out:
    return ret;
}

static int
iter_append_map(ErlNifEnv *               env,
                ERL_NIF_TERM              map,
                DBusMessageIter *         appender,
                const DBusSignatureIter * sig_iter)
{
    DBusMessageIter   sub_appender;
    DBusSignatureIter sub_sig_iter;
    char *            sig = NULL;
    int               ret = FALSE;

    if (!enif_is_map(env, map))
    {
        goto out;
    }

    dbus_signature_iter_recurse(sig_iter, &sub_sig_iter);
    sig = dbus_signature_iter_get_signature(&sub_sig_iter);
    if (sig == NULL)
    {
        goto out;
    }

    if (!dbus_message_iter_open_container(appender, DBUS_TYPE_ARRAY, sig, &sub_appender))
    {
        goto out;
    }

    ErlNifMapIterator map_iter;
    enif_map_iterator_create(env, map, &map_iter, ERL_NIF_MAP_ITERATOR_FIRST);

    ERL_NIF_TERM key;
    ERL_NIF_TERM value;
    ret = TRUE;
    while (enif_map_iterator_get_pair(env, &map_iter, &key, &value))
    {
        // reset the signature iterator
        dbus_signature_iter_recurse(sig_iter, &sub_sig_iter);
        ret = iter_append_dict_entry(env, key, value, &sub_appender, &sub_sig_iter);
        if (!ret)
        {
            break;
        }
        enif_map_iterator_next(env, &map_iter);
    }

    enif_map_iterator_destroy(env, &map_iter);
    ret = iter_close_container(appender, &sub_appender, (ret == TRUE));

out:
    dbus_free(sig);
    return ret;
}

static int
iter_append_string(ErlNifEnv * env, ERL_NIF_TERM term, DBusMessageIter * appender, int sig_type)
{
    GET_STR(str, term);
    // in C, given "int array[]", "&array == array" (the comp.lang.c
    // FAQ says otherwise, but gcc and the FAQ don't agree). So if
    // you're using an array instead of a pointer you have to create a
    // pointer variable, assign the array to it, then take the address
    // of the pointer variable. For strings it works to write const
    // char *array = "Hello" and then use &array though. But not when
    // they're declared like an array of characters.
    const char * str_ptr = str;
    return dbus_message_iter_append_basic(appender, sig_type, &str_ptr);
}

static int
iter_append_binary(ErlNifEnv * env, ERL_NIF_TERM term, DBusMessageIter * appender)
{
    DBusMessageIter sub_appender;
    int             ret = FALSE;
    ErlNifBinary    bin;
    if (!enif_inspect_binary(env, term, &bin))
    {
        goto out;
    }

    const char bin_signature[2] = {DBUS_TYPE_BYTE, DBUS_TYPE_INVALID};
    if (!dbus_message_iter_open_container(appender,
                                          DBUS_TYPE_ARRAY,
                                          bin_signature,
                                          &sub_appender))
    {
        goto out;
    }


    ret = dbus_message_iter_append_fixed_array(&sub_appender,
                                               DBUS_TYPE_BYTE,
                                               &bin.data,
                                               bin.size);

    ret = iter_close_container(appender, &sub_appender, (ret == TRUE));

out:
    return ret;
}


int
ebus_message_append_arg(ErlNifEnv *         env,
                        ERL_NIF_TERM        term,
                        DBusMessageIter *   appender,
                        DBusSignatureIter * sig_iter,
                        dbus_bool_t *       more)
{
    int            sig_type = dbus_signature_iter_get_current_type(sig_iter);
    DBusBasicValue v;
    int            ret = FALSE;

    switch (sig_type)
    {
    case DBUS_TYPE_BOOLEAN:
        if (term == ATOM_TRUE)
        {
            v.bool_val = 1;
        }
        else if (term == ATOM_FALSE)
        {
            v.bool_val = 0;
        } else {
            ret = FALSE;
            break;
        }
        ret = dbus_message_iter_append_basic(appender, sig_type, &v.bool_val);
        break;
    case DBUS_TYPE_DOUBLE:
        if (enif_get_double(env, term, &v.dbl))
        {
            ret = dbus_message_iter_append_basic(appender, sig_type, &v.dbl);
        } else if (enif_get_int64(env, term, &v.i64)) {
            v.dbl = v.i64;
            ret = dbus_message_iter_append_basic(appender, sig_type, &v.dbl);
        }
        break;
    case DBUS_TYPE_BYTE:
        if (enif_get_int(env, term, &v.i32))
        {
            if (v.i32 >= 0x00 && v.i32 <= 0xff)
            {
                v.byt = v.i32;
                ret   = dbus_message_iter_append_basic(appender, sig_type, &v.byt);
            }
        }
        break;
    case DBUS_TYPE_INT16:
        if (enif_get_int(env, term, &v.i32))
        {
            if (v.i32 >= -0x8000 && v.i32 <= 0x7fff)
            {
                v.i16 = v.i32;
                ret   = dbus_message_iter_append_basic(appender, sig_type, &v.i16);
            }
        }
        break;
    case DBUS_TYPE_INT32:
        if (enif_get_int(env, term, &v.i32))
        {
            ret = dbus_message_iter_append_basic(appender, sig_type, &v.i32);
        }
        break;
    case DBUS_TYPE_UINT16:
        if (enif_get_uint(env, term, &v.u32))
        {
            if (v.u32 <= 0xffff)
            {
                v.u16 = v.u32;
                ret   = dbus_message_iter_append_basic(appender, sig_type, &v.u16);
            }
        }
        break;
    case DBUS_TYPE_UINT32:
        if (enif_get_uint(env, term, &v.u32))
        {
            ret = dbus_message_iter_append_basic(appender, sig_type, &v.u32);
        }
        break;
    case DBUS_TYPE_INT64:
        if (enif_get_int64(env, term, &v.i64))
        {
            ret = dbus_message_iter_append_basic(appender, sig_type, &v.i64);
        }
        break;
    case DBUS_TYPE_UINT64:
        if (enif_get_uint64(env, term, &v.u64))
        {
            ret = dbus_message_iter_append_basic(appender, sig_type, &v.u64);
        }
        break;
    case DBUS_TYPE_STRING:
    case DBUS_TYPE_SIGNATURE:
    case DBUS_TYPE_OBJECT_PATH:
        ret = iter_append_string(env, term, appender, sig_type);
        break;
    case DBUS_TYPE_ARRAY:
        sig_type = dbus_signature_iter_get_element_type(sig_iter);
        if (sig_type == DBUS_TYPE_DICT_ENTRY)
        {
            ret = iter_append_map(env, term, appender, sig_iter);
        }
        else if (sig_type == DBUS_TYPE_BYTE)
        {
            ret = iter_append_binary(env, term, appender);
        }
        else
        {
            ret = iter_append_list(env, term, appender, sig_iter);
        }
        break;
    case DBUS_TYPE_STRUCT:
        ret = iter_append_struct(env, term, appender, sig_iter);
        break;
    }

    if (ret == TRUE && more != NULL)
    {
        *more = dbus_signature_iter_next(sig_iter);
    }

    return ret;
}
