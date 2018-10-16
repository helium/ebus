#include "ebus_message_append_arg.h"
#include <string.h>

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

static __inline char *
append_type_string(char * dest, char * str, size_t * size_dest)
{
    strncat(dest, str, *size_dest);
    int str_len = strlen(str);
    *size_dest  = str_len > *size_dest ? 0 : *size_dest - str_len;
    return dest;
}

static int
infer_type_number(ErlNifEnv * env, ERL_NIF_TERM term, char * dest, size_t * size_dest)
{
#define SET_TYPE(T)                                                                      \
    {                                                                                    \
        append_type_string(dest, T, size_dest);                                          \
        return TRUE;                                                                     \
    }
    DBusBasicValue v;
    if (enif_get_int(env, term, &v.i32))
    {
        if (v.i32 >= 0x00 && v.i32 <= 0xff)
        {
            SET_TYPE(DBUS_TYPE_BYTE_AS_STRING);
        }
        else if (v.i32 >= -0x8000 && v.i32 <= 0x7fff)
        {
            SET_TYPE(DBUS_TYPE_INT16_AS_STRING);
        }
        else if (v.i32 >= 0 && v.i32 <= 0xffff)
        {
            SET_TYPE(DBUS_TYPE_UINT16_AS_STRING);
        }
        SET_TYPE(DBUS_TYPE_INT32_AS_STRING);
    }
    else if (enif_get_uint(env, term, &v.u32))
    {
        SET_TYPE(DBUS_TYPE_UINT32_AS_STRING);
    }
    else if (enif_get_int64(env, term, &v.i64))
    {
        SET_TYPE(DBUS_TYPE_INT64_AS_STRING);
    }
    else if (enif_get_uint64(env, term, &v.u64))
    {
        SET_TYPE(DBUS_TYPE_UINT64_AS_STRING);
    }
    else if (enif_get_double(env, term, &v.dbl))
    {
        SET_TYPE(DBUS_TYPE_DOUBLE_AS_STRING);
    }
    return FALSE;
#undef SET_TYPE
}

static int
infer_type_list(ErlNifEnv * env, ERL_NIF_TERM term, char * dest, size_t * size_dest)
{
    char str[1];
    if (enif_get_string(env, term, str, 1, ERL_NIF_LATIN1) != 0)
    {
        append_type_string(dest, DBUS_TYPE_STRING_AS_STRING, size_dest);
        return TRUE;
    }

    // Guess the signature based on the first element in the list
    ERL_NIF_TERM head, tail;
    enif_get_list_cell(env, term, &head, &tail);

    append_type_string(dest, DBUS_TYPE_ARRAY_AS_STRING, size_dest);
    return ebus_message_infer_type(env, head, dest, size_dest);
}

static int
infer_type_map(ErlNifEnv * env, ERL_NIF_TERM term, char * dest, size_t * size_dest)
{
    append_type_string(dest,
                       DBUS_TYPE_ARRAY_AS_STRING DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING,
                       size_dest);

    size_t term_size;
    enif_get_map_size(env, term, &term_size);
    if (term_size == 0)
    {
        // Empty map, guess type "vv"(?)
        append_type_string(dest,
                           DBUS_TYPE_VARIANT_AS_STRING DBUS_TYPE_VARIANT_AS_STRING,
                           size_dest);
    }
    else
    {
        // Guess the type of the map by infering the types of the first pair
        ERL_NIF_TERM      key, value;
        ErlNifMapIterator iter;
        enif_map_iterator_create(env, term, &iter, ERL_NIF_MAP_ITERATOR_FIRST);
        enif_map_iterator_get_pair(env, &iter, &key, &value);
        enif_map_iterator_destroy(env, &iter);

        if (!ebus_message_infer_type(env, key, dest, size_dest))
        {
            return FALSE;
        }
        if (!ebus_message_infer_type(env, value, dest, size_dest))
        {
            return FALSE;
        }
    }

    append_type_string(dest, DBUS_DICT_ENTRY_END_CHAR_AS_STRING, size_dest);
    return TRUE;
}

static int
infer_type_tuple(ErlNifEnv * env, ERL_NIF_TERM term, char * dest, size_t * size_dest)
{
    append_type_string(dest, DBUS_STRUCT_BEGIN_CHAR_AS_STRING, size_dest);

    int                  arity;
    const ERL_NIF_TERM * array;
    enif_get_tuple(env, term, &arity, &array);

    if (arity == 0)
    {
        // Structs can't be empty in dbus
        return FALSE;
    }

    for (int i = 0; i < arity; i++)
    {
        if (!ebus_message_infer_type(env, array[i], dest, size_dest))
        {
            return FALSE;
        }
    }

    append_type_string(dest, DBUS_STRUCT_END_CHAR_AS_STRING, size_dest);
    return TRUE;
}


int
ebus_message_infer_type(ErlNifEnv * env, ERL_NIF_TERM term, char * dest, size_t * size_dest)
{
    if (term == ATOM_TRUE || term == ATOM_FALSE)
    {
        append_type_string(dest, DBUS_TYPE_BOOLEAN_AS_STRING, size_dest);
        return TRUE;
    }
    else if (enif_is_number(env, term))
    {
        return infer_type_number(env, term, dest, size_dest);
    }
    else if (enif_is_binary(env, term))
    {
        append_type_string(dest,
                           DBUS_TYPE_ARRAY_AS_STRING DBUS_TYPE_BYTE_AS_STRING,
                           size_dest);
        return TRUE;
    }
    else if (enif_is_list(env, term))
    {
        return infer_type_list(env, term, dest, size_dest);
    }
    else if (enif_is_map(env, term))
    {
        return infer_type_map(env, term, dest, size_dest);
    }
    else if (enif_is_tuple(env, term))
    {
        return infer_type_tuple(env, term, dest, size_dest);
    }

    return FALSE;
}

static int
iter_append_variant(ErlNifEnv *               env,
                    ERL_NIF_TERM              term,
                    DBusMessageIter *         appender,
                    const DBusSignatureIter * sig_iter)
{
    char sig[DBUS_MAXIMUM_SIGNATURE_LENGTH];
    sig[0]          = DBUS_TYPE_INVALID;
    size_t sig_size = DBUS_MAXIMUM_SIGNATURE_LENGTH;

    if (!ebus_message_infer_type(env, term, sig, &sig_size))
    {
        return FALSE;
    }

    DBusSignatureIter sub_sig_iter;
    dbus_signature_iter_init(&sub_sig_iter, sig);

    int             ret = FALSE;
    DBusMessageIter sub_appender;
    if (!dbus_message_iter_open_container(appender, DBUS_TYPE_VARIANT, sig, &sub_appender))
    {
        goto out;
    }

    ret = ebus_message_append_arg(env, term, &sub_appender, &sub_sig_iter, NULL);

    ret = iter_close_container(appender, &sub_appender, (ret == TRUE));

out:
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
        }
        else
        {
            ret = FALSE;
            break;
        }
        ret = dbus_message_iter_append_basic(appender, sig_type, &v.bool_val);
        break;
    case DBUS_TYPE_DOUBLE:
        if (enif_get_double(env, term, &v.dbl))
        {
            ret = dbus_message_iter_append_basic(appender, sig_type, &v.dbl);
        }
        else if (enif_get_int64(env, term, &v.i64))
        {
            v.dbl = v.i64;
            ret   = dbus_message_iter_append_basic(appender, sig_type, &v.dbl);
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
    case DBUS_TYPE_VARIANT:
        ret = iter_append_variant(env, term, appender, sig_iter);
    }

    if (ret == TRUE && more != NULL)
    {
        *more = dbus_signature_iter_next(sig_iter);
    }

    return ret;
}
