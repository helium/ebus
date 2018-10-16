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

#include "ebus_connection.h"
#include "ebus_message.h"
#include "ebus_shared.h"
#include "ebus_error.h"
#include "ebus_timeout.h"
#include "ebus_watch.h"
#include "ebus_object.h"

ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_ENOMEM;
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_UNDEFINED;
ERL_NIF_TERM ATOM_ALREADY;

static ErlNifFunc nif_funcs[] = {
    {"connection_get", 2, ebus_connection_get, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"connection_close", 1, ebus_connection_close, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"connection_unique_name", 1, ebus_connection_unique_name, 0},
    {"connection_request_name", 3, ebus_connection_request_name, 0},
    {"connection_release_name", 2, ebus_connection_release_name, 0},
    {"connection_add_match", 2, ebus_connection_add_match, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"connection_send", 2, ebus_connection_send, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"connection_call", 4, ebus_connection_call, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"connection_dispatch", 1, ebus_connection_dispatch, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"connection_set_filters", 2, ebus_connection_set_filters, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"connection_register_object_path", 3, ebus_connection_register_object_path, 0},
    {"connection_unregister_object_path", 2, ebus_connection_unregister_object_path, 0},

    {"message_new_signal", 3, ebus_message_new_signal, 0},
    {"message_new_call", 4, ebus_message_new_call, 0},
    {"message_new_reply", 1, ebus_message_new_reply, 0},
    {"message_append_args", 3, ebus_message_append_args, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"message_get_args", 1, ebus_message_get_args, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"message_get_type", 1, ebus_message_get_type, 0},
    {"message_get_serial", 1, ebus_message_get_serial, 0},
    {"message_set_serial", 2, ebus_message_set_serial, 0},
    {"message_get_reply_serial", 1, ebus_message_get_reply_serial, 0},
    {"message_get_destination", 1, ebus_message_get_destination, 0},
    {"message_get_path", 1, ebus_message_get_path, 0},
    {"message_get_interface", 1, ebus_message_get_interface, 0},
    {"message_get_member", 1, ebus_message_get_member, 0},
    {"message_get_error", 1, ebus_message_get_error, 0},
    {"message_infer_signature", 1, ebus_message_infer_signature, 0},

    {"watch_handle", 2, ebus_watch_handle, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"timeout_handle", 1, ebus_timeout_handle, ERL_NIF_DIRTY_JOB_CPU_BOUND},
};

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;


    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error");
    ATOM(ATOM_ENOMEM, "enomem");
    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");
    ATOM(ATOM_UNDEFINED, "undefined");
    ATOM(ATOM_ALREADY, "already");

    ebus_connection_load(env);
    ebus_message_load(env);
    ebus_watch_load(env);
    ebus_timeout_load(env);
    ebus_filter_load(env);
    ebus_object_load(env);
    ebus_error_load(env);

    return 0;
}

ERL_NIF_INIT(ebus_nif, nif_funcs, load, NULL, NULL, NULL);
