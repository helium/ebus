-define(BUS_SESSION, 0).
-define(BUS_SYSTEM, 1).
-define(BUS_STARTER, 2).


%%
%% Owner Flags

%% @doc Allow another service to become the primary owner if requested.
-define(BUS_NAME_FLAG_ALLOW_REPLACEMENT, 1).
%% @doc Request to replace the current primary owner.
-define(BUS_NAME_FLAG_REPLACE_EXISTING,  2).
%% @doc If we can not become the primary owner do not place us in the queue.
-define(BUS_NAME_FLAG_DO_NOT_QUEUE,      4).

%%
%% Replies to a request for a name

%% @doc  Service has become the primary owner of the requested name.
-define(BUS_REQUEST_NAME_REPLY_PRIMARY_OWNER,  1).
%% @doc  Service could not become the primary owner and has been placed in the queue.
-define(BUS_REQUEST_NAME_REPLY_IN_QUEUE,       2).
%% @doc Service is already in the queue.
-define(BUS_REQUEST_NAME_REPLY_EXISTS,         3).
%% @doc Service is already the primary owner.
-define(BUS_REQUEST_NAME_REPLY_ALREADY_OWNER,  4).

%%
%% Replies to releasing a name.

%% @doc Service was released from the given name.
-define(BUS_RELEASE_NAME_REPLY_RELEASED,       1).
%% @doc The given name does not exist on the bus.
-define(BUS_RELEASE_NAME_REPLY_NON_EXISTENT,   2).
%% @doc Service is not an owner of the given name.
-define(BUS_RELEASE_NAME_REPLY_NOT_OWNER,      3).
