-define(DBUS_OBJECT_MANAGER_IFACE, "org.freedesktop.DBus.ObjectManager").
-define(DBUS_OBJECT_MANAGER(M), ?DBUS_OBJECT_MANAGER_IFACE ++ "." ++ M).

-define(DBUS_PROPETIES_INTERFACE, "org.freedesktop.DBus.Properties").
-define(DBUS_PROPERTIES(M), ?DBUS_PROPETIES_INTERFACE ++ "." ++ M).

-define(DBUS_ERROR_FAILED, "org.freedesktop.DBus.Error.Failed").
-define(DBUS_ERROR_NOT_SUPPORTED, "org.freedesktop.DBus.Error.NotSupported").
