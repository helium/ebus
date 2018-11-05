-define(DBUS_OBJECT_MANAGER_IFACE, "org.freedesktop.DBus.ObjectManager").
-define(DBUS_OBJECT_MANAGER(M), ?DBUS_OBJECT_MANAGER_IFACE ++ "." ++ M).

-define(DBUS_PROPETIES_INTERFACE, "org.freedesktop.DBus.Properties").
-define(DBUS_PROPERTIES(M), ?DBUS_PROPETIES_INTERFACE ++ "." ++ M).
