# erl-dbus

The `erl-dbus` library provides an Erlang binding for the canonical
[libdbus](https://cgit.freedesktop.org/dbus/dbus/) IPC mechanism.


Install
------

Add `erl-dbus` to your `deps` section in `rebar.config`:

``` shell
{deps, [erl-dbus]}.
```

Building
--------

Fork the repo and simply use `make` to build the library. You will
need `cmake` installed to build the required libdbus package.

To run the tests run `make test`.


Usage
-----
