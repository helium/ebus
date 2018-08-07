# ebus

The `ebus` library provides an Erlang binding for the canonical
[libdbus](https://cgit.freedesktop.org/dbus/dbus/) IPC mechanism.


Install
------

Add `ebus` to your `deps` section in `rebar.config`:

``` shell
{deps, [ebus]}.
```

Building
--------

Fork the repo and simply use `make` to build the library. You will
need the `libdbus` development libraries and headers installed on your
system.

To run the tests run `make test`.


Usage
-----
