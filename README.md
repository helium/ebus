[![Travis][travis badge]][travis]
[![Hex.pm Version][hex version badge]][hex]
[![Hex.pm License][hex license badge]][hex]
[![Erlang Versions][erlang version badge]][erlang]
[![Build Tool][build tool]][rebar]

# ebus


The `ebus` library provides an Erlang binding for the canonical
[libdbus](https://cgit.freedesktop.org/dbus/dbus/) IPC mechanism.


  * [Features](#features)
  * [Install](#install)
  * [Examples](#examples)
  * [Building](#building)


## Features

  * A native binding to libdbus
  * A callback based service implemenatation scaffold to make it easy
    to implement a dbus service
  * A proxy object to make it easy to call a dbus service

## Install

Add `ebus` to your `deps` section in `rebar.config`:

``` shell
{deps, [ebus]}.
```

## Examples

Here's an example of talking to [connman] to get the current list of
connectivity related technologies:

```erlang
Eshell V10.1  (abort with ^G)E
1>  {ok, B} = ebus:start(system).
{ok,<0.221.0>}
2>  {ok, P} = ebus_proxy:start(B, "net.connman", "/", []).
{ok,<0.224.0>}
3> ebus_proxy:call(P, "net.connman.Manager.GetTechnologies").
{ok,[[{"/net/connman/technology/p2p",
       #{"Connected" => false,"Name" => "P2P","Powered" => false,
         "Tethering" => false,"Type" => "p2p"}},
      {"/net/connman/technology/ethernet",
       #{"Connected" => false,"Name" => "Wired","Powered" => true,
         "Tethering" => false,"Type" => "ethernet"}},
      {"/net/connman/technology/wifi",
       #{"Connected" => true,"Name" => "WiFi","Powered" => true,
         "Tethering" => false,"Type" => "wifi"}},
      {"/net/connman/technology/bluetooth",
       #{"Connected" => false,"Name" => "Bluetooth",
         "Powered" => false,"Tethering" => false,
         "Type" => "bluetooth"}}]]}
```

## Building

Fork the repo and simply use `make` to build the library. You will
need the `libdbus` development libraries and headers installed on your
system.

To run the tests run `make test`.


<!-- Badges -->
[travis]: https://travis-ci.com/helium/ebus
[travis badge]: https://img.shields.io/travis/com/helium/ebus/master.svg?style=flat-square
[hex]: https://hex.pm/packages/ebus
[hex version badge]: https://img.shields.io/hexpm/v/ebus.svg?style=flat-square
[hex license badge]: https://img.shields.io/hexpm/l/ebus.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-21.1-blue.svg?style=flat-square
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-orange.svg?style=flat-square

<!-- Links -->
[connman]: https://01.org/connman
[rebar]: http://rebar3.org
[erlang]: http://erlang.org
