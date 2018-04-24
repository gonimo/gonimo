[![Gitter Chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/gonimo/Lobby)

# Gonimo - GOod NIght MOnitor

Welcome to the source code of [Gonimo](https://gonimo.com), the web based baby
monitor. You can give it a try [here](https://app.gonimo.com)!

Please join us on [Gitter](https://gitter.im/gonimo/Lobby), if you have any questions.

# Docs

We will create more documentation as things mature. For frontend development
have a look at the [Gonimo Architecture](./front/doc/Gonimo-Architecture.md),
which describes how frontend code is supposed to be built (WIP).

# Build Instructions

## Clone needed repos

First, we are going to need a fork of the [reflex-platform](https://github.com/eskimor/reflex-platform/tree/gonimo). Clone it into a directory of your choice:

```bash
$ git clone -b gonimo git@github.com:eskimor/reflex-platform.git
```

Make sure you have the `gonimo` branch, as specified in the above command.

Next we need to clone the Gonimo source code:

```bash
$ git clone -b develop git@github.com:gonimo/gonimo.git
```

Make sure you have the `develop` branch, as specified in the above command.
Now we are ready to rumble.

## Setting up nix and the nix binary cache

Follow the [setup instructions][reflex setup] of the reflex-platform, in order to have [nix][nix] installed.

For avoiding _long_ build times, I'd strongly recommend to add the following
lines to your `/etc/nix/nix.conf` (create the file if it does not exist):

```
binary-caches = https://cache.nixos.org/ https://hydra.nixos.org/ https://nixcache.reflex-frp.org
trusted-binary-caches = https://cache.nixos.org/ https://hydra.nixos.org/ https://nixcache.reflex-frp.org
binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
```

## Building Gonimo

Now change to the checked out Gonimo repository.


### Backend Build

```bash
$ ./cabal new-build gonimo-back

```

Please note the `./` before cabal, we are using a wrapper script around cabal which sets up a nix-shell.

Save the path of the resulting executable, which is printed at the end. We'll
need it for executing it later on. (cabal new-run was broken, at the time of
writing).

### Warp Build of the Frontend

If you want quick builds, use this.
You can build the warp based development frontend by typing:

```bash
$ ./cabal new-build gonimo-front-warp
```

Same as for the backend, save the path that is printed at the end.

### GHCJS Build of the Frontend

The ghcjs version is built with:

```bash
$ ./cabal-ghcjs new-build gonimo-front-ghcjs

```

## Starting Gonimo

Once you've built the backend and either the warp frontend or the ghcjs one, we are ready for running Gonimo.

First start the backend, by executing the previously built executable. It has to be executed from the `gonimo` directory. Then either start the warp frontend - in this case, you can connect to your running `Gonimo` by visiting:

```
http://localhost:3709/index.html
```

Or connect directly to the backend for accessing the ghcjs build:

```
http://localhost:8081/index.html
```

Have fun!

[reflex setup]: https://github.com/eskimor/reflex-platform/blob/gonimo/README.md#setup
[nix]: https://nixos.org/nix/
