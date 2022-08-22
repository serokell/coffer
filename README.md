<!--
 - SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

# Coffer

![coffer-logo](./logo.png "Coffer logo")

Coffer is multi-backend password store with multiple frontends. It reaches out to [Vault](https://www.vaultproject.io/), [pass](https://www.passwordstore.org/) and others to store your most precious secrets.

## Usage

### CLI

The CLI is built from a multitude of commands, a quick list can be found below:

```
Available commands:
  view                     View entries under the specified path, optionally
                           returning only the specified field for each entry
  create                   Create a new entry at the specified path
  set-field                Set a field on the entry at the specified path
  delete-field             Delete a field from the entry at the specified path
  find                     Find and list entries, optionally filtering
  rename                   Rename an entry or directory
  copy                     Copy an entry or directory
  delete                   Delete an entry or directory
  tag                      Add or remove tags from an entry
```

#### Configuration

First you must configure coffer and tell it how to reach your preferred backend(s). The way you do this depends on which frontend you want to use, please refer to *frontends* for a list. As an example, here is a `config.toml` taken in by the CLI frontend:

```toml
main_backend = "vault-local"

[[backend]]
type = "vault-kv"
name = "vault-local"
address = "localhost:8200"
mount = "secret"
token = "<vault token>"
```

The configuration consists of a single `main_backend` key and N `[[backend]]` sections. Each one must at least have the `type` key and the `name` key. The rest depends on each backend, to see a comprehensive list, refer to [backends](./docs/cli-backends-configuration.md).

The coffer CLI will look for its configuration file in the following order:
1. A file passed to the tool using the `-c/--config` argument
2. A file passed to the tool using the `COFFER_CONFIG` environment variable
3. `config.toml` file in the current working directory.

### Web API

Coffer also has a Web API.
Its documentation can be found [here](https://petstore.swagger.io/?url=https://raw.githubusercontent.com/serokell/coffer/main/docs/swagger.json).

The port can be specified via the command line option `--port` or the environment variable `COFFER_SERVER_PORT`.

```shell
$ stack run coffer-server -- --port=8081
```

## Development

### Nix

This is the preffered way, install Nix according to the [Nix download page](https://nixos.org/download.html "nix download"). Then execute:

```shell
$ nix develop --extra-experimental-features 'nix-command flakes'
```

You're now ready to develop coffer, using the exact same version (down to a single bit) of GHC, Cabal and all the other required tooling. To actually perform a build run:

```shell
$ cabal build
```

To run coffer while developing run:

```shell
$ cabal run coffer -- <&args>
```

A language server is also available, to use it, either start your IDE from a `nix shell`, or setup `direnv` for your IDE. To install `direnv` itself, take a look [here](https://direnv.net/ "direnv site").

### System Packages

If you don't want to install Nix or can't, then you can try to install the required packages from your distro's package manager. This is not recommended, supported or tested. A complete, but maybe incorrect list, which may need translation to your package manager's naming convention, can be found below:

```
ghc
cabal
haskell-language-server (optional)
stack
stylish-haskell
zlib
```

## Testing

The package contains 4 test suites:
1. A unit tests suite for the coffer library, `coffer:test:test`.
1. An integration test suite for the Web API, `coffer:test:server-integration`.
1. A set of golden tests written in `bats` for the coffer executable.
1. A Haskell doctest suite, `coffer:test:doctests`.

Use `make test` to run all test suites:

```sh
$ make test

$ make test \
    TEST_ARGUMENTS='--pattern "test name"' \
    BATSFILTER='test name' \
    DOCTEST_ARGUMENTS='--verbose'
```

You can also run an individual test suite:

```sh
$ make test-unit TEST_ARGUMENTS='--pattern "test name"'

$ make server-integration TEST_ARGUMENTS='--pattern "test name"'

$ make bats BATSFILTER='test name'

$ make doctest DOCTEST_ARGUMENTS='--verbose'
```
