<!--
 - SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

# Coffer

![coffer-logo](./logo.png "Coffer logo")

Coffer is multi-backend password store with multiple frontends. It reaches out to [Vault](https://www.vaultproject.io/), [pass](https://www.passwordstore.org/) and others to store your most precious secrets.

## Installation

??? from releases?

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

Coffer also has a Web API. Currently, it runs on `localhost:8081`. In the future we could configure this but at this moment we can't.

You can run server in the following ways:
1. Cabal
```shell
$ cabal run coffer-server
```
2. Stack
```shell
$ stack run coffer-server
```

Documentation for Web API endpoints and their return types could be generated via `servant-openapi3`. (TODO: write something about docs generation after [#114](https://github.com/serokell/coffer/issues/114) is resolved).

#### Configuration
(TODO: write something here after [#95](https://github.com/serokell/coffer/issues/95) is resolved). At this moment we configure Web API via HTTP header (we are passing `VaultToken`, but it's not used anywhere).

Examples:
1. Creating new entry
```shell
$ curl -s -D /dev/stderr \
	-H 'token: root' \
	'localhost:8081/api/v1/content/create?path=/ex1' \
	-X POST \
	-H 'Content-Type: application/json' \
	-d '{ "fields": { "some-field": { "contents": "hi", "visibility": "public" } }, "tags": ["tag"] }' | jq

HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Tue, 31 May 2022 13:00:28 GMT
Server: Warp/3.3.18
Content-Type: application/json;charset=utf-8

{
  "path": "/ex1",
  "dateModified": "2022-05-31T13:00:28.267250327Z",
  "masterField": null,
  "fields": {
    "some-field": {
      "dateModified": "2022-05-31T13:00:28.267250327Z",
      "visibility": "public",
      "contents": "hi"
    }
  },
  "tags": [
    "tag"
  ]
}

```
2. View all entries
```shell
$ curl -s -D /dev/stderr \
	-H 'token: root' \
	'localhost:8081/api/v1/content/view?path=/' \
	-X GET \
	-H 'Content-Type: application/json' | jq

HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Tue, 31 May 2022 13:05:31 GMT
Server: Warp/3.3.18
Content-Type: application/json;charset=utf-8

{
  "entries": [
    {
      "path": "/ex1",
      "dateModified": "2022-05-31T13:04:05.827650358Z",
      "masterField": null,
      "fields": {},
      "tags": []
    },
    {
      "path": "/ex2",
      "dateModified": "2022-05-31T13:04:24.259142027Z",
      "masterField": null,
      "fields": {
        "example": {
          "dateModified": "2022-05-31T13:04:24.259142027Z",
          "visibility": "public",
          "contents": "some contents"
        }
      },
      "tags": []
    },
    {
      "path": "/ex3",
      "dateModified": "2022-05-31T13:04:08.178256852Z",
      "masterField": null,
      "fields": {},
      "tags": []
    }
  ],
  "subdirs": {}
}
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

### `Golden` Tests
Before you run `golden` tests you need to obtain a `coffer` executable. You can get it in two ways:
1. Cabal
```shell
$ cabal install
```
2. Stack
```shell
$ stack install
```
These commands would build the project and add a symblink for a `coffer` executable.

Now you can run `golden` tests with next command:
```shell
$ make bats
```

### Haskell Tests
These test suites are easier to run than `golden` tests. You can run all tests with this command:
```shell
$ make test
```
Also we have a special task in `Makefile` which runs only `server-integration` test suite (this test suite is included in `test` task).
```shell
$ make server-integration
```