<!--
 - SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

# Coffer CLI Backends Configuration

## Vault

### Backend configuration file example
```toml
type = "vault-kv" # type for 'vault' backend
address = "localhost:8200" # required
mount = "secret" # required
token = "<vault token>" # required
```

### How to run
First of all, you need to install `vault` from its [official site](https://www.vaultproject.io/). Then you can start server with the next command:
```shell
$ vault server -dev
```
Also you can specify token and address where it would be launched (default `localhost:8200`)
```shell
-dev-root-token-id=<vault token>
-dev-listen-address=<addr>
```