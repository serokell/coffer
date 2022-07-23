# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "vault bad path" {
  run coffer view "$(echo -e "bad\npath")"

  assert_failure
  assert_output --partial - <<EOF
Internal backend error:
Backend returned a path segment that is not a valid entry or directory name.
Path segments for Vault KV can only contain the following characters:
  'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_'
Got: "bad\npath".
EOF
}

@test "vault bad entry path" {
  run coffer create "$(echo -e "bad/entry\npath")"

  assert_failure
  assert_output --partial - <<EOF
Internal backend error:
Backend returned a path segment that is not a valid entry or directory name.
Path segments for Vault KV can only contain the following characters:
  'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_'
Got: "entry\npath".
EOF
}
