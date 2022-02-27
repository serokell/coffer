# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "delete an entry" {
  coffer create /a
  coffer create /b

  run coffer delete /a

  assert_success
  assert_output "[SUCCESS] Deleted '/a'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  b - [2000-01-01 01:01:01]
EOF
}

@test "delete not existing entry" {
  run coffer delete /a

  assert_failure
  assert_output "[ERROR] Entry or directory not found at '/a'."
}

@test "delete directory without recursive" {
  coffer create /a/b/c
  coffer create /a/d

  run coffer delete /a/b

  assert_failure
  assert_output - <<EOF
[ERROR] The path '/a/b' is a directory.
Use '--recursive' or '-r' to recursively delete all entries.

EOF
}

@test "delete directory with recursive" {
  coffer create /a/b/c
  coffer create /a/d

  run coffer delete /a/b -r

  assert_success
  assert_output "[SUCCESS] Deleted '/a/b/c'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    d - [2000-01-01 01:01:01]
EOF
}
