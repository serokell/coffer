# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "delete field" {
  coffer create /a/b --field test=test

  run coffer delete-field /a/b test

  assert_success
  assert_output "[SUCCESS] Deleted field 'test' from '/a/b'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b - [2000-01-01 01:01:01]
EOF
}

@test "delete non existing field" {
  coffer create /a/b

  run coffer delete-field /a/b not-exist

  assert_failure
  assert_output "[ERROR] Entry does not have a field with name 'not-exist'."
}

@test "delete-field fails when the path doesn't exist" {
  run coffer delete-field /notexist idk

  assert_failure
  assert_output "[ERROR] Entry not found at '/notexist'."
}

@test "delete-field fails when the path is a directory" {
  coffer create /a/b/c

  run coffer delete-field /a/b dir

  assert_failure
  assert_output "[ERROR] Entry not found at '/a/b'."
}
