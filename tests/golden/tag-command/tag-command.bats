# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "add and delete tag" {
  coffer create /a/b
  coffer create /a/c

  run coffer tag /a/c tag

  assert_success
  assert_output "[SUCCESS] Added tag 'tag' to '/a/c'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b - [2000-01-01 01:01:01]
    c - [tag] [2000-01-01 01:01:01]
EOF

  run coffer tag /a/c tag -d

  assert_success
  assert_output "[SUCCESS] Removed tag 'tag' from '/a/c'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b - [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
EOF
}

@test "double tag" {
  coffer create /a

  run coffer tag /a tag

  assert_success
  assert_output "[SUCCESS] Added tag 'tag' to '/a'."

  run coffer tag /a tag

  assert_failure
  assert_output "[ERROR] Entry already has the tag 'tag'."
}

@test "deleting not existing tag" {
  coffer create /a

  run coffer tag /a tag -d

  assert_failure
  assert_output "[ERROR] Entry does not have the tag 'tag'."
}

@test "trying to tag a directory" {
  coffer create /dir/a

  run coffer tag /dir tag

  assert_failure
  assert_output "[ERROR] Entry not found at '/dir'."
}

@test "tag fails when the path doesn't exist" {
  run coffer tag notexist kek

  assert_failure
  assert_output "[ERROR] Entry not found at '/notexist'."
}

@test "tag on specified backend" {
  coffer create /a/b
  coffer create second#/a/b

  run coffer tag second#/a/b tag

  assert_success
  assert_output "[SUCCESS] Added tag 'tag' to 'second#/a/b'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer view second#/
  assert_output - <<EOF
/
  a/
    b - [tag] [2000-01-01 01:01:01]
EOF
}
