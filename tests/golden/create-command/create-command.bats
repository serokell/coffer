# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "create entry at root" {
  run coffer create / --field email=johndoe@gmail.com --field pw=123

  assert_failure
  assert_output --partial - <<EOF
Invalid entry path: '/'.
Entry paths must not be empty.
EOF
}

@test "create entry with fields" {
  run coffer create /secrets/google --field email=johndoe@gmail.com --field pw=123
  assert_success
  assert_output "[SUCCESS] Entry created at '/secrets/google'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  secrets/
    google - [2000-01-01 01:01:01]
      email: johndoe@gmail.com [2000-01-01 01:01:01]
      pw: 123                  [2000-01-01 01:01:01]
EOF
}

@test "create entry with private fields" {
  run coffer create /secrets/google --field email=johndoe@gmail.com --privatefield pw=123
  assert_success
  assert_output "[SUCCESS] Entry created at '/secrets/google'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  secrets/
    google - [2000-01-01 01:01:01]
      email: johndoe@gmail.com [2000-01-01 01:01:01]
      pw: 123                  [2000-01-01 01:01:01]
EOF
}

@test "create entry with tags" {
  run coffer create /secrets/google --field email=johndoe@gmail.com --tag mytag1 --tag mytag2

  assert_success
  assert_output "[SUCCESS] Entry created at '/secrets/google'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  secrets/
    google - [mytag1, mytag2] [2000-01-01 01:01:01]
      email: johndoe@gmail.com [2000-01-01 01:01:01]
EOF
}

@test "fieldname must not be empty" {
  run coffer create /secrets/google --field =johndoe@gmail.com

  assert_failure
  assert_output --partial - <<EOF
option --field: Invalid field format: '=johndoe@gmail.com'.
Expected format: 'fieldname=fieldcontents'.

Parser error:
1:1:
  |
1 | =johndoe@gmail.com
  | ^
unexpected '='
expecting fieldname
EOF
}

@test "create fails if an entry already exists at that path" {
  run coffer create /a/b/c

  assert_success
  assert_output "[SUCCESS] Entry created at '/a/b/c'."

  run coffer create /a/b/c

  assert_failure
  assert_output - <<EOF
[ERROR] The entry cannot be created:

An entry already exists at '/a/b/c'.
Use '--force' or '-f' to overwrite existing entries.
EOF
}

@test "create --force succeeds if an entry already exists at that path" {
  run coffer create /a/b/c --field x=x

  assert_success
  assert_output "[SUCCESS] Entry created at '/a/b/c'."

  run coffer create /a/b/c -f --field y=y

  assert_success
  assert_output "[SUCCESS] Entry created at '/a/b/c'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
        y: y [2000-01-01 01:01:01]
EOF
}

@test "specifying the same tag multiple times is the same as specifying it once" {
  run coffer create a --tag same --tag notSame --tag same

  assert_success
  assert_output "[SUCCESS] Entry created at '/a'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a - [notSame, same] [2000-01-01 01:01:01]
EOF
}

@test "field contents can be empty" {
  run coffer create a --field test=

  assert_success
  assert_output "[SUCCESS] Entry created at '/a'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a - [2000-01-01 01:01:01]
    test:  [2000-01-01 01:01:01]
EOF
}

@test "field contents can have multiple lines" {
  run coffer create /path --field "user=$(echo -e "first\nsecond")"

  assert_success

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  path - [2000-01-01 01:01:01]
    user: [2000-01-01 01:01:01]
      first
      second
EOF
}

@test "creating an entry clashes with existing directory" {
  coffer create /a/b/c

  run coffer create /a/b

  assert_failure
  assert_output - <<EOF
[ERROR] The entry cannot be created:

'/a/b' is a directory.
EOF
}

@test "creating a directory clashes with existing entry" {
  coffer create /a/b

  run coffer create /a/b/c

  assert_failure
  assert_output - <<EOF
[ERROR] The entry cannot be created:

Attempted to create the directory '/a/b' but an entry exists at that path.
EOF
}

@test "create entry on specified backend" {
  coffer create /first

  run coffer create second#/second

  assert_success
  assert_output "[SUCCESS] Entry created at 'second#/second'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  first - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer view second#/
  assert_output - <<EOF
/
  second - [2000-01-01 01:01:01]
EOF
}
