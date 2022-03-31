# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "public and private field visibility" {
  coffer create /a/b

  run coffer set-field /a/b test aba -V public

  assert_success
  assert_output - <<EOF
[SUCCESS] Set field 'test' (public) at '/a/b' to:
aba
EOF

  run cleanOutput coffer find
  assert_output - <<EOF
/
  a/
    b - [2000-01-01 01:01:01]
      test: aba [2000-01-01 01:01:01]
EOF

  run coffer set-field /a/b test -V private

  assert_success
  assert_output - <<EOF
[SUCCESS] Set field 'test' (private) at '/a/b' to:
aba
EOF

  run cleanOutput coffer find
  assert_output - <<EOF
/
  a/
    b - [2000-01-01 01:01:01]
      test: [private] [2000-01-01 01:01:01]
EOF
}

@test "directory set field fail" {
  coffer create /a/b

  run coffer set-field /a directory-field kek

  assert_failure
  assert_output "[ERROR] Entry not found at '/a'."
}

@test "change visibility on non existing field" {
  coffer create /a/b

  run coffer set-field /a/b not-exist -V public

  assert_failure
  assert_output - <<EOF
[ERROR] The entry at '/a/b' does not yet have a field 'not-exist'.
In order to create a new field, please include the 'FIELDCONTENTS' argument.
EOF
}

@test "updating an existing field" {
  coffer create a

  run coffer set-field a kek a1

  assert_success
  assert_output - <<EOF
[SUCCESS] Set field 'kek' (public) at '/a' to:
a1
EOF

  run coffer set-field a kek a2

  assert_success
  assert_output - <<EOF
[SUCCESS] Set field 'kek' (public) at '/a' to:
a2
EOF

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a - [2000-01-01 01:01:01]
    kek: a2 [2000-01-01 01:01:01]
EOF
}

@test "set-field fails when the path doesn't exist" {
  run coffer set-field notexist kek a1

  assert_failure
  assert_output "[ERROR] Entry not found at '/notexist'."
}

@test "set field on specified backend" {
  coffer create /a/b
  coffer create second#/a/b

  run coffer set-field second#/a/b test test

  assert_success
  assert_output - <<EOF
[SUCCESS] Set field 'test' (public) at '/a/b' to:
test
EOF

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
    b - [2000-01-01 01:01:01]
      test: test [2000-01-01 01:01:01]
EOF
}

@test "multiline field" {
  coffer create /path

  run coffer set-field /path multiline "$(echo -e "first\nsecond")"

  assert_success
  assert_output - <<EOF
[SUCCESS] Set field 'multiline' (public) at '/path' to:
first
second
EOF
}

@test "set field with ANSI control sequences" {
  coffer create /a/b/c

  run coffer set-field /a/b/c red "$(echo -e "\x1b[41;1mHi I'm red")"

  assert_success
  assert_output - <<EOF
[SUCCESS] Set field 'red' (public) at '/a/b/c' to:
$(printf '\x1b[41;1m')Hi I'm red$reset
EOF
}
