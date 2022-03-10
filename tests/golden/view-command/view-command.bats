# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "view at specified directory" {
  coffer create a/b/c --field test=kek
  coffer create a/b/d
  coffer create kek/a

  run cleanOutput coffer view /

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
        test: kek [2000-01-01 01:01:01]
      d - [2000-01-01 01:01:01]
  kek/
    a - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer view /a/b

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
        test: kek [2000-01-01 01:01:01]
      d - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer view /kek

  assert_success
  assert_output - <<EOF
/
  kek/
    a - [2000-01-01 01:01:01]
EOF
}

@test "view specified field" {
  coffer create a/b/c --field test=kek --field dontShow=yes
  coffer create a/b/d

  run cleanOutput coffer view / test

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
        test: kek [2000-01-01 01:01:01]
EOF
}

@test "view specified entry" {
  coffer create /a/b/c
  coffer create /a/b/d

  run cleanOutput coffer view /a/b/c

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
EOF
}

@test "view specified entry with a field filter" {
  coffer create /a/b/c --field test=kek --field dontShow=yes
  coffer create /a/b/d

  run cleanOutput coffer view /a/b/c test

  assert_success
  assert_output "kek"
}

@test "view command by default" {
  coffer create /a/b/c --field test=kek
  coffer create /a/d

  run cleanOutput coffer /a/b

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
        test: kek [2000-01-01 01:01:01]
EOF
}
