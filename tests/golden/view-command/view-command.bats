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

@test "view on specified backend" {
  coffer create /a/b
  coffer create second#/c/d

  run cleanOutput coffer view second#/

  assert_success
  assert_output - <<EOF
/
  c/
    d - [2000-01-01 01:01:01]
EOF
}

@test "view multiline fieldcontent" {
  coffer create /path --field user="$(echo -e "first\n\nsecond")"

  run coffer view /path user

  assert_success
  assert_output - <<EOF
first

second
EOF
}

@test "view entry with ANSI control sequences" {
  coffer create /a/b/c --field x="$(echo -e "\x1b[41;1mHi i'm red")"
  coffer create /a/b/d --field x=y

  run cleanOutput coffer view /

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
        x: $(printf '\x1b[41;1m')Hi i'm red$reset [2000-01-01 01:01:01]
      d - [2000-01-01 01:01:01]
        x: y [2000-01-01 01:01:01]
EOF
}

@test "view field with ANSI control sequences" {
  coffer create /a/b/c --field x="$(echo -e "\x1b[41;1mHi i'm red")"

  run coffer view /a/b/c x

  assert_success
  assert_output "$(printf '\x1b[41;1m')Hi i'm red$reset"
}
