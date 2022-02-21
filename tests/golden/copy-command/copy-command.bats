# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "copy an entry" {
  coffer create /a

  run coffer copy /a /b

  assert_success
  assert_output "[SUCCESS] Copied '/a' to '/b'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a - [2000-01-01 01:01:01]
  b - [2000-01-01 01:01:01]
EOF
}

@test "copy a directory" {
  coffer create /a/b/c
  coffer create /a/x
  coffer create /diff/entry

  run coffer copy /a /diff

  assert_success
  assert_output - <<EOF
[SUCCESS] Copied '/a/x' to '/diff/x'.
[SUCCESS] Copied '/a/b/c' to '/diff/b/c'.
EOF

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    x - [2000-01-01 01:01:01]
    b/
      c - [2000-01-01 01:01:01]
  diff/
    entry - [2000-01-01 01:01:01]
    x - [2000-01-01 01:01:01]
    b/
      c - [2000-01-01 01:01:01]
EOF
}

@test "copy one directory to other" {
  coffer create /a/b/c
  coffer create /c/d/e

  run coffer copy /a /c

  assert_success
  assert_output "[SUCCESS] Copied '/a/b/c' to '/c/b/c'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
  c/
    b/
      c - [2000-01-01 01:01:01]
    d/
      e - [2000-01-01 01:01:01]
EOF
}

@test "copying an entry fails when it clashes with an existing directory" {
  coffer create /a
  coffer create /b/c/d

  run coffer copy /a /b/c

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied because a directory already exists at the destination.

Cannot copy '/a' to '/b/c'.
EOF

  run coffer copy /a /b/c -f

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied because a directory already exists at the destination.

Cannot copy '/a' to '/b/c'.
EOF
}

@test "copying a directory fails when any entry clashes with an existing entry" {
  coffer create /a/b/c
  coffer create /a/b/x
  coffer create /a/b/y

  coffer create /d/b/c
  coffer create /d/b/x

  run coffer copy /a /d

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied because an entry already exists at the destination.
Use '--force' or '-f' to overwrite existing entries.

Cannot copy '/a/b/c' to '/d/b/c'.
Cannot copy '/a/b/x' to '/d/b/x'.
EOF
}

@test "copying a directory succeeds when any entry clashes with an existing entry and --force is used" {
  coffer create /a/b/c
  coffer create /a/b/x
  coffer create /a/b/y

  coffer create /d/b/c
  coffer create /d/b/x

  run coffer copy /a /d -f

  assert_success
  assert_output - <<EOF
[SUCCESS] Copied '/a/b/c' to '/d/b/c'.
[SUCCESS] Copied '/a/b/x' to '/d/b/x'.
[SUCCESS] Copied '/a/b/y' to '/d/b/y'.
EOF

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
      x - [2000-01-01 01:01:01]
      y - [2000-01-01 01:01:01]
  d/
    b/
      c - [2000-01-01 01:01:01]
      x - [2000-01-01 01:01:01]
      y - [2000-01-01 01:01:01]
EOF
}

@test "copying a directory fails when any entry clashes with an existing directory" {
  coffer create /a/b
  coffer create /x/b/c

  run coffer copy /a /x

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied because a directory already exists at the destination.

Cannot copy '/a/b' to '/x/b'.
EOF

  run coffer copy /a /x -f

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied because a directory already exists at the destination.

Cannot copy '/a/b' to '/x/b'.
EOF
}