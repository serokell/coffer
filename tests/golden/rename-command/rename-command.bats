# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "rename an entry" {
  coffer create /a

  run coffer rename /a /b

  assert_success
  assert_output "[SUCCESS] Renamed '/a' to '/b'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  b - [2000-01-01 01:01:01]
EOF
}

@test "rename a directory" {
  coffer create /a/b/c

  run coffer rename /a/b /d

  assert_success
  assert_output "[SUCCESS] Renamed '/a/b/c' to '/d/c'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  d/
    c - [2000-01-01 01:01:01]
EOF
}

@test "trying to rename without force" {
  coffer create /a/b/c
  coffer create /a/b/d
  coffer create /a/b/e

  coffer create /d/c
  coffer create /d/d

  run coffer rename /a/b /d

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be renamed because an entry already exists at the destination.
Use '--force' or '-f' to overwrite existing entries.

Cannot rename '/a/b/c' to '/d/c'.
Cannot rename '/a/b/d' to '/d/d'.
EOF
}

@test "trying to rename with force" {
  coffer create /a/b/c --field x=a
  coffer create /a/b/d --field x=b
  coffer create /a/b/e

  coffer create /d/c --field y=a
  coffer create /d/d --field y=b

  run coffer rename /a/b /d -f

  assert_success
  assert_output - <<EOF
[SUCCESS] Renamed '/a/b/c' to '/d/c'.
[SUCCESS] Renamed '/a/b/d' to '/d/d'.
[SUCCESS] Renamed '/a/b/e' to '/d/e'.
EOF

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  d/
    c - [2000-01-01 01:01:01]
      x: a [2000-01-01 01:01:01]
    d - [2000-01-01 01:01:01]
      x: b [2000-01-01 01:01:01]
    e - [2000-01-01 01:01:01]
EOF
}

@test "entry-directory renaming conflict" {
  coffer create /a/b/c
  coffer create /d

  run coffer rename d a/b

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be renamed because a directory already exists at the destination.

Cannot rename '/d' to '/a/b'.
EOF
}

@test "moving directory deeper without clashes" {
  coffer create /dir1/entry1

  run coffer rename /dir1 /dir1/dir2

  assert_success
  assert_output "[SUCCESS] Renamed '/dir1/entry1' to '/dir1/dir2/entry1'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  dir1/
    dir2/
      entry1 - [2000-01-01 01:01:01]
EOF
}

@test "moving directory deeper with clashes" {
  coffer create /dir1/entry --field a=b
  coffer create /dir1/dir2/entry --field c=d

  run coffer rename /dir1 /dir1/dir2 -f

  assert_success
  assert_output - <<EOF
[SUCCESS] Renamed '/dir1/entry' to '/dir1/dir2/entry'.
[SUCCESS] Renamed '/dir1/dir2/entry' to '/dir1/dir2/dir2/entry'.
EOF

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  dir1/
    dir2/
      entry - [2000-01-01 01:01:01]
        a: b [2000-01-01 01:01:01]
      dir2/
        entry - [2000-01-01 01:01:01]
          c: d [2000-01-01 01:01:01]
EOF
}

@test "renaming to the same name" {
  coffer create /dir/a --field a=a
  coffer create /dir/b --field b=b

  run coffer rename /dir /dir

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be renamed because an entry already exists at the destination.
Use '--force' or '-f' to overwrite existing entries.

Cannot rename '/dir/a' to '/dir/a'.
Cannot rename '/dir/b' to '/dir/b'.
EOF

  run coffer rename /dir /dir -f

  assert_success
  assert_output - <<EOF
[SUCCESS] Renamed '/dir/a' to '/dir/a'.
[SUCCESS] Renamed '/dir/b' to '/dir/b'.
EOF

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  dir/
    a - [2000-01-01 01:01:01]
      a: a [2000-01-01 01:01:01]
    b - [2000-01-01 01:01:01]
      b: b [2000-01-01 01:01:01]
EOF
}
