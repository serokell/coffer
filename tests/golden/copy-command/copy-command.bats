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
[ERROR] The following entries cannot be copied:

'/a' to '/b/c':
  '/b/c' is a directory.
EOF

  run coffer copy /a /b/c -f

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied:

'/a' to '/b/c':
  '/b/c' is a directory.
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
[ERROR] The following entries cannot be copied:

'/a/b/c' to '/d/b/c':
  An entry already exists at '/d/b/c'.
  Use '--force' or '-f' to overwrite existing entries.
'/a/b/x' to '/d/b/x':
  An entry already exists at '/d/b/x'.
  Use '--force' or '-f' to overwrite existing entries.
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

  EXPECTED=$(cat <<EOF
[ERROR] The following entries cannot be copied:

'/a/b' to '/x/b':
  '/x/b' is a directory.
EOF
)

  assert_failure
  assert_output "$EXPECTED"

  run coffer copy /a /x -f

  assert_failure
  assert_output "$EXPECTED"
}

@test "copy dry-run" {
  coffer create /a/b/c
  coffer create /a/b/d

  run coffer copy /a /x -d

  assert_success
  assert_output - <<EOF
These actions would be done:
[SUCCESS] Copied '/a/b/c' to '/x/b/c'.
[SUCCESS] Copied '/a/b/d' to '/x/b/d'.
EOF

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
      d - [2000-01-01 01:01:01]
EOF
}

@test "directory-entry copy conflict" {
  coffer create /a/b
  coffer create /c/d/e

  run coffer copy /c/d/e /a/b/c

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied:

'/c/d/e' to '/a/b/c':
  Attempted to create the directory '/a/b' but an entry exists at that path.
EOF
}

@test "entry-directory copy conflict" {
  coffer create /test/dir/entry1
  coffer create /test/dir/entry2
  coffer create /test2/dir

  run coffer copy test test2

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied:

'/test/dir/entry1' to '/test2/dir/entry1':
  Attempted to create the directory '/test2/dir' but an entry exists at that path.
'/test/dir/entry2' to '/test2/dir/entry2':
  Attempted to create the directory '/test2/dir' but an entry exists at that path.
EOF
}

@test "copy multierrors" {
  coffer create /a/b
  coffer create /x/b/c
  coffer create /a/d
  coffer create /x/d

  run coffer copy /a /x

  assert_failure
  assert_output - <<EOF
[ERROR] The following entries cannot be copied:

'/a/b' to '/x/b':
  '/x/b' is a directory.
'/a/d' to '/x/d':
  An entry already exists at '/x/d'.
  Use '--force' or '-f' to overwrite existing entries.
EOF
}

@test "copy from one backend to the other" {
  coffer create /a/b

  run coffer copy /a second#/c

  assert_success
  assert_output "[SUCCESS] Copied '/a/b' to '/c/b'."

  run cleanOutput coffer view /
  assert_output - <<EOF
/
  a/
    b - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer view second#/
  assert_output - <<EOF
/
  c/
    b - [2000-01-01 01:01:01]
EOF
}
