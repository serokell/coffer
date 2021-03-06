# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "masks private fields" {
  coffer create /secrets/google --field email=johndoe@gmail.com --privatefield pw=123

  run cleanOutput coffer find /

  assert_success
  assert_output - <<EOF
/
  secrets/
    google - [2000-01-01 01:01:01]
      email: johndoe@gmail.com [2000-01-01 01:01:01]
      pw: [private]            [2000-01-01 01:01:01]
EOF
}

@test "sorting entries" {
  coffer create /secrets/a
  coffer create /secrets/c
  coffer create /secrets/b

  run cleanOutput coffer find / --sort name:asc

  assert_success
  assert_output - <<EOF
/
  secrets/
    a - [2000-01-01 01:01:01]
    b - [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --sort name:desc

  assert_success
  assert_output - <<EOF
/
  secrets/
    c - [2000-01-01 01:01:01]
    b - [2000-01-01 01:01:01]
    a - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --sort date:asc

  assert_success
  assert_output - <<EOF
/
  secrets/
    a - [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
    b - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --sort date:desc

  assert_success
  assert_output - <<EOF
/
  secrets/
    b - [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
    a - [2000-01-01 01:01:01]
EOF

  coffer set-field /secrets/a sortfield c
  coffer set-field /secrets/c sortfield a
  coffer set-field /secrets/b sortfield b

  run cleanOutput coffer find / --sort sortfield:contents:asc

  assert_success
  assert_output - <<EOF
/
  secrets/
    c - [2000-01-01 01:01:01]
      sortfield: a [2000-01-01 01:01:01]
    b - [2000-01-01 01:01:01]
      sortfield: b [2000-01-01 01:01:01]
    a - [2000-01-01 01:01:01]
      sortfield: c [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --sort sortfield:contents:desc

  assert_success
  assert_output - <<EOF
/
  secrets/
    a - [2000-01-01 01:01:01]
      sortfield: c [2000-01-01 01:01:01]
    b - [2000-01-01 01:01:01]
      sortfield: b [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
      sortfield: a [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --sort sortfield:date:asc

  assert_success
  assert_output - <<EOF
/
  secrets/
    a - [2000-01-01 01:01:01]
      sortfield: c [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
      sortfield: a [2000-01-01 01:01:01]
    b - [2000-01-01 01:01:01]
      sortfield: b [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --sort sortfield:date:desc

  assert_success
  assert_output - <<EOF
/
  secrets/
    b - [2000-01-01 01:01:01]
      sortfield: b [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
      sortfield: a [2000-01-01 01:01:01]
    a - [2000-01-01 01:01:01]
      sortfield: c [2000-01-01 01:01:01]
EOF


}

@test "sort on field with name 'date' and 'name'" {

  coffer create /secrets/a
  coffer create /secrets/c
  coffer create /secrets/b

  coffer set-field /secrets/a name name1
  coffer set-field /secrets/c name name2
  coffer set-field /secrets/b name name3

  run cleanOutput coffer find / --sort name:contents:desc

  assert_success
  assert_output - <<EOF
/
  secrets/
    b - [2000-01-01 01:01:01]
      name: name3 [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
      name: name2 [2000-01-01 01:01:01]
    a - [2000-01-01 01:01:01]
      name: name1 [2000-01-01 01:01:01]
EOF

  coffer set-field /secrets/a date date1
  coffer set-field /secrets/c date date2
  coffer set-field /secrets/b date date3

  run cleanOutput coffer find / --sort date:contents:desc

  assert_success
  assert_output - <<EOF
/
  secrets/
    b - [2000-01-01 01:01:01]
      name: name3 [2000-01-01 01:01:01]
      date: date3 [2000-01-01 01:01:01]
    c - [2000-01-01 01:01:01]
      name: name2 [2000-01-01 01:01:01]
      date: date2 [2000-01-01 01:01:01]
    a - [2000-01-01 01:01:01]
      name: name1 [2000-01-01 01:01:01]
      date: date1 [2000-01-01 01:01:01]
EOF
}

@test "filter entries names" {
  coffer create /secrets/abbacaba
  coffer create /secrets/cab
  coffer create /secrets/zzz

  run cleanOutput coffer find / --filter name~bb

  assert_success
  assert_output - <<EOF
/
  secrets/
    abbacaba - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --filter name~ab

  assert_success
  assert_output - <<EOF
/
  secrets/
    abbacaba - [2000-01-01 01:01:01]
    cab - [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --filter name~zz

  assert_success
  assert_output - <<EOF
/
  secrets/
    zzz - [2000-01-01 01:01:01]
EOF
}

@test "'coffer find PATH' excludes entries that are not in PATH" {
  coffer create /a/b/c
  coffer create /a/b/d
  coffer create /a/e

  run cleanOutput coffer find /a/b

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
      d - [2000-01-01 01:01:01]
EOF
}

@test "'coffer find' returns the intersection of all filters" {
  coffer create /ab
  coffer create /cd --field filtering=kek
  coffer create /abcd --field filtering=kek

  run cleanOutput coffer find / --filter name~ab --filter filtering:contents~kek

  assert_success
  assert_output - <<EOF
/
  abcd - [2000-01-01 01:01:01]
    filtering: kek [2000-01-01 01:01:01]
EOF
}

@test "'coffer find PATH TEXT' searches for TEXT in the entry's path segments (including the entry name) and tags" {
  coffer create /a/secret/b
  coffer create /a/d/x
  coffer create /a/d/y --tag secret
  coffer create /a/d/secret

  run cleanOutput coffer find / secret

  assert_success
  assert_output - <<EOF
/
  a/
    secret/
      b - [2000-01-01 01:01:01]
    d/
      secret - [2000-01-01 01:01:01]
      y - [secret] [2000-01-01 01:01:01]
EOF
}

@test "directories where no matches were found do not appear in the result" {
  coffer create /secrets/d/ab
  coffer create /secrets/d/abc
  coffer create /secrets/b/c

  run cleanOutput coffer find / ab

  assert_success
  assert_output - <<EOF
/
  secrets/
    d/
      ab - [2000-01-01 01:01:01]
      abc - [2000-01-01 01:01:01]
EOF
}

@test "PATH can be an entry's path" {
  coffer create /a/b/c
  coffer create /a/b/d

  run cleanOutput coffer find /a/b/c

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
EOF
}

@test "using a path that doesn't exist returns no matches" {
  run coffer find /notexists

  assert_failure
  assert_output "[ERROR] No match found."
}

@test "filter entries fields" {
  coffer create /secrets/a --field filtering=abbacaba
  coffer create /secrets/b --field filtering=cab
  coffer create /secrets/c --field filtering=zzz

  run cleanOutput coffer find / --filter filtering:contents~bb

  assert_success
  assert_output - <<EOF
/
  secrets/
    a - [2000-01-01 01:01:01]
      filtering: abbacaba [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --filter filtering:contents~ab

  assert_success
  assert_output - <<EOF
/
  secrets/
    a - [2000-01-01 01:01:01]
      filtering: abbacaba [2000-01-01 01:01:01]
    b - [2000-01-01 01:01:01]
      filtering: cab [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find / --filter filtering:contents~zz

  assert_success
  assert_output - <<EOF
/
  secrets/
    c - [2000-01-01 01:01:01]
      filtering: zzz [2000-01-01 01:01:01]
EOF
}

@test "find an entry on specified backend" {
  coffer create /a/b/c
  coffer create /a/d/c

  coffer create second#/a/b/c
  coffer create second#/a/d/c

  run cleanOutput coffer find second#/a/b

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
EOF
}

@test "find all entries on specified backend" {
  coffer create /a/b/c
  coffer create /a/d/c

  coffer create second#/a/b/c
  coffer create second#/a/d/c

  run cleanOutput coffer find second#

  assert_success
  assert_output - <<EOF
/
  a/
    b/
      c - [2000-01-01 01:01:01]
    d/
      c - [2000-01-01 01:01:01]
EOF
}

@test "find multiline field" {
  coffer create /path --field user="$(echo -e "first\nsecond")"

  run cleanOutput coffer find

  assert_success
  assert_output - <<EOF
/
  path - [2000-01-01 01:01:01]
    user: [2000-01-01 01:01:01]
      first
      second
EOF
}

@test "'find' resets all ANSI control sequences" {
  coffer create /a/b/c --field x="$(echo -e "\x1b[41;1mHi i'm red")"
  coffer create /a/b/d --field x=y

  run cleanOutput coffer find

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

@test "filter field with name 'date' and 'name'" {
  coffer create /secrets/d/ab --field date=2000 --field name=a
  coffer create /secrets/d/ad --field date=2020 --field name=b
  coffer create /secrets/d/abc --field date=9999

  run cleanOutput coffer find / ab --filter date:contents~20 --filter "date>1000" --filter name~a

  assert_success
  assert_output - <<EOF
/
  secrets/
    d/
      ab - [2000-01-01 01:01:01]
        name: a    [2000-01-01 01:01:01]
        date: 2000 [2000-01-01 01:01:01]
EOF

  run cleanOutput coffer find --filter name:contents~a --filter name~a
  assert_success
  assert_output - <<EOF
/
  secrets/
    d/
      ab - [2000-01-01 01:01:01]
        name: a    [2000-01-01 01:01:01]
        date: 2000 [2000-01-01 01:01:01]
EOF
}
