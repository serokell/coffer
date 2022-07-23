# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

#!/usr/bin/env bats

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "bad path" {
  run coffer view "$(echo -e "bad#path#")"

  assert_failure
  assert_output --partial - <<EOF
Parser error:
Too many # literals.
Expected format is: [<backend-name>#]<path>.
EOF
}

@test "bad entry tag" {
  run coffer tag /path "$(echo -e "bad\ntag")"

  assert_failure
  assert_output --partial - <<EOF
Invalid tag: "bad\ntag".
Tags can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_;'
EOF
}

@test "bad backend name" {
  run coffer create "$(echo -e "bad\nbackend#/path")"

  assert_failure
  assert_output --partial - <<EOF
Backend name can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_;'
EOF
}

@test "bad field name" {
  run coffer view /path "$(echo -e "bad\nfieldname")"

  assert_failure
  assert_output --partial - <<EOF
Invalid field name: "bad\nfieldname".
Field name can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_;'
EOF
}

@test "bad qualified entry path" {
  run coffer create "$(echo -e "back#/path#\n\nsmth")"

  assert_failure
  assert_output --partial - <<EOF
Invalid qualified entry path format: "back#/path#\n\nsmth".
Expected format is: [<backend-name>#]<entry-path>.
<backend-name> can be a string of the following characters: [a-zA-Z0-9] and symbols '-', '_', ';'.
Examples: 'vault_kv-backend#secrets/google', 'my/passwords/entry'.

Parser error:
Too many # literals.
Expected format is: [<backend-name>#]<path>.
EOF
}

@test "bad qualified path" {
  run coffer view "$(echo -e "back#/path#\n\nsmth")"

  assert_failure
  assert_output --partial - <<EOF
Invalid qualified path format: "back#/path#\n\nsmth".
Expected format is: [<backend-name>#]<path>.
<backend-name> can be a string of the following characters: [a-zA-Z0-9] and symbols '-', '_', ';'.
Examples: 'vault_kv-backend#secrets/google', 'my/passwords/mypage/'.

Parser error:
Too many # literals.
Expected format is: [<backend-name>#]<path>.
EOF
}

@test "bad field info" {
  run coffer create /path --field "$(echo -e "bad\n\n=test")"

  assert_failure
  assert_output --partial - <<EOF
option --field: Invalid field format: "bad\n\n=test".
Expected format: 'fieldname=fieldcontents'.

Parser error:
1:4:
  |
1 | bad
  |    ^
unexpected newline
expecting '=', fieldname, or white space
EOF
}

@test "bad filter" {
  run coffer find --filter "$(echo -e "bad\nfilter~str")"

  assert_failure
  assert_output --partial - <<EOF
option --filter: Invalid filter format: "bad\nfilter~str".
EOF
}

@test "bad filter field" {
  run coffer find --filter "$(echo -e "name:bad\n\n~str")"

  assert_failure
  assert_output --partial - <<EOF
option --filter: Invalid filter format: "name:bad\n\n~str".
EOF
}
