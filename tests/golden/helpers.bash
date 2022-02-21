# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

set -o pipefail

setup () {
  # delete all existing entries
  coffer delete -r / || true

  # change working directory to the location of the running `bats` suite.
  cd "$( dirname "$BATS_TEST_FILENAME")"
}

# `cleanOutput <cmd>` will run the given command and then scrub dates out of the output.
#
# Note: Having an auxiliary function is the recommended workaround for when
# you need to pipe the output of a command and use bats' `run` command.
#
# See the `get_projectsh_welcome_message` example here:
# https://bats-core.readthedocs.io/en/stable/tutorial.html#dealing-with-output
cleanOutput () {
  $@ | sed -r 's/\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]$/[2000-01-01 01:01:01]/g'
}