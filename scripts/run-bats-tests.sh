#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

# This script runs two vault instances,
# then runs bats tests and finally kills the vault instances.

BATSFILTER=$1

vault server -dev -dev-root-token-id="root" -dev-listen-address="localhost:8209" > /dev/null 2>&1 &
vault1_pid=$!

vault server -dev -dev-root-token-id="second" -dev-listen-address="localhost:8211" > /dev/null 2>&1 &
vault2_pid=$!

if [ -z "$BATSFILTER" ]; then
  ./tests/golden/helpers/bats/bin/bats ./tests/golden/**
else
  ./tests/golden/helpers/bats/bin/bats ./tests/golden/** -f "$BATSFILTER"
fi

kill $vault1_pid > /dev/null 2>&1
kill $vault2_pid > /dev/null 2>&1
