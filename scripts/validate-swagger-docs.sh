#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

# This script verifies that `docs/swagger.json` is up to date.
#
# Usage:
#   ./scripts/validate-swagger-docs.sh <path to coffer-swagger-api executable>
#   ./scripts/validate-swagger-docs.sh make swagger

set -euo pipefail

# Run `coffer-swagger-api`
"$@"

# Note: we temporarily disable `-e`;
# otherwise the script would exit when `git diff` returns 1.
set +e
git diff --exit-code --name-only -- docs/swagger.json
exitCode=$?
set -e

if [ "$exitCode" != 0 ]; then
    echo "The file 'docs/swagger.json' is not up-to-date."
    echo "Run 'make swagger' on the repository to fix this."
    exit 1
fi
