# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

.PHONY: coffer test haddock haddock-no-deps stylish lint clean bats all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=coffer

coffer:
	$(MAKE_PACKAGE) dev

# Runs all tests.
# Usage:
#  * make test
#  * make test BATSFILTER='test name' TEST_ARGUMENTS='--pattern "test name"' DOCTEST_ARGUMENTS='--verbose'
test:
	make doctest
	make test-unit
	make server-integration
	make bats
test-dumb-term:
	$(MAKE_PACKAGE) test-dumb-term
test-hide-successes:
	$(MAKE_PACKAGE) test-hide-successes
haddock:
	$(MAKE_PACKAGE) haddock
haddock-no-deps:
	$(MAKE_PACKAGE) haddock-no-deps
clean:
	$(MAKE_PACKAGE) clean

stylish:
	find . -name '.stack-work' -prune -o -name '.dist-newstyle' -prune -o -name '*.hs' -exec stylish-haskell -i '{}' \;

lint:
	hlint .

####################################
# Individual test suites

doctest:
	stack test --fast coffer:test:doctests --test-arguments "$(DOCTEST_ARGUMENTS)"

test-unit:
	$(MAKEU) test PACKAGE="coffer:test:test"

server-integration:
	$(MAKEU) test PACKAGE="coffer:test:server-integration"

# Usage:
#   * make bats
#   * make bats BATSFILTER="test name"
bats:
	stack install --fast coffer:exe:coffer
	git submodule update --init --recursive
	./scripts/run-bats-tests.sh $(if $(BATSFILTER),"$(BATSFILTER)",)
