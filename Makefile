# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

.PHONY: coffer test haddock haddock-no-deps stylish lint clean all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=coffer

coffer:
	$(MAKE_PACKAGE) dev
test:
	$(MAKE_PACKAGE) test
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

all:
	$(MAKEU) PACKAGE=""
test-all:
	$(MAKEU) test PACKAGE=""
