# This Makefile is not really that useful; it is mainly intended to add an
# uninstall script, since it is not well-implemented in Stack.
# See: https://stackoverflow.com/a/38639959/2397327
PKG=fun-lazy-compiler
BINARY=flc

.PHONY: install uninstall test build clean

test:
	stack test

build:
	stack build

install:
	# Install to a default location (e.g., `~/.local/bin`)
	stack install

uninstall:
	# Remove installation (e.g., from `~/.local/bin`)
	stack exec ghc-pkg unregister ${PKG}
	rm -f $(shell whereis ${BINARY} | awk '{print $$2}')

clean:
	stack clean
