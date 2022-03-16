# This Makefile is not really that useful; it is mainly intended to add an
# uninstall script, since it is not well-implemented in Stack.
# See: https://stackoverflow.com/a/38639959/2397327
PKG=fun-lazy-compiler
BINARY=flc

.PHONY: install uninstall test build clean

build:
	stack build

test:
	stack test

install:
	stack install

uninstall:
	stack exec ghc-pkg unregister ${PKG}
	rm -f $(shell whereis ${BINARY} | awk '{print $$2}')

clean:
	stack clean
