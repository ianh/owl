.PHONY: install clean clean-js

UNAME!=sh -c 'uname -s 2>/dev/null'
LIBDL_FreeBSD=no
LIBDL=$(LIBDL_$(UNAME))
LDLIBS_no=
LDLIBS_=-ldl

INSTALL?=/usr/bin/install
PREFIX?=/usr/local
CFLAGS?=-O3 -g
CFLAGS+=-std=c11 -pedantic -Wall -Wno-missing-braces -Wno-overlength-strings
EMFLAGS+=-s EXPORTED_FUNCTIONS='["_main","_fflush"]' -s ABORTING_MALLOC=0 -s MODULARIZE=1 -s EXPORT_NAME=Owl -s EXTRA_EXPORTED_RUNTIME_METHODS='["FS","ENV"]'
LDFLAGS?=
LDLIBS?=$(LDLIBS_$(LIBDL))

owl: *.c *.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ *.c $(LDLIBS)

try/owl.js: *.c *.h
	emcc $(CFLAGS) $(LDFLAGS) $(EMFLAGS) -o $@ *.c $(LDLIBS)

install: owl
	$(INSTALL) -m 557 owl $(PREFIX)/bin/owl

clean:
	rm owl

clean-js:
	rm try/owl.js try/owl.wasm try/owl.wast try/owl.wasm.map
