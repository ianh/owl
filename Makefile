.PHONY: install test clean clean-js

UNAME!=sh -c 'uname -s 2>/dev/null'
OS?=$(UNAME)
LIBDL_FreeBSD=no
LIBDL_Windows_NT=no
LIBDL=$(LIBDL_$(OS))
LDLIBS_no=
LDLIBS_=-ldl
DEFINES_Windows_NT=-DNOT_UNIX
DEFINES=$(DEFINES_$(OS))

INSTALL?=/usr/bin/install
PREFIX?=/usr/local
CFLAGS?=-O3 -g
CFLAGS+=-std=c11 -pedantic -Wall -Wno-missing-braces -Wno-overlength-strings
CFLAGS+=$(DEFINES)
EMFLAGS+=-s EXPORTED_FUNCTIONS='["_main","_fflush"]' -s ABORTING_MALLOC=0 -s MODULARIZE=1 -s EXPORT_NAME=Owl -s EXTRA_EXPORTED_RUNTIME_METHODS='["FS","ENV"]'
LDFLAGS?=
LDLIBS?=$(LDLIBS_$(LIBDL))
EMCC?=emcc

owl: *.c *.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ *.c $(LDLIBS)

try/owl.js: *.c *.h
	$(EMCC) $(CFLAGS) $(LDFLAGS) $(EMFLAGS) -o $@ *.c $(LDLIBS)

install: owl
	$(INSTALL) -m 557 owl $(PREFIX)/bin/owl

test: owl
	sh -c 'cd test; for i in *.owl; do ../owl -i /dev/null "$$i" > "results/$$i.stdout" 2> "results/$$i.stderr"; done;:'
	sh -c 'cd test; for i in *.owltest; do ../owl -T "$$i" > "results/$$i.stdout" 2> "results/$$i.stderr"; done;:'
	git diff test/results

clean:
	rm owl

clean-js:
	rm try/owl.js try/owl.wasm try/owl.wast try/owl.wasm.map
