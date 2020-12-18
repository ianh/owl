.PHONY: install test sysinfo clean clean-js

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
CFLAGS?=-O3
CFLAGS+=-std=c11 -pedantic -Wall -Wno-missing-braces -Wno-overlength-strings
CFLAGS+=$(DEFINES)
EMFLAGS+=-s EXPORTED_FUNCTIONS='["_main","_fflush"]' -s ABORTING_MALLOC=0 -s MODULARIZE=1 -s EXPORT_NAME=Owl -s EXTRA_EXPORTED_RUNTIME_METHODS='["FS","ENV"]' -s ALLOW_MEMORY_GROWTH=1
LDFLAGS?=
LDLIBS?=$(LDLIBS_$(LIBDL))
EMCC?=emcc

owl: src/*.c src/*.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ src/*.c $(LDLIBS)

try/owl.js: src/*.c src/*.h
	$(EMCC) $(CFLAGS) $(LDFLAGS) $(EMFLAGS) -o $@ src/*.c $(LDLIBS)

install: owl
	$(INSTALL) -m 557 owl $(PREFIX)/bin/owl

test: owl
	sh -c 'cd test; for i in *.owltest; do ../owl -T "$$i" > "results/$$i.stdout" 2> "results/$$i.stderr"; done;:'
	sh -c 'TMP=`mktemp`; cd test; for i in *.owltest; do ../owl -T -c -o "$$TMP" "$$i" > "results/$$i.cc-stdout" 2> "results/$$i.cc-stderr"; done; rm "$$TMP";:'
	git diff --stat --exit-code test/results
	@echo "All tests passed."

sysinfo:
	@echo "OS=$(OS)"
	@echo "MAKE_VERSION=$(MAKE_VERSION)"
	uname -srv

clean:
	rm owl

clean-js:
	rm try/owl.js try/owl.wasm
