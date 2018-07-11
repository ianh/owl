.PHONY: install clean

UNAME!=sh -c 'uname -s 2>/dev/null'
LIBDL_FreeBSD=no
LIBDL=$(LIBDL_$(UNAME))
LDLIBS_no=
LDLIBS_=-ldl

INSTALL?=/usr/bin/install
PREFIX?=/usr/local
CFLAGS?=-O3 -g
CFLAGS+=-std=c11 -pedantic -Wall -Wno-missing-braces -Wno-overlength-strings
LDFLAGS?=
LDLIBS?=$(LDLIBS_$(LIBDL))

owl: *.c *.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ *.c $(LDLIBS)

install: owl
	$(INSTALL) -m 557 owl $(PREFIX)/bin/owl

clean:
	rm owl
