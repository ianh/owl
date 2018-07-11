.PHONY: install clean

UNAME!=sh -c 'uname -s 2>/dev/null'
LIBDL_FreeBSD=no
LIBDL=$(LIBDL_$(UNAME))
LDLIBS_no=
LDLIBS_=-ldl

INSTALL?=/usr/bin/install
PREFIX?=/usr/local
CFLAGS?=-O3 -g -pedantic -Wall -Wno-missing-braces -Wno-overlength-strings
LDFLAGS?=
LDLIBS?=$(LDLIBS_$(LIBDL))

owl: 1-parse.o *.c *.h
	$(CC) $(CFLAGS) -std=c11 $(LDFLAGS) -o owl 1-parse.o *.c $(LDLIBS)

1-parse.o: 1-parse.h
	$(CC) $(CFLAGS) -std=c11 -c -x c -DOWL_PARSER_IMPLEMENTATION -o 1-parse.o 1-parse.h

install: owl
	$(INSTALL) -m 557 owl $(PREFIX)/bin/owl

clean:
	rm owl 1-parse.o
