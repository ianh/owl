.PHONY: install

UNAME!=sh -c 'uname -s 2>/dev/null'
LIBDL_FreeBSD=no
LIBDL=$(LIBDL_$(UNAME))
LDLIBS_no=
LDLIBS_=-ldl

CC=/usr/bin/cc
CFLAGS=-O3 -g -pedantic -std=c11 -Wall -Wno-missing-braces -Wno-overlength-strings
LDLIBS=$(LDLIBS_$(LIBDL))

owl: 1-parse.o *.c *.h
	$(CC) $(CFLAGS) -o owl 1-parse.o *.c $(LDLIBS)

1-parse.o: 1-parse.h
	$(CC) $(CFLAGS) -c -x c -DOWL_PARSER_IMPLEMENTATION -o 1-parse.o 1-parse.h

install: owl
	mv owl /usr/local/bin/owl
