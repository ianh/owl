.PHONY: install

bluebird: 1-parse.o *.c *.h
	/usr/bin/cc -O0 -g -pedantic -std=c11 -Wall -Wno-missing-braces -Wno-overlength-strings -o bluebird 1-parse.o *.c -ldl

1-parse.o: 1-parse.h
	/usr/bin/cc -O0 -g -pedantic -std=c11 -Wall -Wno-missing-braces -Wno-overlength-strings -c -x c -DBLUEBIRD_PARSER_IMPLEMENTATION -o 1-parse.o 1-parse.h

install: bluebird
	mv bluebird /usr/local/bin/bluebird
