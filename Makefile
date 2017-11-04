.PHONY: install

bluebird: *.c
	cc -pedantic -std=c11 -Wall -Wno-missing-braces -o bluebird *.c

install: bluebird
	mv bluebird /usr/local/bin/bluebird
