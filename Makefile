.PHONY: install

bluebird: *.c
	cc -pedantic -std=c11 -Wall -o bluebird *.c

install: bluebird
	mv bluebird /usr/local/bin/bluebird
