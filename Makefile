.PHONY: install

bluebird: *.c
	cc -o bluebird *.c

install: bluebird
	mv bluebird /usr/local/bin/bluebird
