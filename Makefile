.PHONY: install

bluebird: *.c *.h
	/usr/bin/cc -Os -g -pedantic -std=c11 -Wall -Wno-missing-braces -Wno-overlength-strings -o bluebird *.c

install: bluebird
	mv bluebird /usr/local/bin/bluebird
