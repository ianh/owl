calc: parser.h calc.c
	cc -g -std=c99 -Os -o calc calc.c -lreadline

parser.h: calc.owl
	../../owl -c calc.owl -o parser.h
