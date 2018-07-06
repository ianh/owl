calc: parser.h calc.c
	cc -g -Os -o calc calc.c -lreadline

parser.h: calc.bb
	../../bluebird -c calc.bb -o parser.h
