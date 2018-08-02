hex-color: parser.h parser.o main.c
	cc -g -std=c99 -Os -o hex-color main.c parser.o

parser.o: parser.h parser.c
	cc -g -std=c99 -Os -c parser.c

parser.h: grammar.owl
	../../owl -c grammar.owl -o parser.h
