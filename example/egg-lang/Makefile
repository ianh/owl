egg: parser.h parser.o main.c
	cc -g -Os -o egg main.c parser.o

parser.o: parser.h parser.c
	cc -g -Os -c parser.c

parser.h: grammar.owl
	../../owl -c grammar.owl -o parser.h
