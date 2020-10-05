CC=gcc
CFLAGS=-lSDL2 -lvulkan

kernel :
	$(CC) c/kernel.c $(CFLAGS) -o c/kernel

chibi-scheme :
	make -C chibi-scheme

clean :
	rm c/kernel
