CC=chicken-csc

all :
	$(CC) src/main.scm -o bin/main

clean :
	rm bin/main
