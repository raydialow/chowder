CC=gcc
CFLAGS=`pkg-config --static --libs vulkan glfw3`

kernel :
	$(CC) c/kernel.c $(CFLAGS) -Wall -o c/kernel

clean-kernel :
	rm c/kernel
