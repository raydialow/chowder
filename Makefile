CC=gcc
CFLAGS=`pkg-config --static --libs vulkan glfw3`

kernel :
	$(CC) -shared -o src/kernel.so $(CFLAGS) -Wall -fPIC src/kernel.h

clean :
	rm src/kernel.so
