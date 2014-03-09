CFLAGS=-fPIC
all: libcloglure.so example

libcloglure.so: gloopy.o
	$(CC) -shared -o $@ $^ -L/usr/local/lib -lEGL -lGLESv2 -lGLESv1_CM


example: example.c
	$(CC) -o $@ $< $(CFLAGS)  -L. -lcloglure

clean:
	-rm *.o *.a *.so
