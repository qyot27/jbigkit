# Select an ANSI/ISO C compiler here, GNU gcc is recommended
CC = gcc

# A high optimization level is suggested
CFLAGS = -O2 -Wall

all: libjbig.a test_codec

test_codec: test_codec.c jbig.c jbig.h jbig_tables.o
	$(CC) $(CCFLAGS) -o test_codec -DTEST_CODEC test_codec.c jbig.c \
	jbig_tables.o

libjbig.a: jbig.o jbig_tables.o
	rm -f libjbig.a
	ar rc libjbig.a jbig.o jbig_tables.o
	ranlib libjbig.a

jbig.o: jbig.c jbig.h

test: test_codec
	./test_codec

clean:
	rm -f *.o *~ core test_codec
