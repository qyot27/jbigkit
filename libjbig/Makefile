# Unix makefile for the JBIG-KIT library
# $Id: Makefile,v 1.2 1995-06-08 16:43:07 mskuhn Exp $

# Select an ANSI/ISO C compiler here, GNU gcc is recommended
CC = gcc

# Options for the compiler: A high optimization level is suggested
CFLAGS = -O2 -Wall

all: libjbig.a test_codec

test_codec: test_codec.c jbig.c jbig.h jbig_tables.o
	$(CC) $(CFLAGS) -o test_codec -DTEST_CODEC test_codec.c jbig.c \
	jbig_tables.o

libjbig.a: jbig.o jbig_tables.o
	rm -f libjbig.a
	ar rc libjbig.a jbig.o jbig_tables.o
	ranlib libjbig.a

jbig.o: jbig.c jbig.h

test: test_codec
	./test_codec

clean:
	rm -f *.o *~ core gmon.out dbg_d=??.pbm test_codec
