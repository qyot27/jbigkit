# Unix makefile for the JBIG-KIT PBM tools
# $Id: Makefile,v 1.2 1995-06-08 16:44:30 mskuhn Exp $

# Select an ANSI/ISO C compiler here, e.g. GNU gcc is recommended
CC = gcc

# Options for the compiler
CFLAGS = -O2 -W -I../libjbig

all: pbmtojbig jbigtopbm

pbmtojbig: pbmtojbig.o ../libjbig/libjbig.a
	$(CC) $(CFLAGS) -o pbmtojbig pbmtojbig.o -L../libjbig -ljbig

jbigtopbm: jbigtopbm.o ../libjbig/libjbig.a
	$(CC) $(CFLAGS) -o jbigtopbm jbigtopbm.o -L../libjbig -ljbig

jbigtopbm.o: jbigtopbm.c ../libjbig/jbig.h
pbmtojbig.o: pbmtojbig.c ../libjbig/jbig.h

../libjbig/libjbig.a:
	cd ../libjbig ; make libjbig.a

clean:
	rm -f *.o *~ core pbmtojbig jbigtopbm
