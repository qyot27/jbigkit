# Unix makefile for the JBIG-KIT PBM tools
# $Id: Makefile,v 1.5 1996-01-09 15:23:21 mskuhn Exp $

# Select an ANSI/ISO C compiler here, e.g. GNU gcc is recommended
CC = gcc

# Options for the compiler
CFLAGS = -g -Wall -ansi -pedantic -I../libjbig

all: pbmtojbg jbgtopbm

pbmtojbg: pbmtojbg.o ../libjbig/libjbig.a
	$(CC) $(CFLAGS) -o pbmtojbg pbmtojbg.o -L../libjbig -ljbig

jbgtopbm: jbgtopbm.o ../libjbig/libjbig.a
	$(CC) $(CFLAGS) -o jbgtopbm jbgtopbm.o -L../libjbig -ljbig

jbgtopbm.o: jbgtopbm.c ../libjbig/jbig.h
pbmtojbg.o: pbmtojbg.c ../libjbig/jbig.h

../libjbig/libjbig.a:
	cd ../libjbig ; make libjbig.a

clean:
	rm -f *.o *~ core pbmtojbg jbgtopbm
