# Unix makefile for the JBIG-KIT PBM tools
# $Id: Makefile,v 1.7 1996-02-05 12:55:23 mskuhn Exp $

# Select an ANSI/ISO C compiler here, e.g. GNU gcc is recommended
CC = gcc

# Options for the compiler
CFLAGS = -g -Wall -ansi -pedantic -I../libjbig

all: pbmtojbg jbgtopbm pbmtojbg.txt jbgtopbm.txt pbm.txt

pbmtojbg: pbmtojbg.o ../libjbig/libjbig.a
	$(CC) $(CFLAGS) -o pbmtojbg pbmtojbg.o -L../libjbig -ljbig

jbgtopbm: jbgtopbm.o ../libjbig/libjbig.a
	$(CC) $(CFLAGS) -o jbgtopbm jbgtopbm.o -L../libjbig -ljbig

jbgtopbm.o: jbgtopbm.c ../libjbig/jbig.h
pbmtojbg.o: pbmtojbg.c ../libjbig/jbig.h

../libjbig/libjbig.a:
	cd ../libjbig ; make libjbig.a

pbmtojbg.txt: pbmtojbg.1
	gtroff -man -Tascii pbmtojbg.1 | grotty - -u -b >pbmtojbg.txt

jbgtopbm.txt: jbgtopbm.1
	gtroff -man -Tascii jbgtopbm.1 | grotty - -u -b >jbgtopbm.txt

pbm.txt: pbm.5
	gtroff -man -Tascii pbm.5 | grotty - -u -b >pbm.txt

clean:
	rm -f *.o *~ core pbmtojbg jbgtopbm
