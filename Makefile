# Unix makefile for JBIG-KIT
# $Id: Makefile,v 1.1 1995-06-10 18:46:10 mskuhn Exp $

# Select an ANSI/ISO C compiler here, GNU gcc is recommended
CC = gcc

# Options for the compiler: A high optimization level is suggested
CCFLAGS = -O2 -W
#CCFLAGS = -O -g -Wall -ansi -pedantic   # developer only

CFLAGS = $(CCFLAGS) -I../libjbig

all: lib pbm
	@echo "Enter 'make test' in order to start some automatic tests."

lib:
	(cd libjbig; make "CC=$(CC)" "CFLAGS=$(CFLAGS)")

pbm: lib
	(cd pbmtools; make "CC=$(CC)" "CFLAGS=$(CFLAGS)")

test:
	(cd libjbig; make "CC=$(CC)" "CFLAGS=$(CFLAGS)" test)

clean:
	rm -f *~ core
	(cd libjbig; make clean)
	(cd pbmtools; make clean)

distribution: clean
	rm -f libjbig/libjbig.a
	(cd ..; tar -c -v --exclude RCS -f jbigkit.tar jbigkit)
