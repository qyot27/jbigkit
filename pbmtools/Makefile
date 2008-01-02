# Unix makefile for the JBIG-KIT PBM tools
# $Id$

# Select an ANSI/ISO C compiler here, e.g. GNU gcc is recommended
CC = gcc

# Options for the compiler
CFLAGS = -g -Wall -ansi -pedantic -I../libjbig

.SUFFIXES: .1 .5 .txt $(SUFFIXES)

all: pbmtojbg jbgtopbm pbmtojbg.txt jbgtopbm.txt pbm.txt pgm.txt

pbmtojbg: pbmtojbg.o ../libjbig/libjbig.a
	$(CC) $(CFLAGS) -o pbmtojbg pbmtojbg.o -L../libjbig -ljbig

jbgtopbm: jbgtopbm.o ../libjbig/libjbig.a
	$(CC) $(CFLAGS) -o jbgtopbm jbgtopbm.o -L../libjbig -ljbig

pbmtojbg85: pbmtojbg85.o ../libjbig/libjbig85.a
	$(CC) $(CFLAGS) -o pbmtojbg85 pbmtojbg85.o -L../libjbig -ljbig85

jbgtopbm85: jbgtopbm85.o ../libjbig/libjbig85.a
	$(CC) $(CFLAGS) -o jbgtopbm85 jbgtopbm85.o -L../libjbig -ljbig85

jbgtopbm.o: jbgtopbm.c ../libjbig/jbig.h
pbmtojbg.o: pbmtojbg.c ../libjbig/jbig.h
jbgtopbm85.o: jbgtopbm85.c ../libjbig/jbig85.h
pbmtojbg85.o: pbmtojbg85.c ../libjbig/jbig85.h

../libjbig/libjbig.a: ../libjbig/jbig.c ../libjbig/jbig.h
	cd ../libjbig ; make libjbig.a

test: pbmtojbg jbgtopbm
	make IMG=ccitt1     OPTIONSP=                      dotest1
	make IMG=ccitt2     OPTIONSP=                      dotest1
	make IMG=ccitt3     OPTIONSP=                      dotest1
	make IMG=xvlogo    "OPTIONSP=-d 3"                 dotest1
	make IMG=sandra     OPTIONSP=      OPTIONSJ=       dotest2g
	make IMG=sandra     OPTIONSP=-b    OPTIONSJ=-b     dotest2g
	make IMG=sandra     OPTIONSP=-q    OPTIONSJ=       dotest2g
	make IMG=sandra    "OPTIONSP=-o 0" OPTIONSJ=       dotest2g
	make IMG=sandra    "OPTIONSP=-o 2" OPTIONSJ=       dotest2g
	make IMG=multi      OPTIONSP=      OPTIONSJ=       dotest2g
	make IMG=multi      OPTIONSP=-b    OPTIONSJ=-b     dotest2g
	make IMG=mx        "OPTIONSP=-q -s 3 -m 128"       dotest1
	make IMG=mx        "OPTIONSP=-q -s 3 -m 128"       dotest2b
	make IMG=mx        "OPTIONSP=-q -s 3 -m 128 -p 92" dotest2b
	make IMG=mx        "OPTIONSP=-q -Y -1"             dotest2b
	make IMG=mx        "OPTIONSP=-Y -1"                dotest2b
	rm -f test-*.jbg test-*.pbm test-*.pgm
	./jbgtopbm ../examples/ccitt1.jbg | ./pbmtojbg > test-ccitt1.jbg
	cmp ../examples/ccitt1.jbg test-ccitt1.jbg
	rm -f test-*.jbg test-*.pbm test-*.pgm
	./jbgtopbm < ../examples/ccitt1.jbg | ./pbmtojbg - test-ccitt1.jbg
	cmp ../examples/ccitt1.jbg test-ccitt1.jbg
	rm -f test-*.jbg test-*.pbm test-*.pgm
	./jbgtopbm < ../examples/ccitt1.jbg - test-ccitt1.pbm ; \
	  ./pbmtojbg test-ccitt1.pbm test-ccitt1.jbg
	cmp ../examples/ccitt1.jbg test-ccitt1.jbg
	rm -f test-*.jbg test-*.pbm test-*.pgm
	./jbgtopbm ../examples/ccitt1.jbg test-ccitt1.pbm ; \
	  ./pbmtojbg test-ccitt1.pbm >test-ccitt1.jbg
	cmp ../examples/ccitt1.jbg test-ccitt1.jbg
	rm -f test-*.jbg test-*.pbm test-*.pgm
	@echo
	@echo "The pbmtools have PASSED the functional tests. Good!"
	@echo

dotest1:
	./jbgtopbm ../examples/$(IMG).jbg test-$(IMG).pbm
	./pbmtojbg $(OPTIONSP) test-$(IMG).pbm test-$(IMG).jbg
	cmp test-$(IMG).jbg ../examples/$(IMG).jbg

dotest2b:
	./pbmtojbg $(OPTIONSP) test-$(IMG).pbm test-$(IMG).jbg
	./jbgtopbm $(OPTIONSJ) test-$(IMG).jbg test-$(IMG)-2.pbm
	cmp test-$(IMG).pbm test-$(IMG)-2.pbm

dotest2g:
	./pbmtojbg $(OPTIONSP) ../examples/$(IMG).pgm test-$(IMG).jbg
	./jbgtopbm $(OPTIONSJ) test-$(IMG).jbg test-$(IMG).pgm
	cmp test-$(IMG).pgm ../examples/$(IMG).pgm

test85: pbmtojbg jbgtopbm pbmtojbg85 
	make IMG=ccitt1 OPTIONSP= dotest85
	rm -f test-*.jbg test-*.jbg85 test-*.pbm

dotest85:
	./jbgtopbm ../examples/$(IMG).jbg test-$(IMG).pbm
	./pbmtojbg -f $(OPTIONSP) test-$(IMG).pbm test-$(IMG).jbg
	./pbmtojbg85  $(OPTIONSP) test-$(IMG).pbm test-$(IMG).jbg85
	cmp test-$(IMG).jbg test-$(IMG).jbg85

.1.txt .5.txt:
	groff -man -Tascii -P -c -P -b -P -u $< >$@

clean:
	rm -f *.o *~ core pbmtojbg jbgtopbm
	rm -f test-*.jbg test-*.pbm test-*.pgm
