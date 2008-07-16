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

../libjbig/libjbig.a: ../libjbig/jbig.c ../libjbig/jbig.h \
	../libjbig/jbig_ar.c ../libjbig/jbig_ar.h
	make -C ../libjbig libjbig.a

../libjbig/libjbig85.a: ../libjbig/jbig85.c ../libjbig/jbig85.h \
	../libjbig/jbig_ar.c ../libjbig/jbig_ar.h
	make -C ../libjbig libjbig85.a

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

test85: pbmtojbg jbgtopbm pbmtojbg85 jbgtopbm85
	make IMG=ccitt1 "OPTIONSP=-p 0"      dotest85
	make IMG=ccitt1 "OPTIONSP=-p 8"      dotest85
	make IMG=ccitt1 "OPTIONSP=-p 8 -r"   dotest85b
	make IMG=ccitt1 "OPTIONSP=-p 64"     dotest85
	make IMG=ccitt1 "OPTIONSP=-p 72"     dotest85
	make IMG=ccitt1 "OPTIONSP=-s 2"      dotest85
	make IMG=ccitt1 "OPTIONSP=-s 99999"  dotest85
	make IMG=ccitt1 "OPTIONSP=-Y 9999 0" dotest85
	make IMG=ccitt1 "OPTIONSP=-Y -1 127" dotest85
	make IMG=ccitt1 "OPTIONSP=-Y -1 128" dotest85
	make IMG=ccitt1 "OPTIONSP=-Y -1 2303" dotest85
	make IMG=ccitt1 "OPTIONSP=-Y -1 2304" dotest85
	make IMG=ccitt1 "OPTIONSP=-Y -1 2374" dotest85
	make IMG=ccitt1 "OPTIONSP=-Y -1 2375" dotest85
	make IMG=ccitt2 dotest85
	make IMG=ccitt3 dotest85
	rm -f test-*.jbg test-*.jbg85 test-*.pbm
	@echo
	@echo "The T.85 pbmtools have PASSED the functional tests. Good!"
	@echo

dotest85: test-$(IMG).pbm
	./pbmtojbg85  $(OPTIONSP) test-$(IMG).pbm test-$(IMG).jbg85
	ls -l test-$(IMG).jbg85
	./jbgtopbm test-$(IMG).jbg85 test-$(IMG).pbm85
	cmp test-$(IMG).pbm test-$(IMG).pbm85
	rm test-$(IMG).pbm85
	./jbgtopbm85 test-$(IMG).jbg85 test-$(IMG).pbm85
	cmp test-$(IMG).pbm test-$(IMG).pbm85

dotest85b: test-$(IMG).pbm
	./pbmtojbg -f $(OPTIONSP) test-$(IMG).pbm test-$(IMG).jbg85
	ls -l test-$(IMG).jbg85
	./jbgtopbm test-$(IMG).jbg85 test-$(IMG).pbm85
	cmp test-$(IMG).pbm test-$(IMG).pbm85
	rm test-$(IMG).pbm85
	./jbgtopbm85 test-$(IMG).jbg85 test-$(IMG).pbm85
	cmp test-$(IMG).pbm test-$(IMG).pbm85

test-%.pbm: ../examples/%.jbg
	./jbgtopbm $< $@

.1.txt .5.txt:
	groff -man -Tascii -P -c -P -b -P -u $< >$@

clean:
	rm -f *.o *~ core pbmtojbg jbgtopbm
	rm -f test-*.jbg test-*.pbm test-*.pgm test-*.jbg85 test-*.pbm85
