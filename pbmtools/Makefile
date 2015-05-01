# GNU makefile for the JBIG-KIT PBM tools

.DELETE_ON_ERROR:

# Select an ANSI/ISO C compiler here, e.g. GNU gcc is recommended
CC = $(CROSS)gcc
AR = $(CROSS)ar
LD = $(CROSS)ld
RANLIB = $(CROSS)ranlib

# Options for the compiler
CFLAGS = -g -O -W -Wall -Wno-unused-result -ansi -pedantic # --coverage
CPPFLAGS = -I../libjbig $(XCFLAGS)
LDFLAGS = -L../libjbig

.PHONY: txt pdf test test82 test85 clean

all: pbmtojbg jbgtopbm pbmtojbg85 jbgtopbm85 txt

txt: pbmtojbg.txt jbgtopbm.txt pbm.txt pgm.txt

pdf: pbmtojbg.pdf jbgtopbm.pdf pbm.pdf pgm.pdf

pbmtojbg: pbmtojbg.o ../libjbig/libjbig.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o pbmtojbg pbmtojbg.o -ljbig

jbgtopbm: jbgtopbm.o ../libjbig/libjbig.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o jbgtopbm jbgtopbm.o -ljbig

pbmtojbg85: pbmtojbg85.o ../libjbig/libjbig85.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o pbmtojbg85 pbmtojbg85.o -ljbig85

jbgtopbm85: jbgtopbm85.o ../libjbig/libjbig85.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o jbgtopbm85 jbgtopbm85.o -ljbig85

jbgtopbm.o: jbgtopbm.c ../libjbig/jbig.h
pbmtojbg.o: pbmtojbg.c ../libjbig/jbig.h
jbgtopbm85.o: jbgtopbm85.c ../libjbig/jbig85.h
pbmtojbg85.o: pbmtojbg85.c ../libjbig/jbig85.h

../libjbig/libjbig.a: ../libjbig/jbig.c ../libjbig/jbig.h \
	../libjbig/jbig_ar.c ../libjbig/jbig_ar.h
	$(MAKE) -C ../libjbig libjbig.a

../libjbig/libjbig85.a: ../libjbig/jbig85.c ../libjbig/jbig85.h \
	../libjbig/jbig_ar.c ../libjbig/jbig_ar.h
	$(MAKE) -C ../libjbig libjbig85.a

analyze:
	clang $(CPPFLAGS) --analyze *.c

test: test82 test85

test82: pbmtojbg jbgtopbm
	$(MAKE) IMG=ccitt1     OPTIONSP=                      dotest1
	$(MAKE) IMG=ccitt2     OPTIONSP=                      dotest1
	$(MAKE) IMG=ccitt3     OPTIONSP=                      dotest1
	$(MAKE) IMG=xvlogo    "OPTIONSP=-d 3"                 dotest1
	$(MAKE) IMG=sandra     OPTIONSP=      OPTIONSJ=       dotest2g
	$(MAKE) IMG=sandra     OPTIONSP=-b    OPTIONSJ=-b     dotest2g
	$(MAKE) IMG=sandra     OPTIONSP=-q    OPTIONSJ=       dotest2g
	$(MAKE) IMG=sandra    "OPTIONSP=-o 0" OPTIONSJ=       dotest2g
	$(MAKE) IMG=sandra    "OPTIONSP=-o 2" OPTIONSJ=       dotest2g
	$(MAKE) IMG=multi      OPTIONSP=      OPTIONSJ=       dotest2g
	$(MAKE) IMG=multi      OPTIONSP=-b    OPTIONSJ=-b     dotest2g
	$(MAKE) IMG=mx        "OPTIONSP=-q -s 3 -m 127"       dotest1
	$(MAKE) IMG=mx        "OPTIONSP=-q -s 3 -m 127"       dotest2b
	$(MAKE) IMG=mx        "OPTIONSP=-q -s 3 -m 127 -p 92" dotest2b
	$(MAKE) IMG=mx        "OPTIONSP=-q -Y -1"             dotest2b
	$(MAKE) IMG=mx        "OPTIONSP=-Y -1"                dotest2b
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

test85: pbmtojbg jbgtopbm pbmtojbg85 jbgtopbm85 test-t82.pbm
	$(MAKE) IMG=t82 "OPTIONSP=-p 0"      dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-p 8"      dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-p 8 -r"   dotest85b
	$(MAKE) IMG=t82 "OPTIONSP=-p 64"     dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-p 72"     dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-s 2 -C c" dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-s 99999"  dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-Y 9999 0" dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-Y 1951 0" dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-Y -1 127" dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-Y -1 128" dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-Y -1 1919" dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-Y -1 1920" dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-Y -1 1949" dotest85
	$(MAKE) IMG=t82 "OPTIONSP=-Y -1 1950" dotest85
	$(MAKE) IMG=ccitt1 dotest85
	$(MAKE) IMG=ccitt2 dotest85
	$(MAKE) IMG=ccitt3 dotest85
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
	rm test-$(IMG).pbm85
	./jbgtopbm85 -B 1 test-$(IMG).jbg85 test-$(IMG).pbm85
	cmp test-$(IMG).pbm test-$(IMG).pbm85

dotest85b: test-$(IMG).pbm
	./pbmtojbg -f $(OPTIONSP) test-$(IMG).pbm test-$(IMG).jbg85
	ls -l test-$(IMG).jbg85
	./jbgtopbm test-$(IMG).jbg85 test-$(IMG).pbm85
	cmp test-$(IMG).pbm test-$(IMG).pbm85
	rm test-$(IMG).pbm85
	./jbgtopbm85 test-$(IMG).jbg85 test-$(IMG).pbm85
	cmp test-$(IMG).pbm test-$(IMG).pbm85
	rm test-$(IMG).pbm85
	./jbgtopbm85 -B 1 test-$(IMG).jbg85 test-$(IMG).pbm85
	cmp test-$(IMG).pbm test-$(IMG).pbm85

test-%.pbm: ../examples/%.jbg
	./jbgtopbm $< $@

test-t82.pbm:
	$(MAKE) -C ../libjbig tstcodec
	../libjbig/tstcodec $@

FOPT=-c 1000 -p 300000 -m 3
fuzz: test-t82.pbm
	while \
	  ./pbmtojbg -f test-t82.pbm | ./jbgfuzz.pl $(FOPT) && \
	  ./pbmtojbg test-t82.pbm | ./jbgfuzz.pl $(FOPT) -d jbgtopbm ; \
	do true; done

MAN2TXT=groff -man -Tascii -P -c -P -b -P -u
%.txt: %.1
	$(MAN2TXT) $< >$@
%.txt: %.5
	$(MAN2TXT) $< >$@
MAN2PS=groff -man -Tps
%.ps: %.1
	$(MAN2PS) $< >$@
%.ps: %.5
	$(MAN2PS) $< >$@
%.pdf: %.ps
	ps2pdf $<


clean:
	rm -f *.o *~ core pbmtojbg jbgtopbm pbmtojbg85 jbgtopbm85
	rm -f test-*.jbg test-*.pbm test-*.pgm test-*.jbg85 test-*.pbm85
	rm -f *.gcda *.gcno *.plist
	rm -f *.ps *.pdf
