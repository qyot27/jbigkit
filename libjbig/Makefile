# Unix makefile for the JBIG-KIT library

# Select an ANSI/ISO C compiler here, GNU gcc is recommended
CC = $(CROSS)gcc
AR = $(CROSS)ar
LD = $(CROSS)ld
RANLIB = $(CROSS)ranlib

# Options for the compiler: A high optimization level is suggested
CFLAGS = -g -O -W -Wall -ansi -pedantic $(XCFLAGS) # --coverage

all: libjbig.a libjbig85.a tstcodec tstcodec85 tstjoint

tstcodec: tstcodec.o jbig.o jbig_ar.o
	$(CC) $(LDFLAGS) $(CFLAGS) -o tstcodec tstcodec.o jbig.o jbig_ar.o

tstcodec85: tstcodec85.o jbig85.o jbig_ar.o
	$(CC) $(LDFLAGS) $(CFLAGS) -o tstcodec85 tstcodec85.o jbig85.o jbig_ar.o

tstjoint: tstjoint.o jbig.o jbig85.o jbig_ar.o
	$(CC) $(LDFLAGS) $(CFLAGS) -o tstjoint \
	  tstjoint.o jbig.o jbig85.o jbig_ar.o

libjbig.a: jbig.o jbig_ar.o
	rm -f libjbig.a
	$(AR) rc libjbig.a jbig.o jbig_ar.o
	-$(RANLIB) libjbig.a

libjbig85.a: jbig85.o jbig_ar.o
	rm -f libjbig85.a
	$(AR) rc libjbig85.a jbig85.o jbig_ar.o
	-$(RANLIB) libjbig85.a

jbig.o: jbig.c jbig.h jbig_ar.h
jbig85.o: jbig85.c jbig85.h jbig_ar.h
jbig_ar.o: jbig_ar.c jbig_ar.h
tstcodec.o: tstcodec.c jbig.h
tstcodec85.o: tstcodec85.c jbig85.h

update-po: jbig.c jbig85.c Makefile
	xgettext -ojbig.pot -k_ \
	  --copyright-holder='Markus Kuhn' \
	  --msgid-bugs-address='http://www.cl.cam.ac.uk/~mgk25/jbigkit/' \
	  --package-name jbigkit \
	jbig.c jbig85.c
	cd po && for po in *.po ; do \
	  msgmerge --update $$po ../jbig.pot ; done

analyze:
	clang --analyze *.c

test: tstcodec tstcodec85 tstjoint
	./tstcodec
	./tstcodec85
	./tstjoint

t82test.pbm: tstcodec
	./tstcodec $@

clean:
	rm -f *.o *.gcda *.gcno *.gcov *.plist *~ core gmon.out dbg_d\=??.pbm
	rm -f t82test.pbm
	rm -f tstcodec tstcodec85 tstjoint
