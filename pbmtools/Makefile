# Select an ANSI/ISO C compiler here, e.g. GNU gcc is recommended
CC = gcc

# A high optimization level is suggested
CFLAGS = -O2 -W -I../libjbig

all: pbmtojbig jbigtopbm

pbmtojbig: pbmtojbig.o ../libjbig/libjbig.a
	$(CC) -o pbmtojbig pbmtojbig.o -L../libjbig -ljbig

jbigtopbm: jbigtopbm.o ../libjbig/libjbig.a
	$(CC) -o jbigtopbm jbigtopbm.o -L../libjbig -ljbig

jbigtopbm.o: jbigtopbm.c ../libjbig/jbig.h
pbmtojbig.o: pbmtojbig.c ../libjbig/jbig.h

../libjbig/libjbig.a:
	cd ../libjbig ; make libjbig.a

clean:
	rm -f *.o *~ core pbmtojbig jbigtopbm
