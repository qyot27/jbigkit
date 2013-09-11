ACLOCAL_AMFLAGS = -I m4
AUTOMAKE_OPTIONS = subdir-objects

lib_LTLIBRARIES = libjbig.la libjbig85.la
bin_PROGRAMS = pbmtojbg jbgtopbm pbmtojbg85 jbgtopbm85
dist_man_MANS = pbmtools/jbgtopbm.1 pbmtools/pbmtojbg.1 pbmtools/pbm.5 pbmtools/pgm.5
check_PROGRAMS = tstcodec tstcodec85

AM_CPPFLAGS = -I libjbig

include_HEADERS = libjbig/jbig.h libjbig/jbig85.h libjbig/jbig_ar.h

libjbig_la_SOURCES = libjbig/jbig.c libjbig/jbig_ar.c
libjbig85_la_SOURCES = libjbig/jbig85.c libjbig/jbig_ar.c

libjbig_la_LDFLAGS = -no-undefined -version-info \
		     @JBIG_LT_CURRENT@:@JBIG_LT_REVISION@:@JBIG_LT_AGE@
libjbig85_la_LDFLAGS = -no-undefined -version-info \
		     @JBIG85_LT_CURRENT@:@JBIG85_LT_REVISION@:@JBIG85_LT_AGE@

pbmtojbg_SOURCES = pbmtools/pbmtojbg.c
pbmtojbg85_SOURCES = pbmtools/pbmtojbg85.c
jbgtopbm_SOURCES = pbmtools/jbgtopbm.c
jbgtopbm85_SOURCES = pbmtools/jbgtopbm85.c

pbmtojbg_LDADD = libjbig.la
jbgtopbm_LDADD = libjbig.la
pbmtojbg85_LDADD = libjbig85.la
jbgtopbm85_LDADD = libjbig85.la

tstcodec_SOURCES = libjbig/tstcodec.c
tstcodec85_SOURCES = libjbig/tstcodec85.c

tstcodec_LDADD = libjbig.la
tstcodec85_LDADD = libjbig85.la
