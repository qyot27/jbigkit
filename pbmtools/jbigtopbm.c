/*
 *
 *  jbigtopbm - JBIG to Portable Bitmap converter
 *
 *  Markus Kuhn <mskuhn@cip.informatik.uni-erlangen.de> - 1995-05-10
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "jbig.h"

const char usage_msg[] = "JBIGtoPBM converter 0.1 -- "
"reads bi-level image entity (BIE) as input file\n\n"
"usage: %s [<options>] [<input-file> | -  [<output-file>]]\n\n"
"options:\n\n"
"  -x number\tif possible decode only up to a resolution layer not\n"
"\t\t\twider than the given number of pixels\n"
"  -y number\tif possible decode only up to a resolution layer not\n"
"\t\t\thigher than the given number of pixels\n\n";

char *progname;                  /* global pointer to argv[0] */


/*
 * Print usage message and abort
 */
void usage(void)
{
  fprintf(stderr, usage_msg, progname);
  exit(1);
}


int main (int argc, char **argv)
{
  FILE *fin = stdin, *fout = stdout;
  char *fnin = "<stdin>", *fnout = "<stdout>";
  int i, j, result;
  int all_args = 0, files = 0;
  struct jbg_dec_state s;
#define BUFSIZE 8192
  char buffer[BUFSIZE];
  unsigned char *p;
  size_t len, cnt;
  unsigned long xmax = 4294967295U, ymax = 4294967295U;

  /* parse command line arguments */
  progname = argv[0];
  for (i = 1; i < argc; i++) {
    if (!all_args && argv[i][0] == '-')
      if (argv[i][1] == '\0' && files == 0)
        ++files;
      else
        for (j = 1; j < 10000 && argv[i][j]; j++)
          switch(tolower(argv[i][j])) {
          case '-' :
            all_args = 1;
            break;
          case 'x':
            if (++i >= argc) usage();
            j = 10000;
            xmax = atol(argv[i]);
            break;
          case 'y':
            if (++i >= argc) usage();
            j = 10000;
            ymax = atol(argv[i]);
            break;
          default:
            usage();
          }
    else
      switch (files++) {
      case 0:
        if (argv[i][0] != '-' || argv[i][1] != '\0') {
          fnin = argv[i];
          fin = fopen(fnin, "rb");
          if (!fin) {
            fprintf(stderr, "Can't open input file '%s", fnin);
            perror("'");
            exit(1);
          }
        }
        break;
      case 1:
        fnout = argv[i];
        fout = fopen(fnout, "wb");
        if (!fout) {
          fprintf(stderr, "Can't open input file '%s", fnout);
          perror("'");
          exit(1);
        }
        break;
      default:
        usage();
      }
  }

  /* send input file to decoder */
  jbg_dec_init(&s);
  jbg_dec_maxsize(&s, xmax, ymax);
  result = JBG_EAGAIN;
  do {
    len = fread(buffer, 1, BUFSIZE, fin);
    if (!len) break;
    cnt = 0;
    p = (unsigned char *) buffer;
    while (len > 0 && (result == JBG_EAGAIN || result == JBG_EOK)) {
      result = jbg_dec_in(&s, p, len, &cnt);
      p += cnt;
      len -= cnt;
    }
  } while (result == JBG_EAGAIN || result == JBG_EOK);
  if (ferror(fin)) {
    fprintf(stderr, "Problem while reading input file '%s", fnin);
    perror("'");
    exit(1);
  }
  if (result != JBG_EOK && result != JBG_EOK_INTR) {
    fprintf(stderr, "Problem with input file '%s': %s\n",
	    fnin, jbg_strerror(result, JBG_EN));
    exit(1);
  }
  if (jbg_dec_getplanes(&s) > 1)
    fprintf(stderr, "Warning: storing only first of %d bit planes, ignoring "
	    "remaining %d planes.\n",
	    jbg_dec_getplanes(&s), jbg_dec_getplanes(&s) - 1);
  
  /* write PBM output file */
  fprintf(fout, "P4\n%ld %ld\n", jbg_dec_getwidth(&s), jbg_dec_gethight(&s));
  fwrite(jbg_dec_getimage(&s, 0), 1,
	 jbg_dec_getsize(&s), fout);

  /* check for file errors and close fout */
  if (ferror(fout) || fclose(fout)) {
    fprintf(stderr, "Problem while writing output file '%s", fnout);
    perror("'");
    exit(1);
  }

  jbg_dec_free(&s);
  
  return 0;
}
