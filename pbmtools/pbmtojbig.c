/*
 *
 *  pbmtojbig - Portable Bitmap to JBIG converter
 *
 *  Markus Kuhn <mskuhn@cip.informatik.uni-erlangen.de> - 1995-05-07
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "jbig.h"

const char usage_msg[] = "PBMtoJBIG converter 0.1 -- "
"creates bi-level image entity (BIE) as output file\n\n"
"usage: %s [<options>] [<input-file> | -  [<output-file>]]\n\n"
"options:\n\n"
"  -q\t\tsequential coding, no differential layers (like -d 0)\n"
"  -x number\tmaximum width of lowest resolution layer (default 640)\n"
"  -y number\tmaximum height of lowest resolution layer (default 480)\n"
"  -l number\tlowest layer written to output file (default 0)\n"
"  -h number\thighest layer written to output file (default max)\n"
"  -d number\ttotal number of differential layers (overrides -x and -y)\n"
"  -s number\theight of a stripe in layer 0\n"
"  -m number\tmaximum adaptive template pixel horizontal offset (default 8)\n"
"  -o number\torder byte value: add 1=SMID, 2=ILEAVE, 4=SEQ, 8=HITOLO\n"
"\t\t(default 3 = ILEAVE+SMID)\n"
"  -p number\toptions byte value: add DPON=4, TPBON=8, TPDON=16, LRLTWO=64\n"
"\t\t(default 28 = DPON+TPBON+TPDON)\n"
"  -c\t\tdelay adaptive template changes to first line of next stripe\n"
"\t\t(only provided for a conformance test)\n"
"  -v\t\tverbose output\n\n";

char *progname;                  /* global pointer to argv[0] */
unsigned long total_length = 0;  /* used for determining output file length */


/* 
 * Read an ASCII integer number from file f and skip any PBM
 * comments which are encountered.
 */
unsigned long getint(FILE *f)
{
  int c;
  unsigned long i;

  while ((c = getc(f)) != EOF && !isdigit(c))
    if (c == '#')
      while ((c = getc(f)) != EOF && !(c == 13 || c == 10)) ;
  if (c != EOF) {
    ungetc(c, f);
    fscanf(f, "%lu", &i);
  }

  return i;
}


/*
 * Callback procedure which is used by JBIG encoder to deliver the
 * encoded data. It simply sends the bytes to the output file.
 */
void data_out(unsigned char *start, size_t len, void *file)
{
  fwrite(start, len, 1, (FILE *) file);
  total_length += len;
  return;
}


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
  int c, i, j;
  int all_args = 0, files = 0;
  unsigned long width, height;
  size_t bitmap_size;
  char type;
  unsigned char *bitmap, *p;
  struct jbg_enc_state s;
  int verbose = 0, delay_at = 0;
  long mwidth = 640, mheight = 480;
  int dl = -1, dh = -1, d = -1, l0 = -1, mx = -1;
  int options = JBG_TPDON | JBG_TPBON | JBG_DPON;
  int order = JBG_ILEAVE | JBG_SMID;


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
	  case 'v':
	    verbose = 1;
	    break;
	  case 'c':
	    delay_at = 1;
	    break;
	  case 'x':
	    if (++i >= argc) usage();
	    j = 10000;
	    mwidth = atol(argv[i]);
	    break;
	  case 'y':
	    if (++i >= argc) usage();
	    j = 10000;
	    mheight = atol(argv[i]);
	    break;
	  case 'o':
	    if (++i >= argc) usage();
	    j = 10000;
	    order = atoi(argv[i]);
	    break;
	  case 'p':
	    if (++i >= argc) usage();
	    j = 10000;
	    options = atoi(argv[i]);
	    break;
	  case 'l':
	    if (++i >= argc) usage();
	    j = 10000;
	    dl = atoi(argv[i]);
	    break;
	  case 'h':
	    if (++i >= argc) usage();
	    j = 10000;
	    dh = atoi(argv[i]);
	    break;
	  case 'q':
	    d = 0;
	    break;
	  case 'd':
	    if (++i >= argc) usage();
	    j = 10000;
	    d = atoi(argv[i]);
	    break;
	  case 's':
	    if (++i >= argc) usage();
	    j = 10000;
	    l0 = atoi(argv[i]);
	    break;
	  case 'm':
	    if (++i >= argc) usage();
	    j = 10000;
	    mx = atoi(argv[i]);
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
  
  /* read PBM header */
  while ((c = getc(fin)) != EOF && (isspace(c) || c == '#'))
    if (c == '#')
      while ((c = getc(fin)) != EOF && !(c == 13 || c == 10)) ;
  if (c != 'P') {
    fprintf(stderr, "Input file '%s' does not look like a PBM file!\n", fnin);
    exit(1);
  }
  type = getc(fin);
  width = getint(fin);
  height = getint(fin);
  getc(fin);    /* skip line feed */

  /* read PBM image data */
  bitmap_size = ((((width - 1) | 7) + 1) >> 3) * (size_t) height;
  bitmap = malloc(sizeof(unsigned char) * bitmap_size);
  if (!bitmap) {
    fprintf(stderr, "Sorry, not enough memory available!\n");
    exit(1);
  }
  p = bitmap;
  switch (type) {
  case '1':
    /* PBM text format */
    for (i = 0; i < height; i++)
      for (j = 0; j < width | 7; j++) {
	*p <<= 1;
	if (j < width)
	  *p |= getint(fin) & 1;
	if ((j & 7) == 7)
	  ++p;
      }
    break;
  case '4':
    /* PBM raw binary format */
    fread(bitmap, bitmap_size, 1, fin);
    break;
  default:
    fprintf(stderr, "Unsupported PBM type P%c!\n", type);
    exit(1);
  }
  if (ferror(fin)) {
    fprintf(stderr, "Problem while reading input file '%s", fnin);
    perror("'");
    exit(1);
  }
  if (feof(fin)) {
    fprintf(stderr, "Unexpected end of input file '%s'!\n", fnin);
    exit(1);
  }

  /* Apply JBIG algorithm and write BIE to output file */

  /* initialize parameter struct for JBIG encoder*/
  jbg_enc_init(&s, width, height, 1, &bitmap, data_out, fout);

  /* Select number of resolution layers either directly or based
   * on a given maximum size for the lowest resolution layer */
   if (d >= 0)
    jbg_enc_layers(&s, d);
  else
    jbg_enc_lrlmax(&s, mwidth, mheight);

  /* Specify a few other options (each is ignored if negative) */
  if (delay_at)
    options |= JBG_DELAY_AT;
  jbg_enc_lrange(&s, dl, dh);
  jbg_enc_options(&s, order, options, l0, mx, -1);

  /* now encode everything and send it to data_out() */
  jbg_enc_out(&s);

  /* give encoder a chance to free its temporary data structures */
  jbg_enc_free(&s);

  /* check for file errors and close fout */
  if (ferror(fout) || fclose(fout)) {
    fprintf(stderr, "Problem while writing output file '%s", fnout);
    perror("'");
    exit(1);
  }

  /* In case the user wants to know all the gory details ... */
  if (verbose) {
    fprintf(stderr, "Information about the created JBIG bi-level image entity "
	    "(BIE):\n\n");
    fprintf(stderr, "              input image size: %ld x %ld pixel\n",
	    s.xd, s.yd);
    fprintf(stderr, "                    bit planes: %d\n", s.planes);
    fprintf(stderr, "                       stripes: %ld\n", s.stripes);
    fprintf(stderr, "   lines per stripe in layer 0: %ld\n", s.l0);
    fprintf(stderr, "  total number of diff. layers: %d\n", s.d);
    fprintf(stderr, "           lowest layer in BIE: %d\n", s.dl);
    fprintf(stderr, "          highest layer in BIE: %d\n", s.dh);
    fprintf(stderr, "             lowest layer size: %lu x %lu pixel\n",
	    ceil_half(s.xd, s.d - s.dl), ceil_half(s.yd, s.d - s.dl));
    fprintf(stderr, "            highest layer size: %lu x %lu pixel\n",
	    ceil_half(s.xd, s.d - s.dh), ceil_half(s.yd, s.d - s.dh));
    fprintf(stderr, "                   option bits:%s%s%s%s%s%s%s\n",
	    s.options & JBG_LRLTWO  ? " LRLTWO" : "",
	    s.options & JBG_VLENGTH ? " VLENGTH" : "",
	    s.options & JBG_TPDON   ? " TPDON" : "",
	    s.options & JBG_TPBON   ? " TPBON" : "",
	    s.options & JBG_DPON    ? " DPON" : "",
	    s.options & JBG_DPPRIV  ? " DPPRIV" : "",
	    s.options & JBG_DPLAST  ? " DPLAST" : "");
    fprintf(stderr, "                    order bits:%s%s%s%s\n",
	    s.order & JBG_HITOLO ? " HITOLO" : "",
	    s.order & JBG_SEQ    ? " SEQ" : "",
	    s.order & JBG_ILEAVE ? " ILEAVE" : "",
	    s.order & JBG_SMID   ? " SMID" : "");
    fprintf(stderr, "           AT maximum x-offset: %d\n"
	    "           AT maximum y-offset: %d\n", s.mx, s.my);
    fprintf(stderr, "         length of output file: %lu byte\n\n",
	    total_length);
  }
  return 0;
}
