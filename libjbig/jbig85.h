/*
 *  Header file for the T.85 "light" version of the portable
 *  JBIG image compression library
 *
 *  Copyright 1995-2007 -- Markus Kuhn -- http://www.cl.cam.ac.uk/~mgk25/
 *
 *  $Id$
 */

#ifndef JBG85_H
#define JBG85_H

#include <stddef.h>
#include "jbig_ar.h"

/*
 * JBIG-KIT version number
 */

#define JBG85_VERSION    "1.7"

/*
 * Maximum number of ATMOVEs per stripe that decoder can handle
 */

#define JBG_ATMOVES_MAX  64

/*
 * Option and order flags
 */

#define JBG_HITOLO     0x08
#define JBG_SEQ        0x04
#define JBG_ILEAVE     0x02
#define JBG_SMID       0x01

#define JBG_LRLTWO     0x40
#define JBG_VLENGTH    0x20
#define JBG_TPDON      0x10
#define JBG_TPBON      0x08
#define JBG_DPON       0x04
#define JBG_DPPRIV     0x02
#define JBG_DPLAST     0x01

/* encoding options that will not be indicated in the header */

#define JBG_DELAY_AT   0x100  /* Delay ATMOVE until the first line of the next
			       * stripe. Option available for compatibility
			       * with conformance test example in clause 7.2. */

#define JBG_SDRST      0x200  /* Use SDRST instead of SDNORM. This option is
			       * there for anyone who needs to generate
			       * test data that covers the SDRST cases. */

/*
 * Possible error code return values
 */

#define JBG_EOK        0
#define JBG_EOK_INTR   1
#define JBG_EAGAIN     2
#define JBG_ENOMEM     3
#define JBG_EABORT     4
#define JBG_EMARKER    5
#define JBG_ENOCONT    6
#define JBG_EINVAL     7
#define JBG_EIMPL      8

/*
 * Status of a JBIG encoder
 */

struct jbg85_enc_state {
  unsigned long x0, y0;                         /* size of the input image */
  unsigned long l0;                          /* number of lines per stripe */
  unsigned char *pline[2];                  /* point to previous two lines */
  int options;                                      /* encoding parameters */
  int newlen;     /* 0 = jbg85_enc_newlen() has not yet been called
                     1 = jbg85_enc_newlen() has updated y0, NEWLEN pending
                     2 = NEWLEN has already been output                    */
  unsigned mx;                               /* maximum ATMOVE window size */
  unsigned long y;                       /* next line number to be encoded */
  unsigned long i;            /* next per-stripe line number to be encoded */
  int tx;                           /* x-offset of adaptive template pixel */
  int c_all, c[128];              /* adaptive template algorithm variables */
  int new_tx;            /* -1 = no ATMOVE pending, otherwise new TX value */
  int ltp_old;                           /* true if line y-1 was "typical" */
  struct jbg_arenc_state s;                   /* arithmetic encoder status */
  void (*data_out)(unsigned char *start, size_t len, void *file);
                                                    /* data write callback */
  void *file;                            /* parameter passed to data_out() */
  unsigned char *comment; /* content of comment marker segment to be added
                             at next opportunity (will be reset to NULL
                             as soon as comment has been written)          */
  unsigned long comment_len;       /* length of data pointed to by comment */
};


/*
 * Status of a JBIG decoder
 */

struct jbg85_dec_state {
  /* data from BIH */
  unsigned long x0, y0;                          /* size of the full image */
  unsigned long l0;                          /* number of lines per stripe */
  int options;                                      /* encoding parameters */
  int mx;                                    /* maximum ATMOVE window size */
  /* image data */
  int p[3];    /* curr. line starts at linebuf+bpl*p[0], prev. line starts
                * at linebuf+bpl*p[1], its predecessor at linebuf+bpl*p[2] */
  unsigned char *linebuf;              /* buffer region provided by caller */
  size_t linebuf_len;
  size_t bpl;                                            /* bytes per line */
  /* status information */
  int tx;                                         /*  x-offset of AT pixel */
  struct jbg_ardec_state s;                   /* arithmetic decoder status */
  int reset;                   /* true if previous stripe ended with SDRST */
  unsigned long bie_len;                    /* number of bytes read so far */
  unsigned char buffer[20]; /* used to store BIH or marker segments fragm. */
  int buf_len;                                /* number of bytes in buffer */
  unsigned long comment_skip;      /* remaining bytes of a COMMENT segment */
  unsigned long x;                             /* x position of next pixel */
  unsigned long stripe;                                  /* current stripe */
  unsigned long y;                      /* line in image (first line is 0) */ 
  unsigned long i;   /* line in current stripe (first line of stripe is 0) */ 
  int at_moves;                /* number of AT moves in the current stripe */
  unsigned long at_line[JBG_ATMOVES_MAX];           /* lines at which an   *
					             * AT move will happen */
  int at_tx[JBG_ATMOVES_MAX];        /* ATMOVE x-offsets in current stripe */
  unsigned long line_h1, line_h2, line_h3;     /* variables of decode_pscd */
  int pseudo;         /* flag for TPBON/TPDON:  next pixel is pseudo pixel */
  int lntp;                            /* flag for TP: line is not typical */
  unsigned long ymax;  /* if possible abort before image grows beyond this */
  void (*line_out)(unsigned char *start, size_t len,
		   unsigned long y, void *file);
                                                    /* data write callback */
  void *file;                            /* parameter passed to data_out() */
};


/* function prototypes */

void jbg85_enc_init(struct jbg85_enc_state *s,
		    unsigned long x0, unsigned long y0,
		    void (*data_out)(unsigned char *start, size_t len,
				     void *file),
		    void *file);
void jbg85_enc_options(struct jbg85_enc_state *s, int options,
		       unsigned long l0, int mx);
void jbg85_enc_lineout(struct jbg85_enc_state *s, unsigned char *line);
void jbg85_enc_newlen(struct jbg85_enc_state *s, unsigned long y0);

void jbg85_dec_init(struct jbg85_dec_state *s,
		    unsigned char *buf, size_t buflen,
		    void (*line_out)(unsigned char *start, size_t len,
				     unsigned long y, void *file),
		    void *file);
void jbg85_dec_maxlen(struct jbg85_dec_state *s, unsigned long ymax);
int  jbg85_dec_in(struct jbg85_dec_state *s, unsigned char *data, size_t len,
		  size_t *cnt);
long jbg85_dec_getwidth(const struct jbg85_dec_state *s);
long jbg85_dec_getheight(const struct jbg85_dec_state *s);

const char *jbg85_strerror(int errnum);

#endif /* JBG85_H */
