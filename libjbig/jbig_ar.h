/*
 *  Header file for the arithmetic encoder and decoder of
 *  the portable JBIG compression library
 *
 *  Markus Kuhn -- http://www.cl.cam.ac.uk/~mgk25/jbigkit/
 *
 *  $Id$
 */

#ifndef JBG_AR_H
#define JBG_AR_H

/*
 * Status description of an arithmetic encoder
 */

struct jbg_arenc_state {
  unsigned char st[4096];    /* probability status for contexts, MSB = MPS */
  unsigned long c;                /* C register, base of coding intervall, *
                                   * layout as in Table 23                 */
  unsigned long a;      /* A register, normalized size of coding intervall */
  long sc;        /* counter for buffered 0xff values which might overflow */
  int ct;  /* bit shift counter, determines when next byte will be written */
  int buffer;                /* buffer for most recent output byte != 0xff */
  void (*byte_out)(int, void *); /* function which receives all PSCD bytes */
  void *file;                              /* parameter passed to byte_out */
};


/*
 * Status description of an arithmetic decoder
 */

enum jbg_ardec_result {
  JBG_OK,                          /* symbol has been successfully decoded */
  JBG_READY,               /* no more bytes of this PSCD required, marker  *
			    * encountered, probably more symbols available */
  JBG_MORE,            /* more PSCD data bytes required to decode a symbol */
  JBG_MARKER     /* more PSCD data bytes required, ignored final 0xff byte */
};

struct jbg_ardec_state {
  unsigned char st[4096];    /* probability status for contexts, MSB = MPS */
  unsigned long c;                /* C register, base of coding intervall, *
                                   * layout as in Table 25                 */
  unsigned long a;      /* A register, normalized size of coding intervall */
  int ct;     /* bit shift counter, determines when next byte will be read */
  unsigned char *pscd_ptr;               /* pointer to next PSCD data byte */
  unsigned char *pscd_end;                   /* pointer to byte after PSCD */
  enum jbg_ardec_result result;          /* result of previous decode call */
  int startup;                            /* controls initial fill of s->c */
};

void arith_encode_init(struct jbg_arenc_state *s, int reuse_st);
void arith_encode_flush(struct jbg_arenc_state *s);
void arith_encode(struct jbg_arenc_state *s, int cx, int pix);
void arith_decode_init(struct jbg_ardec_state *s, int reuse_st);
int  arith_decode(struct jbg_ardec_state *s, int cx);

#endif /* JBG_AR_H */
