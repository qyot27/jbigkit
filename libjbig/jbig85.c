/*
 *  T.85 "light" version of the portable JBIG image compression library
 *
 *  Copyright 1995-2007 -- Markus Kuhn -- http://www.cl.cam.ac.uk/~mgk25/
 *
 *  $Id$
 *
 *  This module implements a portable standard C encoder and decoder
 *  using the JBIG1 lossless bi-level image compression algorithm
 *  specified in International Standard ISO 11544:1993 and
 *  ITU-T Recommendation T.82. See the file jbig.txt for usage
 *  instructions and application examples.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 *  If you want to use this program under different license conditions,
 *  then contact the author for an arrangement.
 *
 *  It is possible that certain products which can be built using this
 *  software module might form inventions protected by patent rights in
 *  some countries (e.g., by patents about arithmetic coding algorithms
 *  owned by IBM and AT&T in the USA). Provision of this software by the
 *  author does NOT include any licences for any patents. In those
 *  countries where a patent licence is required for certain applications
 *  of this software module, you will have to obtain such a licence
 *  yourself.
 */

#ifdef DEBUG
#include <stdio.h>
#else
#define NDEBUG
#endif

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "jbig85.h"

#define TPB2CX  0x195  /* contexts for TP special pixels */
#define TPB3CX  0x0e5
#define TPDCX   0xc3f

/* marker codes */
#define MARKER_STUFF    0x00
#define MARKER_RESERVE  0x01
#define MARKER_SDNORM   0x02
#define MARKER_SDRST    0x03
#define MARKER_ABORT    0x04
#define MARKER_NEWLEN   0x05
#define MARKER_ATMOVE   0x06
#define MARKER_COMMENT  0x07
#define MARKER_ESC      0xff

/* object code version id */

const char jbg85_version[] = 
" JBIG-KIT " JBG85_VERSION " (T.85 version) -- Markus Kuhn -- "
"$Id$ ";

#define _(String) String  /* to mark translatable string for GNU gettext */

/*
 * Array with English ASCII error messages that correspond
 * to return values from public functions in this library.
 */
static const char *errmsg[] = {
  _("Everything is OK"),                                     /* JBG_EOK */
  _("Reached specified maximum image size"),                 /* JBG_EOK_INTR */
  _("Unexpected end of input data stream"),                  /* JBG_EAGAIN */
  _("Not enough memory available"),                          /* JBG_ENOMEM */
  _("ABORT marker segment encountered"),                     /* JBG_EABORT */
  _("Unknown marker segment encountered"),                   /* JBG_EMARKER */
  _("Incremental BIE does not continue previous one"),       /* JBG_ENOCONT */
  _("Input data stream contains invalid data"),              /* JBG_EINVAL */
  _("Input data stream uses unimplemented JBIG features")    /* JBG_EIMPL */
};


/*
 * Callback adapter function for arithmetic encoder
 */
static void jbg85_enc_byte_out(int byte, void *s)
{
  unsigned char c = byte;
  ((struct jbg85_enc_state *)s)->data_out(&c, sizeof(unsigned char),
					  ((struct jbg85_enc_state *)s)->file);
}

/*
 * Initialize the status struct for the encoder.
 */
void jbg85_enc_init(struct jbg85_enc_state *s,
		    unsigned long x0, unsigned long y0,
		    void (*data_out)(unsigned char *start, size_t len,
				     void *file),
		    void *file)
{
  assert(x0 > 0 && y0 > 0);
  s->x0 = x0;
  s->y0 = y0;
  s->newlen = 0;       /* no NEWLEN pending or output */
  s->data_out = data_out;
  s->file = file;

  s->l0 = s->y0 / 35;             /* 35 stripes/image suggested default */
  if (s->l0 > 128) s->l0 = 128;
  if (s->l0 < 2) s->l0 = 2;
#if 1
  s->l0 = 128; /* T.85 BASIC setting */
#endif
  s->mx = 127;
  s->new_tx = -1;                /* no ATMOVE pending */
  s->tx = 0;
  s->options = JBG_TPBON | JBG_VLENGTH;
  s->comment = NULL;            /* no COMMENT pending */
  s->pline[0] = s->pline[1] = NULL;
  s->y = 0;
  s->i = 0;
  s->ltp_old = 1;
  
  /* initialize arithmetic encoder */
  arith_encode_init(&s->s, 0);
  s->s.byte_out = &jbg85_enc_byte_out;
  s->s.file = s;
  
  return;
}


/*
 * The following function allows to specify the bits describing the
 * options of the format as well as the maximum AT movement window and
 * the number of layer 0 lines per stripes.
 */
void jbg85_enc_options(struct jbg85_enc_state *s, int options,
		     unsigned long l0, int mx)
{
  if (options >= 0) s->options = options;
  if (l0 > 0) s->l0 = l0;
  if (mx >= 0 && mx < 128) s->mx = mx;

  return;
}


/*
 * Encode one full BIE and pass the generated data to the specified
 * call-back function
 */
void jbg85_enc_lineout(struct jbg85_enc_state *s, unsigned char *line)
{
  unsigned char buf[20];
  unsigned long bpl;
  unsigned char *hp1, *hp2, *hp3, *p1, *q1;
  unsigned long line_h1 = 0, line_h2, line_h3;
  unsigned long j;  /* loop variable for pixel column */
  long o;
  unsigned a, p, t;
  int ltp;
  unsigned long cmin, cmax, clmin, clmax;
  int tmax;

  if (s->y >= s->y0) {
    /* we have already output the full image, go away */
    return;
  }

  /* things that need to be done before the first line is encoded */
  if (s->y == 0) {
    /* prepare BIH */
    buf[0]  = 0;   /* DL = initial layer to be transmitted */
    buf[1]  = 0;   /* D  = number of differential layers */
    buf[2]  = 1;   /* P  = number of bit planes */
    buf[3]  = 0;
    buf[4]  =  s->x0 >> 24;
    buf[5]  = (s->x0 >> 16) & 0xff;
    buf[6]  = (s->x0 >>  8) & 0xff;
    buf[7]  =  s->x0        & 0xff;
    buf[8]  =  s->y0 >> 24;
    buf[9]  = (s->y0 >> 16) & 0xff;
    buf[10] = (s->y0 >>  8) & 0xff;
    buf[11] =  s->y0 & 0xff;
    buf[12] =  s->l0 >> 24;
    buf[13] = (s->l0 >> 16) & 0xff;
    buf[14] = (s->l0 >>  8) & 0xff;
    buf[15] =  s->l0 & 0xff;
    buf[16] = s->mx;
    buf[17] = 0;   /* MY = maximum horizontal offset allowed for AT pixel */
    buf[18] = 0;   /* order: HITOLO = SEQ = ILEAVE = SMID = 0 */
    buf[19] = s->options & (JBG_LRLTWO | JBG_VLENGTH | JBG_TPBON);

    /* output BIH */
    s->data_out(buf, 20, s->file);
  }

  /* things that need to be done before the next SDE is encoded */
  if (s->i == 0) {

    /* output NEWLEN if there is any pending */
    if (s->newlen == 1) {
      buf[0] = MARKER_ESC;
      buf[1] = MARKER_NEWLEN;
      buf[2] =  s->y0 >> 24;
      buf[3] = (s->y0 >> 16) & 0xff;
      buf[4] = (s->y0 >>  8) & 0xff;
      buf[5] =  s->y0        & 0xff;
      s->data_out(buf, 6, s->file);
      s->newlen = 2;
    }

    /* output comment marker segment if there is any pending */
    if (s->comment) {
      buf[0] = MARKER_ESC;
      buf[1] = MARKER_COMMENT;
      buf[2] =  s->comment_len >> 24;
      buf[3] = (s->comment_len >> 16) & 0xff;
      buf[4] = (s->comment_len >>  8) & 0xff;
      buf[5] =  s->comment_len & 0xff;
      s->data_out(buf, 6, s->file);
      s->data_out(s->comment, s->comment_len, s->file);
      s->comment = NULL;
    }

    /* output ATMOVE if there is any pending */
    if (s->new_tx != -1 && s->new_tx != s->tx) {
      s->tx = s->new_tx;
      buf[0] = MARKER_ESC;
      buf[1] = MARKER_ATMOVE;
      buf[2] = 0;
      buf[3] = 0;
      buf[4] = 0;
      buf[5] = 0;
      buf[6] = s->tx;
      buf[7] = 0;
      s->data_out(buf, 8, s->file);
    }
    
    /* initialize adaptive template movement algorithm */
    if (s->mx == 0) {
      s->new_tx = 0;  /* ATMOVE has been disabled */
    } else {
      s->c_all = 0;
      for (t = 0; t <= s->mx; t++)
	s->c[t] = 0;
      s->new_tx = -1; /* we have yet to determine ATMOVE ... */
    }

    /* restart arithmetic encoder */
    arith_encode_init(&s->s, 1);
  }

#ifdef DEBUG
  static long tp_lines, tp_exceptions, tp_pixels, dp_pixels;
  static long encoded_pixels;
  if (s->y == 0)
    tp_lines = tp_exceptions = tp_pixels = dp_pixels = encoded_pixels = 0;
  fprintf(stderr, "encode line %ld (%2d of stripe)\n", s->y, s->i);
#endif

  /* bytes per line */
  bpl = (s->x0 + 7) >> 3;
  /* ensure correct zero padding of bitmap at the final byte of each line */
  if (s->x0 & 7) {
    line[bpl - 1] &= ~((1 << (8 - (s->x0 & 7))) - 1);
  }

  /* typical prediction */
  ltp = 0;
  if (s->options & JBG_TPBON) {
    p1 = line;
    q1 = s->pline[0];
    ltp = 1;
    if (q1)
      while (p1 < line + bpl && (ltp = (*p1++ == *q1++)) != 0);
    else
      while (p1 < line + bpl && (ltp = (*p1++ == 0    )) != 0);
    arith_encode(&s->s, (s->options & JBG_LRLTWO) ? TPB2CX : TPB3CX,
		 ltp == s->ltp_old);
#ifdef DEBUG
    tp_lines += ltp;
#endif
    s->ltp_old = ltp;
  }
  
  if (!ltp) {

    /*
     * Layout of the variables line_h1, line_h2, line_h3, which contain
     * as bits the neighbour pixels of the currently coded pixel X:
     *
     *          76543210765432107654321076543210     line_h3
     *          76543210765432107654321076543210     line_h2
     *  76543210765432107654321X76543210             line_h1
     */
  
    /* pointer to first image byte of the three lines of interest */
    hp3 = s->pline[1];
    hp2 = s->pline[0];
    hp1 = line;
  
    line_h1 = line_h2 = line_h3 = 0;
    if (hp2) line_h2 = (long)*hp2 << 8;
    if (hp3) line_h3 = (long)*hp3 << 8;
  
    /* encode line */
    for (j = 0; j < s->x0;) {
      line_h1 |= *hp1;
      if (j < bpl * 8 - 8 && hp2) {
	line_h2 |= *(hp2 + 1);
	if (hp3)
	  line_h3 |= *(hp3 + 1);
      }
      if (s->options & JBG_LRLTWO) {
	/* two line template */
	do {
	  line_h1 <<= 1;  line_h2 <<= 1;  line_h3 <<= 1;
	  if (s->tx) {
	    if ((unsigned) s->tx > j)
	      a = 0;
	    else {
	      o = (j - s->tx) - (j & ~7L);
	      a = (hp1[o >> 3] >> (7 - (o & 7))) & 1;
	      a <<= 4;
	    }
	    assert(s->tx > 23 ||
		   a == ((line_h1 >> (4 + s->tx)) & 0x010));
	    arith_encode(&s->s, (((line_h2 >> 10) & 0x3e0) | a |
				 ((line_h1 >>  9) & 0x00f)),
			 (line_h1 >> 8) & 1);
	  }
	  else
	    arith_encode(&s->s, (((line_h2 >> 10) & 0x3f0) |
				 ((line_h1 >>  9) & 0x00f)),
			 (line_h1 >> 8) & 1);
#ifdef DEBUG
	  encoded_pixels++;
#endif
	  /* statistics for adaptive template changes */
	  if (s->new_tx == -1 && j >= s->mx && j < s->x0 - 2) {
	    p = (line_h1 & 0x100) != 0; /* current pixel value */
	    s->c[0] += ((line_h2 & 0x4000) != 0) == p; /* default position */
	    assert(!(((line_h2 >> 6) ^ line_h1) & 0x100) ==
		   (((line_h2 & 0x4000) != 0) == p));
	    for (t = 5; t <= s->mx && t <= j; t++) {
	      o = (j - t) - (j & ~7L);
	      a = (hp1[o >> 3] >> (7 - (o & 7))) & 1;
	      assert(t > 23 ||
		     (a == p) == !(((line_h1 >> t) ^ line_h1) & 0x100));
	      s->c[t] += a == p;
	    }
	    for (; t <= s->mx; t++) {
	      s->c[t] += 0 == p;
	    }
	    ++s->c_all;
	  }
	} while (++j & 7 && j < s->x0);
      } else {
	/* three line template */
	do {
	  line_h1 <<= 1;  line_h2 <<= 1;  line_h3 <<= 1;
	  if (s->tx) {
	    if ((unsigned) s->tx > j)
	      a = 0;
	    else {
	      o = (j - s->tx) - (j & ~7L);
	      a = (hp1[o >> 3] >> (7 - (o & 7))) & 1;
	      a <<= 2;
	    }
	    assert(s->tx > 23 ||
		   a == ((line_h1 >> (6 + s->tx)) & 0x004));
	    arith_encode(&s->s, (((line_h3 >>  8) & 0x380) |
				 ((line_h2 >> 12) & 0x078) | a |
				 ((line_h1 >>  9) & 0x003)),
			 (line_h1 >> 8) & 1);
	  } else
	    arith_encode(&s->s, (((line_h3 >>  8) & 0x380) |
				 ((line_h2 >> 12) & 0x07c) |
				 ((line_h1 >>  9) & 0x003)),
			 (line_h1 >> 8) & 1);
#ifdef DEBUG
	  encoded_pixels++;
#endif
	  /* statistics for adaptive template changes */
	  if (s->new_tx == -1 && j >= s->mx && j < s->x0 - 2) {
	    p = (line_h1 & 0x100) != 0; /* current pixel value */
	    s->c[0] += ((line_h2 & 0x4000) != 0) == p; /* default position */
	    assert(!(((line_h2 >> 6) ^ line_h1) & 0x100) ==
		   (((line_h2 & 0x4000) != 0) == p));
	    for (t = 3; t <= s->mx && t <= j; t++) {
	      o = (j - t) - (j & ~7L);
	      a = (hp1[o >> 3] >> (7 - (o & 7))) & 1;
	      assert(t > 23 ||
		     (a == p) == !(((line_h1 >> t) ^ line_h1) & 0x100));
	      s->c[t] += a == p;
	    }
	    for (; t <= s->mx; t++) {
	      s->c[t] += 0 == p;
	    }
	    ++s->c_all;
	  }
	} while (++j & 7 && j < s->x0);
      } /* if (s->options & JBG_LRLTWO) */
      hp1++;
      if (hp2) hp2++;
      if (hp3) hp3++;
    } /* for (j = ...) */
  } /* if (!ltp) */

  /* line is complete now, deal with end of stripe */
  s->i++; s->y++;
  s->pline[1] = s->pline[0];
  s->pline[0] = line;
  if (s->i == s->l0 || s->y == s->y0) {
    /* end of stripe reached */
    arith_encode_flush(&s->s);
    buf[0] = MARKER_ESC;
    buf[1] = MARKER_SDNORM;
    s->data_out(buf, 2, s->file);
    s->i = 0;
  }

  /* check whether it is worth to perform an ATMOVE */
  if (s->new_tx == -1 && s->c_all > 2048) {
    cmin = clmin = 0xffffffffL;
    cmax = clmax = 0;
    tmax = 0;
    for (t = (s->options & JBG_LRLTWO) ? 5 : 3; t <= s->mx; t++) {
      if (s->c[t] > cmax) cmax = s->c[t];
      if (s->c[t] < cmin) cmin = s->c[t];
      if (s->c[t] > s->c[tmax]) tmax = t;
    }
    clmin = (s->c[0] < cmin) ? s->c[0] : cmin;
    clmax = (s->c[0] > cmax) ? s->c[0] : cmax;
    if (s->c_all - cmax < (s->c_all >> 3) &&
	cmax - s->c[s->tx] > s->c_all - cmax &&
	cmax - s->c[s->tx] > (s->c_all >> 4) &&
	/*                 ^ T.82 said < here, fixed in Cor.1/25 */
	cmax - (s->c_all - s->c[s->tx]) > s->c_all - cmax &&
	cmax - (s->c_all - s->c[s->tx]) > (s->c_all >> 4) &&
	cmax - cmin > (s->c_all >> 2) &&
	(s->tx || clmax - clmin > (s->c_all >> 3))) {
      /* we have decided to perform an ATMOVE */
      s->new_tx = tmax;
#ifdef DEBUG
      fprintf(stderr, "ATMOVE: tx=%d, c_all=%d\n",
	      s->new_tx, s->c_all);
#endif
    } else {
      s->new_tx = s->tx;  /* we have decided not to perform an ATMOVE */
    }
  }
  assert(s->tx >= 0); /* i.e., tx can safely be cast to unsigned */
  
#if 0
  if (s->y == s->y0)
    fprintf(stderr, "tp_lines = %ld, tp_exceptions = %ld, tp_pixels = %ld, "
	    "dp_pixels = %ld, encoded_pixels = %ld\n",
	    tp_lines, tp_exceptions, tp_pixels, dp_pixels, encoded_pixels);
#endif

  return;
}

/*
 * Inform encoder about new (reduced) height of image
 */
void jbg85_enc_newlen(struct jbg85_enc_state *s, unsigned long newlen)
{
  unsigned char buf[6];

  if (s->newlen == 2 || newlen >= s->y0 || newlen < 1 ||
      !(s->options & JBG_VLENGTH)) {
    /* invalid invocation or parameter */
    return;
  }
  if (newlen < s->y) {
    /* we are already beyond the new end, therefore move the new end */
    newlen = s->y;
  }
  s->y0 = newlen;
  s->newlen = 1;
  if (s->y == s->y0) {
    /* we are already at the end; abort the current stripe if necessary */
    if (s->i > 0) {
      arith_encode_flush(&s->s);
      buf[0] = MARKER_ESC;
      buf[1] = MARKER_SDNORM;
      s->data_out(buf, 2, s->file);
      s->i = 0;
    }
    buf[0] = MARKER_ESC;
    buf[1] = MARKER_NEWLEN;
    buf[2] =  s->y0 >> 24;
    buf[3] = (s->y0 >> 16) & 0xff;
    buf[4] = (s->y0 >>  8) & 0xff;
    buf[5] =  s->y0        & 0xff;
    s->data_out(buf, 6, s->file);
    s->newlen = 2;
    /* if newlen refers to a line in the preceeding stripe, ITU-T T.82
     * section 6.2.6.2 requires us to append another SDNORM (no idea why!) */
    buf[1] = MARKER_SDNORM;
    s->data_out(buf, 2, s->file);
  }
}

/*
 * Convert the error codes used by jbg85_dec_in() into an English ASCII string
 */
const char *jbg85_strerror(int errnum)
{
  if (errnum < 0 || (unsigned) errnum >= sizeof(errmsg)/sizeof(errmsg[0]))
    return "Unknown error code passed to jbg85_strerror()";

  return errmsg[errnum];
}

#ifdef TODO

/*
 * The constructor for a decoder 
 */
void jbg85_dec_init(struct jbg85_dec_state *s)
{
  s->order = 0;
  s->d = -1;
  s->bie_len = 0;
  s->buf_len = 0;
  s->dppriv = NULL;
  s->xmax = 4294967295UL;
  s->ymax = 4294967295UL;
  s->dmax = 256;
  s->s = NULL;

  return;
}


/*
 * Specify a maximum image size for the decoder. If the JBIG file has
 * the order bit ILEAVE, but not the bit SEQ set, then the decoder
 * will abort to decode after the image has reached the maximal
 * resolution layer which is still not wider than xmax or higher than
 * ymax.
 */
void jbg_dec_maxsize(struct jbg_dec_state *s, unsigned long xmax,
		     unsigned long ymax)
{
  if (xmax > 0) s->xmax = xmax;
  if (ymax > 0) s->ymax = ymax;

  return;
}


/*
 * Decode the new len PSDC bytes to which data points and add them to
 * the current stripe. Return the number of bytes which have actually
 * been read (this will be less than len if a marker segment was 
 * part of the data or if the final byte was 0xff were this code
 * can not determine, whether we have a marker segment.
 */
static size_t decode_pscd(struct jbg_dec_state *s, unsigned char *data,
			  size_t len)
{
  unsigned long stripe;
  unsigned int layer, plane;
  unsigned long hl, ll, y, hx, hy, lx, ly, hbpl, lbpl;
  unsigned char *hp, *lp1, *lp2, *p1, *q1;
  register unsigned long line_h1, line_h2, line_h3;
  register unsigned long line_l1, line_l2, line_l3;
  struct jbg_ardec_state *se;
  unsigned long x;
  long o;
  unsigned a;
  int n;
  int pix, cx = 0, slntp, tx;

  /* SDE loop variables */
  stripe = s->ii[iindex[s->order & 7][STRIPE]];
  layer = s->ii[iindex[s->order & 7][LAYER]];
  plane = s->ii[iindex[s->order & 7][PLANE]];

  /* forward data to arithmetic decoder */
  se = s->s[plane] + layer - s->dl;
  se->pscd_ptr = data;
  se->pscd_end = data + len;
  
  /* number of lines per stripe in highres image */
  hl = s->l0 << layer;
  /* number of lines per stripe in lowres image */
  ll = hl >> 1;
  /* current line number in highres image */
  y = stripe * hl + s->i;
  /* number of pixels in highres image */
  hx = jbg_ceil_half(s->xd, s->d - layer);
  hy = jbg_ceil_half(s->yd, s->d - layer);
  /* number of pixels in lowres image */
  lx = jbg_ceil_half(hx, 1);
  ly = jbg_ceil_half(hy, 1);
  /* bytes per line in highres and lowres image */
  hbpl = jbg_ceil_half(hx, 3);
  lbpl = jbg_ceil_half(lx, 3);
  /* pointer to highres and lowres image bytes */
  hp  = s->lhp[ layer    & 1][plane] + (stripe * hl + s->i) * hbpl +
    (s->x >> 3);
  lp2 = s->lhp[(layer-1) & 1][plane] + (stripe * ll + (s->i >> 1)) * lbpl +
    (s->x >> 4);
  lp1 = lp2 + lbpl;

  /* restore a few local variables */
  line_h1 = s->line_h1;
  line_h2 = s->line_h2;
  line_h3 = s->line_h3;
  line_l1 = s->line_l1;
  line_l2 = s->line_l2;
  line_l3 = s->line_l3;
  x = s->x;

#ifdef DEBUG
  if (s->x == 0 && s->i == 0 && s->pseudo)
    fprintf(stderr, "decode_pscd(%p, %p, %ld): s/d/p = %2lu/%2u/%2u\n",
	    (void *) s, (void *) data, (long) len, stripe, layer, plane);
#endif

  if (s->x == 0 && s->i == 0 &&
      (stripe == 0 || s->reset[plane][layer - s->dl]) && s->pseudo) {
    s->tx[plane][layer - s->dl] = s->ty[plane][layer - s->dl] = 0;
    s->lntp[plane][layer - s->dl] = 1;
  }

  if (layer == 0) {

    /*
     *  Decode lowest resolution layer
     */

    for (; s->i < hl && y < hy; s->i++, y++) {

      /* adaptive template changes */
      if (x == 0 && s->pseudo)
	for (n = 0; n < s->at_moves; n++)
	  if (s->at_line[n] == s->i) {
	    s->tx[plane][layer - s->dl] = s->at_tx[n];
	    s->ty[plane][layer - s->dl] = s->at_ty[n];
#ifdef DEBUG
	    fprintf(stderr, "ATMOVE: line=%lu, tx=%d, ty=%d.\n", s->i,
		    s->tx[plane][layer - s->dl], s->ty[plane][layer - s->dl]);
#endif
	  }
      tx = s->tx[plane][layer - s->dl];
      assert(tx >= 0); /* i.e., tx can safely be cast to unsigned */

      /* typical prediction */
      if (s->options & JBG_TPBON && s->pseudo) {
	slntp = arith_decode(se, (s->options & JBG_LRLTWO) ? TPB2CX : TPB3CX);
	if (se->result == JBG_MORE || se->result == JBG_MARKER)
	  goto leave;
	s->lntp[plane][layer - s->dl] =
	  !(slntp ^ s->lntp[plane][layer - s->dl]);
	if (!s->lntp[plane][layer - s->dl]) {
	  /* this line is 'typical' (i.e. identical to the previous one) */
	  p1 = hp;
	  if (s->i == 0 && (stripe == 0 || s->reset[plane][layer - s->dl]))
	    while (p1 < hp + hbpl) *p1++ = 0;
	  else {
	    q1 = hp - hbpl;
	    while (q1 < hp) *p1++ = *q1++;
	  }
	  hp += hbpl;
	  continue;
	}
	/* this line is 'not typical' and has to be coded completely */
      }
      s->pseudo = 0;
      
      /*
       * Layout of the variables line_h1, line_h2, line_h3, which contain
       * as bits the neighbour pixels of the currently decoded pixel X:
       *
       *                     76543210 76543210 76543210 76543210     line_h3
       *                     76543210 76543210 76543210 76543210     line_h2
       *   76543210 76543210 76543210 76543210 X                     line_h1
       */
      
      if (x == 0) {
	line_h1 = line_h2 = line_h3 = 0;
	if (s->i > 0 || (y > 0 && !s->reset[plane][layer - s->dl]))
	  line_h2 = (long)*(hp - hbpl) << 8;
	if (s->i > 1 || (y > 1 && !s->reset[plane][layer - s->dl]))
	  line_h3 = (long)*(hp - hbpl - hbpl) << 8;
      }
      
      /*
       * Another tiny JBIG standard bug:
       *
       * While implementing the line_h3 handling here, I discovered
       * another problem with the ITU-T T.82(1993 E) specification.
       * This might be a somewhat pathological case, however. The
       * standard is unclear about how a decoder should behave in the
       * following situation:
       *
       * Assume we are in layer 0 and all stripes are single lines
       * (L0=1 allowed by table 9). We are now decoding the first (and
       * only) line of the third stripe. Assume, the first stripe was
       * terminated by SDRST and the second stripe was terminated by
       * SDNORM. While decoding the only line of the third stripe with
       * the three-line template, we need access to pixels from the
       * previous two stripes. We know that the previous stripe
       * terminated with SDNROM, so we access the pixel from the
       * second stripe. But do we have to replace the pixels from the
       * first stripe by background pixels, because this stripe ended
       * with SDRST? The standard, especially clause 6.2.5 does never
       * mention this case, so the behaviour is undefined here. My
       * current implementation remembers only the marker used to
       * terminate the previous stripe. In the above example, the
       * pixels of the first stripe are accessed despite the fact that
       * this stripe ended with SDRST. An alternative (only slightly
       * more complicated) implementation would be to remember the end
       * marker (SDNORM or SDRST) of the previous two stripes in a
       * plane/layer and to act accordingly when accessing the two
       * previous lines. What am I supposed to do here?
       *
       * As the standard is unclear about the correct behaviour in the
       * situation of the above example, I strongly suggest to avoid
       * the following situation while encoding data with JBIG:
       *
       *   LRLTWO = 0, L0=1 and both SDNORM and SDRST appear in layer 0.
       *
       * I guess that only a very few if any encoders will switch
       * between SDNORM and SDRST, so let us hope that this ambiguity
       * in the standard will never cause any interoperability
       * problems.
       *
       * Markus Kuhn -- 1995-04-30
       */

      /* decode line */
      while (x < hx) {
	if ((x & 7) == 0) {
	  if (x < hbpl * 8 - 8 &&
	      (s->i > 0 || (y > 0 && !s->reset[plane][layer - s->dl]))) {
	    line_h2 |= *(hp - hbpl + 1);
	    if (s->i > 1 || (y > 1 && !s->reset[plane][layer - s->dl]))
	      line_h3 |= *(hp - hbpl - hbpl + 1);
	  }
	}
	if (s->options & JBG_LRLTWO) {
	  /* two line template */
	  do {
	    if (tx) {
	      if ((unsigned) tx > x)
		a = 0;
	      else if (tx < 8)
		a = ((line_h1 >> (tx - 5)) & 0x010);
	      else {
		o = (x - tx) - (x & ~7L);
		a = (hp[o >> 3] >> (7 - (o & 7))) & 1;
		a <<= 4;
	      }
	      assert(tx > 31 ||
		     a == ((line_h1 >> (tx - 5)) & 0x010));
	      pix = arith_decode(se, (((line_h2 >> 9) & 0x3e0) | a |
				      (line_h1 & 0x00f)));
	    } else
	      pix = arith_decode(se, (((line_h2 >> 9) & 0x3f0) |
				      (line_h1 & 0x00f)));
	    if (se->result == JBG_MORE || se->result == JBG_MARKER)
	      goto leave;
	    line_h1 = (line_h1 << 1) | pix;
	    line_h2 <<= 1;
	  } while ((++x & 7) && x < hx);
	} else {
	  /* three line template */
	  do {
	    if (tx) {
	      if ((unsigned) tx > x)
		a = 0;
	      else if (tx < 8)
		a = ((line_h1 >> (tx - 3)) & 0x004);
	      else {
		o = (x - tx) - (x & ~7L);
		a = (hp[o >> 3] >> (7 - (o & 7))) & 1;
		a <<= 2;
	      }
	      assert(tx > 31 ||
		     a == ((line_h1 >> (tx - 3)) & 0x004));
	      pix = arith_decode(se, (((line_h3 >>  7) & 0x380) |
				      ((line_h2 >> 11) & 0x078) | a |
				      (line_h1 & 0x003)));
	    } else
	      pix = arith_decode(se, (((line_h3 >>  7) & 0x380) |
				      ((line_h2 >> 11) & 0x07c) |
				      (line_h1 & 0x003)));
	    if (se->result == JBG_MORE || se->result == JBG_MARKER)
	      goto leave;
	    
	    line_h1 = (line_h1 << 1) | pix;
	    line_h2 <<= 1;
	    line_h3 <<= 1;
	  } while ((++x & 7) && x < hx);
	} /* if (s->options & JBG_LRLTWO) */
	*hp++ = line_h1;
      } /* while */
      *(hp - 1) <<= hbpl * 8 - hx;
      x = 0;
      s->pseudo = 1;
    } /* for (i = ...) */
    
  } else {

    /*
     *  Decode differential layer
     */

  }

 leave:

  /* save a few local variables */
  s->line_h1 = line_h1;
  s->line_h2 = line_h2;
  s->line_h3 = line_h3;
  s->line_l1 = line_l1;
  s->line_l2 = line_l2;
  s->line_l3 = line_l3;
  s->x = x;

  return se->pscd_ptr - data;
}


/*
 * Provide a new BIE fragment to the decoder.
 *
 * If cnt is not NULL, then *cnt will contain after the call the
 * number of actually read bytes. If the data was not complete, then
 * the return value will be JBG_EAGAIN and *cnt == len. In case this
 * function has returned with JBG_EOK, then it has reached the end of
 * a BIE but it can be called again with data from the next BIE if
 * there exists one in order to get to a higher resolution layer. In
 * case the return value was JBG_EOK_INTR then this function can be
 * called again with the rest of the BIE, because parsing the BIE has
 * been interrupted by a jbg_dec_maxsize() specification. In both
 * cases the remaining len - *cnt bytes of the previous block will
 * have to passed to this function again (if len > *cnt). In case of
 * any other return value than JBG_EOK, JBG_EOK_INTR or JBG_EAGAIN, a
 * serious problem has occured and the only function you should call
 * is jbg_dec_free() in order to remove the mess (and probably
 * jbg_strerror() in order to find out what to tell the user).
 */
int jbg_dec_in(struct jbg_dec_state *s, unsigned char *data, size_t len,
	       size_t *cnt)
{
  int i, j, required_length;
  unsigned long x, y;
  unsigned long is[3], ie[3];
  size_t dummy_cnt;

  if (!cnt) cnt = &dummy_cnt;
  *cnt = 0;
  if (len < 1) return JBG_EAGAIN;

  /* read in 20-byte BIH */
  if (s->bie_len < 20) {
    while (s->bie_len < 20 && *cnt < len)
      s->buffer[s->bie_len++] = data[(*cnt)++];
    if (s->bie_len < 20) 
      return JBG_EAGAIN;
    if (s->buffer[1] < s->buffer[0])
      return JBG_EINVAL;
    /* test whether this looks like a valid JBIG header at all */
    if (s->buffer[3] != 0 || (s->buffer[18] & 0xf0) != 0 ||
	(s->buffer[19] & 0x80) != 0)
      return JBG_EINVAL;
    if (s->buffer[0] != s->d + 1)
      return JBG_ENOCONT;
    s->dl = s->buffer[0];
    s->d = s->buffer[1];
    if (s->dl == 0)
      s->planes = s->buffer[2];
    else
      if (s->planes != s->buffer[2])
	return JBG_ENOCONT;
    x = (((long) s->buffer[ 4] << 24) | ((long) s->buffer[ 5] << 16) |
	 ((long) s->buffer[ 6] <<  8) | (long) s->buffer[ 7]);
    y = (((long) s->buffer[ 8] << 24) | ((long) s->buffer[ 9] << 16) |
	 ((long) s->buffer[10] <<  8) | (long) s->buffer[11]);
    if (s->dl != 0 && ((s->xd << (s->d - s->dl + 1)) != x &&
		       (s->yd << (s->d - s->dl + 1)) != y))
      return JBG_ENOCONT;
    s->xd = x;
    s->yd = y;
    s->l0 = (((long) s->buffer[12] << 24) | ((long) s->buffer[13] << 16) |
	     ((long) s->buffer[14] <<  8) | (long) s->buffer[15]);
    /* ITU-T T.85 trick not directly supported by decoder; for full
     * T.85 compatibility with respect to all NEWLEN marker scenarios,
     * preprocess BIE with jbg_newlen() before passing it to the decoder. */
    if (s->yd == 0xffffffff)
      return JBG_EIMPL;
    if (!s->planes || !s->xd || !s->yd || !s->l0)
      return JBG_EINVAL;
    /* prevent uint32 overflow: s->l0 * 2 ^ s->d < 2 ^ 32 */
    if (s->d > 31 || (s->d != 0 && s->l0 >= (1UL << (32 - s->d))))
      return JBG_EIMPL;
    s->mx = s->buffer[16];
    if (s->mx > 127)
      return JBG_EINVAL;
    s->my = s->buffer[17];
#if 0
    if (s->my > 0) 
      return JBG_EIMPL;
#endif
    s->order = s->buffer[18];
    if (iindex[s->order & 7][0] < 0)
      return JBG_EINVAL;
    /* HITOLO and SEQ currently not yet implemented */
    if (s->dl != s->d && (s->order & JBG_HITOLO || s->order & JBG_SEQ))
      return JBG_EIMPL;
    s->options = s->buffer[19];

    /* calculate number of stripes that will be required */
    s->stripes = jbg_stripes(s->l0, s->yd, s->d);
    
    /* some initialization */
    s->ii[iindex[s->order & 7][STRIPE]] = 0;
    s->ii[iindex[s->order & 7][LAYER]] = s->dl;
    s->ii[iindex[s->order & 7][PLANE]] = 0;
    if (s->dl == 0) {
      s->s      = (struct jbg_ardec_state **)
	checked_malloc(s->planes, sizeof(struct jbg_ardec_state *));
      s->tx     = (int **) checked_malloc(s->planes, sizeof(int *));
      s->ty     = (int **) checked_malloc(s->planes, sizeof(int *));
      s->reset  = (int **) checked_malloc(s->planes, sizeof(int *));
      s->lntp   = (int **) checked_malloc(s->planes, sizeof(int *));
      s->lhp[0] = (unsigned char **)
	checked_malloc(s->planes, sizeof(unsigned char *));
      s->lhp[1] = (unsigned char **)
	checked_malloc(s->planes, sizeof(unsigned char *));
      for (i = 0; i < s->planes; i++) {
	s->s[i]     = (struct jbg_ardec_state *)
	  checked_malloc(s->d - s->dl + 1, sizeof(struct jbg_ardec_state));
	s->tx[i]    = (int *) checked_malloc(s->d - s->dl + 1, sizeof(int));
	s->ty[i]    = (int *) checked_malloc(s->d - s->dl + 1, sizeof(int));
	s->reset[i] = (int *) checked_malloc(s->d - s->dl + 1, sizeof(int));
	s->lntp[i]  = (int *) checked_malloc(s->d - s->dl + 1, sizeof(int));
	s->lhp[ s->d    & 1][i] = (unsigned char *)
	  checked_malloc(s->yd, jbg_ceil_half(s->xd, 3));
	s->lhp[(s->d-1) & 1][i] = (unsigned char *)
	  checked_malloc(jbg_ceil_half(s->yd, 1), jbg_ceil_half(s->xd, 1+3));
      }
    } else {
      for (i = 0; i < s->planes; i++) {
	s->s[i]     = (struct jbg_ardec_state *)
	  checked_realloc(s->s[i], s->d - s->dl + 1,
			  sizeof(struct jbg_ardec_state));
	s->tx[i]    = (int *) checked_realloc(s->tx[i],
					      s->d - s->dl + 1, sizeof(int));
	s->ty[i]    = (int *) checked_realloc(s->ty[i],
					      s->d - s->dl + 1, sizeof(int));
	s->reset[i] = (int *) checked_realloc(s->reset[i],
					      s->d - s->dl + 1, sizeof(int));
	s->lntp[i]  = (int *) checked_realloc(s->lntp[i],
					      s->d - s->dl + 1, sizeof(int));
	s->lhp[ s->d    & 1][i] = (unsigned char *)
	  checked_realloc(s->lhp[ s->d    & 1][i],
			  s->yd, jbg_ceil_half(s->xd, 3));
	s->lhp[(s->d-1) & 1][i] = (unsigned char *)
	  checked_realloc(s->lhp[(s->d-1) & 1][i],
			  jbg_ceil_half(s->yd, 1), jbg_ceil_half(s->xd, 1+3));
      }
    }
    for (i = 0; i < s->planes; i++)
      for (j = 0; j <= s->d - s->dl; j++)
	arith_decode_init(s->s[i] + j, 0);
    if (s->dl == 0 || (s->options & JBG_DPON && !(s->options & JBG_DPPRIV)))
      s->dppriv = jbg_dptable;
    s->comment_skip = 0;
    s->buf_len = 0;
    s->x = 0;
    s->i = 0;
    s->pseudo = 1;
    s->at_moves = 0;
  }

  /* read in DPTABLE */
  if (s->bie_len < 20 + 1728 && 
      (s->options & (JBG_DPON | JBG_DPPRIV | JBG_DPLAST)) ==
      (JBG_DPON | JBG_DPPRIV)) {
    assert(s->bie_len >= 20);
    while (s->bie_len < 20 + 1728 && *cnt < len)
      s->buffer[s->bie_len++ - 20] = data[(*cnt)++];
    if (s->bie_len < 20 + 1728) 
      return JBG_EAGAIN;
    if (!s->dppriv || s->dppriv == jbg_dptable)
      s->dppriv = (char *) checked_malloc(1728, sizeof(char));
    jbg_dppriv2int(s->dppriv, s->buffer);
  }

  /*
   * BID processing loop
   */
  
  while (*cnt < len) {

    /* process floating marker segments */

    /* skip COMMENT contents */
    if (s->comment_skip) {
      if (s->comment_skip <= len - *cnt) {
	*cnt += s->comment_skip;
	s->comment_skip = 0;
      } else {
	s->comment_skip -= len - *cnt;
	*cnt = len;
      }
      continue;
    }

    /* load complete marker segments into s->buffer for processing */
    if (s->buf_len > 0) {
      assert(s->buffer[0] == MARKER_ESC);
      while (s->buf_len < 2 && *cnt < len)
	s->buffer[s->buf_len++] = data[(*cnt)++];
      if (s->buf_len < 2) continue;
      switch (s->buffer[1]) {
      case MARKER_COMMENT: required_length = 6; break;
      case MARKER_ATMOVE:  required_length = 8; break;
      case MARKER_NEWLEN:  required_length = 6; break;
      case MARKER_ABORT:
      case MARKER_SDNORM:
      case MARKER_SDRST:   required_length = 2; break;
      case MARKER_STUFF:
	/* forward stuffed 0xff to arithmetic decoder */
	s->buf_len = 0;
	decode_pscd(s, s->buffer, 2);
	continue;
      default:
	return JBG_EMARKER;
      }
      while (s->buf_len < required_length && *cnt < len)
	s->buffer[s->buf_len++] = data[(*cnt)++];
      if (s->buf_len < required_length) continue;
      /* now the buffer is filled with exactly one marker segment */
      switch (s->buffer[1]) {
      case MARKER_COMMENT:
	s->comment_skip =
	  (((long) s->buffer[2] << 24) | ((long) s->buffer[3] << 16) |
	   ((long) s->buffer[4] <<  8) | (long) s->buffer[5]);
	break;
      case MARKER_ATMOVE:
	if (s->at_moves < JBG_ATMOVES_MAX) {
	  s->at_line[s->at_moves] =
	    (((long) s->buffer[2] << 24) | ((long) s->buffer[3] << 16) |
	     ((long) s->buffer[4] <<  8) | (long) s->buffer[5]);
	  s->at_tx[s->at_moves] = (signed char) s->buffer[6];
	  s->at_ty[s->at_moves] = s->buffer[7];
	  if (s->at_tx[s->at_moves] < - (int) s->mx ||
	      s->at_tx[s->at_moves] >   (int) s->mx ||
	      s->at_ty[s->at_moves] >   (int) s->my ||
	      (s->at_ty[s->at_moves] == 0 && s->at_tx[s->at_moves] < 0))
	    return JBG_EINVAL;
	  if (s->at_ty[s->at_moves] != 0)
	    return JBG_EIMPL;
	  s->at_moves++;
	} else
	  return JBG_EIMPL;
	break;
      case MARKER_NEWLEN:
	y = (((long) s->buffer[2] << 24) | ((long) s->buffer[3] << 16) |
	     ((long) s->buffer[4] <<  8) | (long) s->buffer[5]);
	if (y > s->yd || !(s->options & JBG_VLENGTH))
	  return JBG_EINVAL;
	s->yd = y;
	/* calculate again number of stripes that will be required */
	s->stripes = jbg_stripes(s->l0, s->yd, s->d);
	break;
      case MARKER_ABORT:
	return JBG_EABORT;
	
      case MARKER_SDNORM:
      case MARKER_SDRST:
	/* decode final pixels based on trailing zero bytes */
	decode_pscd(s, s->buffer, 2);

	arith_decode_init(s->s[s->ii[iindex[s->order & 7][PLANE]]] + 
			  s->ii[iindex[s->order & 7][LAYER]] - s->dl,
			  s->ii[iindex[s->order & 7][STRIPE]] != s->stripes - 1
			  && s->buffer[1] != MARKER_SDRST);
	
	s->reset[s->ii[iindex[s->order & 7][PLANE]]]
	  [s->ii[iindex[s->order & 7][LAYER]] - s->dl] =
	    (s->buffer[1] == MARKER_SDRST);
	
	/* prepare for next SDE */
	s->x = 0;
	s->i = 0;
	s->pseudo = 1;
	s->at_moves = 0;
	
	/* increment layer/stripe/plane loop variables */
	/* start and end value for each loop: */
	is[iindex[s->order & 7][STRIPE]] = 0;
	ie[iindex[s->order & 7][STRIPE]] = s->stripes - 1;
	is[iindex[s->order & 7][LAYER]] = s->dl;
	ie[iindex[s->order & 7][LAYER]] = s->d;
	is[iindex[s->order & 7][PLANE]] = 0;
	ie[iindex[s->order & 7][PLANE]] = s->planes - 1;
	i = 2;  /* index to innermost loop */
	do {
	  j = 0;  /* carry flag */
	  if (++s->ii[i] > ie[i]) {
	    /* handling overflow of loop variable */
	    j = 1;
	    if (i > 0)
	      s->ii[i] = is[i];
	  }
	} while (--i >= 0 && j);

	s->buf_len = 0;
	
	/* check whether this have been all SDEs */
	if (j) {
#ifdef DEBUG
	  fprintf(stderr, "This was the final SDE in this BIE, "
		  "%d bytes left.\n", len - *cnt);
#endif
	  s->bie_len = 0;
	  return JBG_EOK;
	}

	/* check whether we have to abort because of xmax/ymax */
	if (iindex[s->order & 7][LAYER] == 0 && i < 0) {
	  /* LAYER is the outermost loop and we have just gone to next layer */
	  if (jbg_ceil_half(s->xd, s->d - s->ii[0]) > s->xmax ||
	      jbg_ceil_half(s->yd, s->d - s->ii[0]) > s->ymax) {
	    s->xmax = 4294967295UL;
	    s->ymax = 4294967295UL;
	    return JBG_EOK_INTR;
	  }
	  if (s->ii[0] > (unsigned long) s->dmax) {
	    s->dmax = 256;
	    return JBG_EOK_INTR;
	  }
	}

	break;
      }
      s->buf_len = 0;

    } else if (data[*cnt] == MARKER_ESC)
      s->buffer[s->buf_len++] = data[(*cnt)++];

    else {

      /* we have found PSCD bytes */
      *cnt += decode_pscd(s, data + *cnt, len - *cnt);
      if (*cnt < len && data[*cnt] != 0xff) {
#ifdef DEBUG
	fprintf(stderr, "PSCD was longer than expected, unread bytes "
		"%02x %02x %02x %02x ...\n", data[*cnt], data[*cnt+1],
		data[*cnt+2], data[*cnt+3]);
#endif
	return JBG_EINVAL;
      }
      
    }
  }  /* of BID processing loop 'while (*cnt < len) ...' */

  return JBG_EAGAIN;
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call this
 * function in order to find out the width of the image.
 */
long jbg_dec_getwidth(const struct jbg_dec_state *s)
{
  if (s->d < 0)
    return -1;
  if (iindex[s->order & 7][LAYER] == 0) {
    if (s->ii[0] < 1)
      return -1;
    else
      return jbg_ceil_half(s->xd, s->d - (s->ii[0] - 1));
  }

  return s->xd;
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call this
 * function in order to find out the height of the image.
 */
long jbg_dec_getheight(const struct jbg_dec_state *s)
{
  if (s->d < 0)
    return -1;
  if (iindex[s->order & 7][LAYER] == 0) {
    if (s->ii[0] < 1)
      return -1;
    else
      return jbg_ceil_half(s->yd, s->d - (s->ii[0] - 1));
  }
  
  return s->yd;
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call this
 * function in order to get a pointer to the image.
 */
unsigned char *jbg_dec_getimage(const struct jbg_dec_state *s, int plane)
{
  if (s->d < 0)
    return NULL;
  if (iindex[s->order & 7][LAYER] == 0) {
    if (s->ii[0] < 1)
      return NULL;
    else
      return s->lhp[(s->ii[0] - 1) & 1][plane];
  }
  
  return s->lhp[s->d & 1][plane];
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call
 * this function in order to find out the size in bytes of one
 * bitplane of the image.
 */
long jbg_dec_getsize(const struct jbg_dec_state *s)
{
  if (s->d < 0)
    return -1;
  if (iindex[s->order & 7][LAYER] == 0) {
    if (s->ii[0] < 1)
      return -1;
    else
      return 
	jbg_ceil_half(s->xd, s->d - (s->ii[0] - 1) + 3) *
	jbg_ceil_half(s->yd, s->d - (s->ii[0] - 1));
  }
  
  return jbg_ceil_half(s->xd, 3) * s->yd;
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call
 * this function in order to find out the size of the image that you
 * can retrieve with jbg_merge_planes().
 */
long jbg_dec_getsize_merged(const struct jbg_dec_state *s)
{
  if (s->d < 0)
    return -1;
  if (iindex[s->order & 7][LAYER] == 0) {
    if (s->ii[0] < 1)
      return -1;
    else
      return 
	jbg_ceil_half(s->xd, s->d - (s->ii[0] - 1)) *
	jbg_ceil_half(s->yd, s->d - (s->ii[0] - 1)) *
	((s->planes + 7) / 8);
  }
  
  return s->xd * s->yd * ((s->planes + 7) / 8);
}


/* 
 * The destructor function which releases any resources obtained by the
 * other decoder functions.
 */
void jbg_dec_free(struct jbg_dec_state *s)
{
  int i;

  if (s->d < 0 || s->s == NULL)
    return;
  s->d = -2;

  for (i = 0; i < s->planes; i++) {
    checked_free(s->s[i]);
    checked_free(s->tx[i]);
    checked_free(s->ty[i]);
    checked_free(s->reset[i]);
    checked_free(s->lntp[i]);
    checked_free(s->lhp[0][i]);
    checked_free(s->lhp[1][i]);
  }
  
  checked_free(s->s);
  checked_free(s->tx);
  checked_free(s->ty);
  checked_free(s->reset);
  checked_free(s->lntp);
  checked_free(s->lhp[0]);
  checked_free(s->lhp[1]);
  if (s->dppriv && s->dppriv != jbg_dptable)
    checked_free(s->dppriv);

  s->s = NULL;

  return;
}


/*
 * Split bigendian integer pixel field into separate bit planes. In the
 * src array, every pixel is represented by a ((has_planes + 7) / 8) byte
 * long word, most significant byte first. While has_planes describes
 * the number of used bits per pixel in the source image, encode_plane
 * is the number of most significant bits among those that we
 * actually transfer to dest.
 */
void jbg_split_planes(unsigned long x, unsigned long y, int has_planes,
		      int encode_planes,
		      const unsigned char *src, unsigned char **dest,
		      int use_graycode)
{
  unsigned long bpl = jbg_ceil_half(x, 3);  /* bytes per line in dest plane */
  unsigned long line, i;
  unsigned k = 8;
  int p;
  unsigned prev;     /* previous *src byte shifted by 8 bit to the left */
  register int bits, msb = has_planes - 1;
  int bitno;

  /* sanity checks */
  if (encode_planes > has_planes)
    encode_planes = has_planes;
  use_graycode = use_graycode != 0 && encode_planes > 1;
  
  for (p = 0; p < encode_planes; p++)
    memset(dest[p], 0, bpl * y);
  
  for (line = 0; line < y; line++) {                 /* lines loop */
    for (i = 0; i * 8 < x; i++) {                    /* dest bytes loop */
      for (k = 0; k < 8 && i * 8 + k < x; k++) {     /* pixel loop */
	prev = 0;
	for (p = 0; p < encode_planes; p++) {        /* bit planes loop */
	  /* calculate which bit in *src do we want */
	  bitno = (msb - p) & 7;
	  /* put this bit with its left neighbor right adjusted into bits */
	  bits = (prev | *src) >> bitno;
	  /* go to next *src byte, but keep old */
	  if (bitno == 0)
	    prev = *src++ << 8;
	  /* make space for inserting new bit */
	  dest[p][bpl * line + i] <<= 1;
	  /* insert bit, if requested apply Gray encoding */
	  dest[p][bpl * line + i] |= (bits ^ (use_graycode & (bits>>1))) & 1;
	  /*
	   * Theorem: Let b(n),...,b(1),b(0) be the digits of a
	   * binary word and let g(n),...,g(1),g(0) be the digits of the
	   * corresponding Gray code word, then g(i) = b(i) xor b(i+1).
	   */
	}
	/* skip unused *src bytes */
	for (;p < has_planes; p++)
	  if (((msb - p) & 7) == 0)
	    src++;
      }
    }
    for (p = 0; p < encode_planes; p++)              /* right padding loop */
      dest[p][bpl * (line + 1) - 1] <<= 8 - k;
  }
  
  return;
}

/* 
 * Merge the separate bit planes decoded by the JBIG decoder into an
 * integer pixel field. This is essentially the counterpart to
 * jbg_split_planes().
 */
void jbg_dec_merge_planes(const struct jbg_dec_state *s, int use_graycode,
			  void (*data_out)(unsigned char *start, size_t len,
					   void *file), void *file)
{
#define BUFLEN 4096
  int bpp;
  unsigned long bpl, line, i;
  unsigned k = 8;
  int p;
  unsigned char buf[BUFLEN];
  unsigned char *bp = buf;
  unsigned char **src;
  unsigned long x, y;
  unsigned v;

  /* sanity check */
  use_graycode = use_graycode != 0;
  
  x = jbg_dec_getwidth(s);
  y = jbg_dec_getheight(s);
  if (x <= 0 || y <= 0)
    return;
  bpp = (s->planes + 7) / 8;   /* bytes per pixel in dest image */
  bpl = jbg_ceil_half(x, 3);   /* bytes per line in src plane */

  if (iindex[s->order & 7][LAYER] == 0)
    if (s->ii[0] < 1)
      return;
    else
      src = s->lhp[(s->ii[0] - 1) & 1];
  else
    src = s->lhp[s->d & 1];
  
  for (line = 0; line < y; line++) {                    /* lines loop */
    for (i = 0; i * 8 < x; i++) {                       /* src bytes loop */
      for (k = 0; k < 8 && i * 8 + k < x; k++) {        /* pixel loop */
	v = 0;
	for (p = 0; p < s->planes;) {                   /* dest bytes loop */
	  do {
	    v = (v << 1) |
	      (((src[p][bpl * line + i] >> (7 - k)) & 1) ^
	       (use_graycode & v));
	  } while ((s->planes - ++p) & 7);
	  *bp++ = v;
	  if (bp - buf == BUFLEN) {
	    data_out(buf, BUFLEN, file);
	    bp = buf;
	  }
	}
      }
    }
  }
  
  if (bp - buf > 0)
    data_out(buf, bp - buf, file);
  
  return;
}


/*
 * Given a pointer p to the first byte of either a marker segment or a
 * PSCD, as well as the length len of the remaining data, return
 * either the pointer to the first byte of the next marker segment or
 * PSCD, or p+len if this was the last one, or NULL if some error was
 * encountered. Possible errors are:
 *
 *  - not enough bytes left for complete marker segment
 *  - no marker segment terminates the PSCD
 *  - unknown marker code encountered
 *  
 */
unsigned char *jbg_next_pscdms(unsigned char *p, size_t len)
{
  unsigned char *pp;
  unsigned long l;

  if (len < 2)
    return NULL; /* not enough bytes left for complete marker segment */

  if (p[0] != MARKER_ESC || p[1] == MARKER_STUFF) {
    do {
      while (p[0] == MARKER_ESC && p[1] == MARKER_STUFF) {
	p += 2;
	len -= 2;
	if (len < 2)
	  return NULL; /* not enough bytes left for complete marker segment */
      }
      assert(len >= 2);
      pp = (unsigned char *) memchr(p, MARKER_ESC, len - 1);
      if (!pp)
	return NULL; /* no marker segment terminates the PSCD */
      l = pp - p;
      assert(l < len);
      p += l;
      len -= l;
    } while (p[1] == MARKER_STUFF);
  } else {
    switch (p[1]) {
    case MARKER_SDNORM:
    case MARKER_SDRST:
    case MARKER_ABORT:
      return p + 2;
    case MARKER_NEWLEN:
      if (len < 6)
	return NULL; /* not enough bytes left for complete marker segment */
      return p + 6;
    case MARKER_ATMOVE:
      if (len < 8)
	return NULL; /* not enough bytes left for complete marker segment */
      return p + 8;
    case MARKER_COMMENT:
      if (len < 6)
	return NULL; /* not enough bytes left for complete marker segment */
      l = (((long) p[2] << 24) | ((long) p[3] << 16) |
	   ((long) p[4] <<  8) |  (long) p[5]);
      if (len - 6 < l)
	return NULL; /* not enough bytes left for complete marker segment */
      return p + 6 + l;
    default:
      /* unknown marker sequence encountered */
      return NULL;
    }
  }

  return p;
}


/*
 * Scan a complete BIE for a NEWLEN marker segment, then read the new
 * YD value found in it and use it to overwrite the one in the BIE
 * header. Use this procedure if a BIE initially declares an
 * unreasonably high provisional YD value (e.g., 0xffffffff) or
 * depends on the fact that section 6.2.6.2 of ITU-T T.82 says that a
 * NEWLEN marker segment "could refer to a line in the immediately
 * preceding stripe due to an unexpected termination of the image or
 * the use of only such stripe". ITU-T.85 explicitely suggests the
 * use of this for fax machines that start transmission before having
 * encountered the end of the page. None of this is necessary for
 * BIEs produced by JBIG-KIT, which normally does not use NEWLEN.
 */
int jbg_newlen(unsigned char *bie, size_t len)
{
  unsigned char *p = bie + 20;
  int i;

  if (len < 20)
    return JBG_EAGAIN;
  if ((bie[19] & (JBG_DPON | JBG_DPPRIV | JBG_DPLAST))
      == (JBG_DPON | JBG_DPPRIV))
    p += 1728; /* skip DPTABLE */
  if (p >= bie + len)
    return JBG_EAGAIN;

  while ((p = jbg_next_pscdms(p, len - (p - bie)))) {
    if (p == bie + len)
      return JBG_EOK;
    else if (p[0] == MARKER_ESC)
      switch (p[1]) {
      case MARKER_NEWLEN:
	/* overwrite YD in BIH with YD from NEWLEN */
	for (i = 0; i < 4; i++) {
	  bie[8+i] = p[2+i];
	}
	return JBG_EOK;
      case MARKER_ABORT:
	return JBG_EABORT;
      }
  }
  return JBG_EINVAL;
}

#endif
