/*
 *  Portable Free JBIG image compression library
 *
 *  Markus Kuhn -- mskuhn@cip.informatik.uni-erlangen.de
 *
 *  $Id: jbig.c,v 1.2 1995-06-08 16:43:07 mskuhn Exp $
 *
 *  This module implements a portable standard C encoder and decoder
 *  using the JBIG lossless bi-level image compression algorithm as
 *  specified in International Standard ISO 11544 or equivalently as
 *  specified in ITU-T Recommendation T.82. See the file jbig.doc
 *  for usage instructions and application examples.
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
 *  It is possible that certain products which can be built using this
 *  software module might form inventions protected by patent rights in
 *  some countries (e.g. by patents about arithmetic coding algorithms
 *  owned by IBM and AT&T in the USA). Provision of this software by the
 *  author does NOT include any licences for any patents. In those
 *  countries were a patent licence is required for certain applications
 *  of this software module, you will have to obtain such a licence
 *  yourself.
 */

/* #define DEBUG */

#ifdef DEBUG
#include <stdio.h>
#else
#define NDEBUG
#endif

#include <stdlib.h>
#include <assert.h>

#include "jbig.h"


/* optional export of arithmetic coder functions for test purposes */
#ifdef TEST_CODEC
#define ARITH
#define ARITH_INL
#else
#define ARITH      static
#ifdef __GNUC__
#define ARITH_INL  static __inline__
#else
#define ARITH_INL  static
#endif
#endif

#define MX_MAX  23     /* maximal supported mx offset for
			* adaptive template in the encoder */

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

/* loop array indices */
#define STRIPE  0
#define LAYER   1
#define PLANE   2

/* special jbuf pointers (instead of NULL) */
#define SDE_DONE ((struct jbuf *) -1)
#define SDE_TODO ((struct jbuf *) 0)

/*
 * the following array specifies for each combination of the 3
 * ordering bits, which ii[] variable represents which dimension
 * of s->sde.
 */
static const int index[8][3] = {
  { 2, 1, 0 },    /* no ordering bit set */
  { -1, -1, -1},  /* SMID -> illegal combination */
  { 2, 0, 1 },    /* ILEAVE */
  { 1, 0, 2 },    /* SMID + ILEAVE */
  { 0, 2, 1 },    /* SEQ */
  { 1, 2, 0 },    /* SEQ + SMID */
  { 0, 1, 2 },    /* SEQ + ILEAVE */
  { -1, -1, -1 }  /* SEQ + SMID + ILEAVE -> illegal combination */
};


/*
 * Array [language][message] with text string error messages that correspond
 * to return values from public functions in this library.
 */
#define NEMSG         9  /* number of error codes */
#define NEMSG_LANG    3  /* number of supported languages */
static const char *errmsg[NEMSG_LANG][NEMSG] = {
  /* English (JBG_EN) */
  {
    "Everything is ok",                                     /* JBG_EOK */
    "Reached specified maximum size",                       /* JBG_EOK_INTR */
    "Unexpected end of data",                               /* JBG_EAGAIN */
    "Not enough memory available",                          /* JBG_ENOMEM */
    "ABORT marker found",                                   /* JBG_EABORT */
    "Unknown marker segment encountered",                   /* JBG_EMARKER */
    "Incremental BIE does not fit to previous one",         /* JBG_ENOCONT */
    "Invalid data encountered",                             /* JBG_EINVAL */
    "Unimplemented features used"                           /* JBG_EIMPL */
  },
  /* German (JBG_DE_8859_1) */
  {
    "Kein Problem aufgetreten",                             /* JBG_EOK */
    "Angegebene maximale Bildgr\366\337e erreicht",         /* JBG_EOK_INTR */
    "Unerwartetes Ende der Daten",                          /* JBG_EAGAIN */
    "Nicht gen\374gend Speicher vorhanden",                 /* JBG_ENOMEM */
    "Es wurde eine Abbruch-Sequenz gefunden",               /* JBG_EABORT */
    "Eine unbekannte Markierungssequenz wurde gefunden",    /* JBG_EMARKER */
    "Neue Daten passen nicht zu vorangegangenen Daten",     /* JBG_ENOCONT */
    "Es wurden ung\374ltige Daten gefunden",                /* JBG_EINVAL */
    "Noch nicht implementierte Optionen wurden benutzt"     /* JBG_EIMPL */
  },
  /* German (JBG_DE_UTF_8) */
  {
    "Kein Problem aufgetreten",                             /* JBG_EOK */
    "Angegebene maximale Bildgr\303\266\303\237e erreicht", /* JBG_EOK_INTR */
    "Unerwartetes Ende der Daten",                          /* JBG_EAGAIN */
    "Nicht gen\303\274gend Speicher vorhanden",             /* JBG_ENOMEM */
    "Es wurde eine Abbruch-Sequenz gefunden",               /* JBG_EABORT */
    "Eine unbekannte Markierungssequenz wurde gefunden",    /* JBG_EMARKER */
    "Neue Daten passen nicht zu vorangegangenen Daten",     /* JBG_ENOCONT */
    "Es wurden ung\303\274ltige Daten gefunden",            /* JBG_EINVAL */
    "Noch nicht implementierte Optionen wurden benutzt"     /* JBG_EIMPL */
  }
};



/*
 * The following three functions are the only places in this code, were
 * C library memory management functions are called. The whole JBIG
 * library has been designed in order to allow multi-threaded
 * execution. no static or global variables are used, so all fuctions
 * are fully reentrant. However if you want to use this multi-thread
 * capability and your malloc, realloc and free are not reentrant,
 * then simply add the necessary semaphores or mutex primitives below.
 */

static void *checked_malloc(size_t size)
{
  void *p;
  
  p = malloc(size);
  /* Full manual exception handling is ugly here for performance
   * reasons. If an adequate handling of lack of memory is required,
   * then use C++ and throw a C++ exception here. */
  if (!p)
    abort();

#ifdef DEBUG
  printf("%p = malloc(%ld)\n", p, (long) size);
#endif

  return p;
}


static void *checked_realloc(void *ptr, size_t size)
{
  void *p;

  p = realloc(ptr, size);
  /* Full manual exception handling is ugly here for performance
   * reasons. If an adequate handling of lack of memory is required,
   * then use C++ and throw a C++ exception here. */
  if (!p)
    abort();

#ifdef DEBUG
  printf("%p = realloc(%p, %ld)\n", p, ptr, (long) size);
#endif

  return p;
}


static void checked_free(void *ptr)
{
  free(ptr);

#ifdef DEBUG
  printf("free(%p)\n", ptr);
#endif

}



/*
 * The next functions implement the arithmedic encoder and decoder
 * required for JBIG. The same algorithm is also used in the arithmetic
 * variant of JPEG.
 */

#ifdef DEBUG
static long encoded_pixels = 0;
#endif

ARITH void arith_encode_init(struct enc_state *s, int reuse_st)
{
  int i;
  
  if (!reuse_st)
    for (i = 0; i < 4096; s->st[i++] = 0);
  s->c = 0;
  s->a = 0x10000;
  s->sc = 0;
  s->ct = 11;
  s->buffer = -1;    /* empty */
  
  return;
}


ARITH void arith_encode_flush(struct enc_state *s)
{
  long temp;

#ifdef DEBUG
  printf("encoded pixels = %ld, a = %05lx, c = %08lx\n",
	 encoded_pixels, s->a, s->c);
#endif

  /* find the s->c in the coding interval with the largest
   * number of trailing zero bits */
  if ((temp = (s->a - 1 + s->c) & 0xffff0000) < s->c)
    s->c = temp + 0x8000;
  else
    s->c = temp;
  /* send remaining bytes to output */
  s->c <<= s->ct;
  if (s->c & 0xf8000000) {
    /* one final overflow has to be handled */
    if (s->buffer >= 0) {
      s->byte_out(s->buffer + 1, s->file);
      if (s->buffer + 1 == MARKER_ESC)
	s->byte_out(MARKER_STUFF, s->file);
    }
    /* output 0x00 bytes only when more non-0x00 will follow */
    if (s->c & 0x7fff800)
      for (; s->sc; --s->sc)
	s->byte_out(0x00, s->file);
  } else {
    if (s->buffer >= 0)
      s->byte_out(s->buffer, s->file); 
    /* T.82 figure 30 says buffer+1 for the above line! Typo? */
    for (; s->sc; --s->sc) {
      s->byte_out(0xff, s->file);
      s->byte_out(MARKER_STUFF, s->file);
    }
  }
  /* output final bytes only if they are not 0x00 */
  if (s->c & 0x7fff800) {
    s->byte_out((s->c >> 19) & 0xff, s->file);
    if (((s->c >> 19) & 0xff) == MARKER_ESC)
      s->byte_out(MARKER_STUFF, s->file);
    if (s->c & 0x7f800) {
      s->byte_out((s->c >> 11) & 0xff, s->file);
      if (((s->c >> 11) & 0xff) == MARKER_ESC)
	s->byte_out(MARKER_STUFF, s->file);
    }
  }

  return;
}


ARITH_INL void arith_encode(struct enc_state *s, int cx, int pix) 
{
  extern short jbg_lsz[];
  extern char jbg_nmps[], jbg_nlps[], jbg_swtch[];
  register int lsz, ss;
  register unsigned char *st;
  long temp;

#ifdef DEBUG
  ++encoded_pixels;
#endif

  assert(cx >= 0 && cx < 4096);
  st = s->st + cx;
  ss = *st & 0x7f;
  assert(ss >= 0 && ss < 113);
  lsz = jbg_lsz[ss];

#if 0
  printf("pix = %d, cx = %d, mps = %d, st = %3d, lsz = 0x%04x, a = 0x%05lx, "
	 "c = 0x%08lx, ct = %2d, buf = 0x%02x\n",
	 pix, cx, !!(s->st[cx] & 0x80), ss, lsz, s->a, s->c, s->ct, s->buffer);
#endif

  if ((pix << 7 ^ s->st[cx]) & 0x80) {
    /* encode the less probable symbol */
    if ((s->a -= lsz) >= lsz) {
      /* If the interval size (lsz) for the less probable symbol (LPS)
       * is larger than the interval size for the MPS, then exchange
       * the two symbols for coding efficiency, otherwise code the LPS
       * as usual: */
      s->c += s->a;
      s->a = lsz;
    }
    /* Check whether MPS/LPS exchange is necessary */
    if (jbg_swtch[ss])
      *st ^= 0x80;
    /* chose next probability estimator status */
    *st &= 0x80;
    *st |= jbg_nlps[ss];
  } else {
    /* encode the more probable symbol */
    if ((s->a -= lsz) & 0xffff8000)
      return;   /* A >= 0x8000 -> ready, no renormalization required */
    if (s->a < lsz) {
      /* If the interval size (lsz) for the less probable symbol (LPS)
       * is larger than the interval size for the MPS, then exchange
       * the two symbols for coding efficiency: */
      s->c += s->a;
      s->a = lsz;
    }
    /* chose next probability estimator status */
    *st &= 0x80;
    *st |= jbg_nmps[ss];
  }

  /* renormalization of coding interval */
  do {
    s->a <<= 1;
    s->c <<= 1;
    --s->ct;
    if (s->ct == 0) {
      /* another byte is ready for output */
      temp = s->c >> 19;
      if (temp & 0xffffff00) {
	/* handle overflow over all buffered 0xff bytes */
	if (s->buffer >= 0) {
	  ++s->buffer;
	  s->byte_out(s->buffer, s->file);
	  if (s->buffer == MARKER_ESC)
	    s->byte_out(MARKER_STUFF, s->file);
	}
	for (; s->sc; --s->sc)
	  s->byte_out(0x00, s->file);
	s->buffer = temp & 0xff;  /* new output byte, might overflow later */
	assert(s->buffer != 0xff);
	/* can s->buffer really never become 0xff here? */
      } else if (temp == 0xff) {
	/* buffer 0xff byte (which might overflow later) */
	++s->sc;
      } else {
	/* output all buffered 0xff bytes, they will not overflow any more */
	if (s->buffer >= 0)
	  s->byte_out(s->buffer, s->file);
	for (; s->sc; --s->sc) {
	  s->byte_out(0xff, s->file);
	  s->byte_out(MARKER_STUFF, s->file);
	}
	s->buffer = temp;   /* buffer new output byte (can still overflow) */
      }
      s->c &= 0x7ffff;
      s->ct = 8;
    }
  } while (s->a < 0x8000);
 
  return;
}


ARITH void arith_decode_init(struct dec_state *s, int reuse_st)
{
  int i;
  
  if (!reuse_st)
    for (i = 0; i < 4096; s->st[i++] = 0);
  s->c = 0;
  s->a = 1;
  s->ct = 0;
  s->result = OK;
  s->startup = 1;
  return;
}


ARITH_INL int arith_decode(struct dec_state *s, int cx)
{
  extern short jbg_lsz[];
  extern char jbg_nmps[], jbg_nlps[], jbg_swtch[];
  register int lsz, ss;
  register unsigned char *st;
  int pix;

  /* renormalization */
  while (s->a < 0x8000 || s->startup) {
    if (s->ct < 1 && s->result != READY) {
      /* first we have to move a new byte into s->c */
      if (s->pscd_ptr >= s->pscd_end) {
	s->result = MORE;
	return -1;
      }
      if (*s->pscd_ptr == 0xff) 
	if (s->pscd_ptr + 1 >= s->pscd_end) {
	  s->result = MARKER;
	  return -1;
	} else {
	  if (*(s->pscd_ptr + 1) == MARKER_STUFF) {
	    s->c |= 0xff << (8 - s->ct);
	    s->ct += 8;
	    s->pscd_ptr += 2;
	    s->result = OK;
	  } else
	    s->result = READY;
	}
      else {
	s->c |= *(s->pscd_ptr++) << (8 - s->ct);
	s->ct += 8;
	s->result = OK;
      }
    }
    s->c <<= 1;
    s->a <<= 1;
    --s->ct;
    if (s->a == 0x10000)
      s->startup = 0;
  }

  st = s->st + cx;
  ss = *st & 0x7f;
  assert(ss < 113);
  lsz = jbg_lsz[ss];

#if 0
  printf("cx = %d, mps = %d, st = %3d, lsz = 0x%04x, a = 0x%05lx, "
	 "c = 0x%08lx, ct = %2d\n",
	 cx, !!(s->st[cx] & 0x80), ss, lsz, s->a, s->c, s->ct);
#endif

  if ((s->c >> 16) < (s->a -= lsz))
    if (s->a & 0xffff8000)
      return *st >> 7;
    else {
      /* MPS_EXCHANGE */
      if (s->a < lsz) {
	pix = 1 - (*st >> 7);
	/* Check whether MPS/LPS exchange is necessary */
	if (jbg_swtch[ss])
	  *st ^= 0x80;
	/* chose next probability estimator status */
	*st &= 0x80;
	*st |= jbg_nlps[ss];
      } else {
	pix = *st >> 7;
	*st &= 0x80;
	*st |= jbg_nmps[ss];
      }
    }
  else {
    /* LPS_EXCHANGE */
    if (s->a < lsz) {
      s->c -= s->a << 16;
      s->a = lsz;
      pix = *st >> 7;
      *st &= 0x80;
      *st |= jbg_nmps[ss];
    } else {
      s->c -= s->a << 16;
      s->a = lsz;
      pix = 1 - (*st >> 7);
      /* Check whether MPS/LPS exchange is necessary */
      if (jbg_swtch[ss])
	*st ^= 0x80;
      /* chose next probability estimator status */
      *st &= 0x80;
      *st |= jbg_nlps[ss];
    }
  }

  return pix;
}



/*
 * Memory management for buffers which are used for temporarily
 * storing SDEs by the encoder.
 *
 * The following functions manage a set of struct jbuf storage
 * containers were each can keep JBG_BUFSIZE bytes. The jbuf
 * containers can be linked to form linear double-chained lists for
 * which a number of operations are provided. Blocks which are
 * tempoarily not used any more are returned to a freelist which each
 * encoder keeps. Only the destructor of the encoder actually returns
 * the block via checked_free() to the stdlib memory management.
 */


/*
 * Allocate a new buffer block and initialize it. Try to get it from
 * the free_list, and if it is empty, call checked_malloc().
 */
static struct jbuf *jbuf_init(struct jbuf **free_list)
{
  struct jbuf *new_block;
  
  /* Test whether a block from the free list is available */
  if (*free_list) {
    new_block = *free_list;
    *free_list = new_block->next;
  } else {
    /* request a new memory block */
    new_block = (struct jbuf *) checked_malloc(sizeof(struct jbuf));
  }
  new_block->len = 0;
  new_block->next = NULL;
  new_block->previous = NULL;
  new_block->last = new_block;
  new_block->free_list = free_list;

  return new_block;
}


/*
 * Return an entire free_list to the memory management of stdlib.
 * This is only done by jbg_enc_free().
 */
static void jbuf_free(struct jbuf **free_list)
{
  struct jbuf *tmp;
  
  while (*free_list) {
    tmp = (*free_list)->next;
    checked_free(*free_list);
    *free_list = tmp;
  }
  
  return;
}


/*
 * Append a single byte to a single list that starts with the block
 * *(struct jbuf *) head. The type of *head is void here in order to
 * keep the interface of the arithmetic encoder gereric, which uses this
 * function as a call-back function in order to deliver single bytes
 * for a PSCD.
 */
static void jbuf_write(int b, void *head)
{
  struct jbuf *now;

  now = ((struct jbuf *) head)->last;
  if (now->len < JBG_BUFSIZE - 1) {
    now->d[now->len++] = b;
    return;
  }
  now->next = jbuf_init(((struct jbuf *) head)->free_list);
  now->next->previous = now;
  now->next->d[now->next->len++] = b;
  ((struct jbuf *) head)->last = now->next;

  return;
}


/*
 * Remove any trailing zero bytes from the end of a linked jbuf list,
 * however make sure that no zero byte is removed which directly
 * follows a 0xff byte (i.e., keep MARKER_ESC MARKER_STUFF sequences
 * intact). This function is used to remove any redundant final zero
 * bytes from a PSCD.
 */
static void jbuf_remove_zeros(struct jbuf *head)
{
  struct jbuf *last;

  while (1) {
    /* remove trailing 0x00 in last block of list until this block is empty */
    last = head->last;
    while (last->len && last->d[last->len - 1] == 0)
      last->len--;
    /* if block became really empty, remove it in case it is not the
     * only remaining block and then loop to next block */
    if (last->previous && !last->len) {
      head->last->next = *head->free_list;
      *head->free_list = head->last;
      head->last = last->previous;
    } else
      break;
  }

  /*
   * If the final non-zero byte is 0xff (MARKER_ESC), then we just have
   * removed a MARKER_STUFF and we will append it again now in order
   * to preserve PSCD status of byte stream.
   */
  if (head->last->d[head->last->len - 1] == MARKER_ESC)
    jbuf_write(MARKER_STUFF, head);
 
  return;
}


/*
 * The jbuf list which starts with block *new_prefix is concatenated
 * with the list which starts with block **start and *start will then point
 * to the first block of the new list.
 */
static void jbuf_prefix(struct jbuf *new_prefix, struct jbuf **start)
{
  new_prefix->last->next = *start;
  new_prefix->last->next->previous = new_prefix->last;
  new_prefix->last = new_prefix->last->next->last;
  *start = new_prefix;
  
  return;
}


/*
 * Send the contents of a jbuf list that starts with block **head to
 * the call back function data_out and return the blocks of the jbuf
 * list to the freelist from which these jbuf blocks have been taken.
 * After the call, *head == NULL.
 */
static void jbuf_output(struct jbuf **head,
			void (*data_out)(unsigned char *start,
					 size_t len, void *file),
			void *file)
{
  struct jbuf *tmp;
  
  while (*head) {
    data_out((*head)->d, (*head)->len, file);
    tmp = (*head)->next;
    (*head)->next = *(*head)->free_list;
    *(*head)->free_list = *head;
    *head = tmp;
  }
  
  return;
}


/*
 * Calculate y = ceil(x/2) applied n times. This function is used to
 * determine the number of pixels per row or column after n resolution
 * reductions. E.g. X[d-1] = ceil_half(X[d], 1) and X[0] =
 * ceil_half(X[d], d) as defined in clause 6.2.3 of T.82.
 */
unsigned long ceil_half(unsigned long x, int n)
{
  unsigned long mask;
  
  mask = (1 << n) - 1;     /* the lowest n bits are 1 here */
  return (x >> n) + ((mask & x) != 0);
}


/*
 * Initialize the status struct for the encoder.
 */
void jbg_enc_init(struct jbg_enc_state *s, unsigned long x, unsigned long y,
                  int planes, unsigned char **p,
                  void (*data_out)(unsigned char *start, size_t len,
				   void *file),
		  void *file)
{
  unsigned long i, lx;
  size_t bufsize;

  extern char resred[], dptable[];

  s->xd = x;
  s->yd = y;
  s->planes = planes;
  s->data_out = data_out;
  s->file = file;

  s->d = 0;
  s->dl = 0;
  s->dh = s->d;
  s->l0 = ceil_half(s->yd, s->d) / 35; /* 35 stripes/image */
  while (s->l0 << s->d > 128)          /* but not more than 128 lines/stripe */
    --s->l0;
  if (s->l0 < 2) s->l0 = 2;
  s->mx = 8;
  s->my = 0;
  s->order = JBG_ILEAVE | JBG_SMID;
  s->options = JBG_TPBON | JBG_TPDON | JBG_DPON;
  s->dppriv = dptable;
  s->res_tab = resred;
  
  s->highres = 0;
  s->lhp[0] = p;
  s->lhp[1] = checked_malloc(planes * sizeof(unsigned char *));
  bufsize = ((((ceil_half(x, 1) - 1) | 7) + 1) >> 3) * ceil_half(y, 1);
  for (i = 0; i < planes; i++)
    s->lhp[1][i] = checked_malloc(sizeof(unsigned char) * bufsize);
  
  s->free_list = NULL;
  s->s = (struct enc_state *) 
    checked_malloc(s->planes * sizeof(struct enc_state));
  s->tx = (int *) checked_malloc(s->planes * sizeof(int));
  lx = ceil_half(x, 1);
  s->tp = (char *) checked_malloc(lx * sizeof(char));
  for (i = 0; i < lx; s->tp[i++] = 2);
  s->sde = NULL;

  return;
}


/*
 * This function selects the number of differential layers based on
 * the maximum size requested for the lowest resolution layer. If
 * possible, a number of differential layers is selected, which will
 * keep the size of the lowest resolution layer below or equal to the
 * given width x and height y. However not more than 6 differential
 * resolution layers will be used. In addition, a reasonable value for
 * l0 (height of one stripe in the lowest resolution layer) is
 * selected, which obeys the recommended limitations for l0 in annex A
 * and C of the JBIG standard. The selected number of resolution layers
 * is returned. 
 */
int jbg_enc_lrlmax(struct jbg_enc_state *s, unsigned long x, 
		   unsigned long y)
{
  for (s->d = 0; s->d < 6; s->d++)
    if (ceil_half(s->xd, s->d) <= x && ceil_half(s->yd, s->d) <= y)
      break;
  s->dl = 0;
  s->dh = s->d;

  s->l0 = ceil_half(s->yd, s->d) / 35; /* 35 stripes/image */
  while (s->l0 << s->d > 128)          /* but not more than 128 lines/stripe */
    --s->l0;
  if (s->l0 < 2) s->l0 = 2;

  return s->d;
}


/*
 * As an alternative to jbg_enc_lrlmax(), the following function allows
 * to specify the number of layers directly. The stripe hight and layer
 * range is also adjusted automatically here.
 */
void jbg_enc_layers(struct jbg_enc_state *s, int d)
{
  if (d < 0 || d > 255)
    return;
  s->d  = d;
  s->dl = 0;
  s->dh = s->d;

  s->l0 = ceil_half(s->yd, s->d) / 35; /* 35 stripes/image */
  while (s->l0 << s->d > 128)          /* but not more than 128 lines/stripe */
    --s->l0;
  if (s->l0 < 2) s->l0 = 2;

  return;
}


/*
 * Specify the highest and lowest resolution layers which will be
 * written to the output file. Call this function not before
 * jbg_enc_layers() or jbg_enc_lrlmax(), because these two functions
 * reset the lowest and highest resolution layer to default values.
 * Negative values are ignored. The total number of layers is returned.
 */
int jbg_enc_lrange(struct jbg_enc_state *s, int dl, int dh)
{
  if (dl >= 0     && dl <= s->d) s->dl = dl;
  if (dh >= s->dl && dh <= s->d) s->dh = dh;

  return s->d;
}


/*
 * The following function allows to specify the bits describing the
 * options of the format as well as the maximum AT movement window and
 * the number of layer 0 lines per stripes.
 */
void jbg_enc_options(struct jbg_enc_state *s, int order, int options,
		     long l0, int mx, int my)
{
  if (order >= 0 && order <= 0x0f) s->order = order;
  if (options >= 0) s->options = options;
  if (l0 >= 0) s->l0 = l0;
  if (mx >= 0 && my < 128) s->mx = mx;
  if (my >= 0 && my < 256) s->my = my;

  return;
}


/*
 * This function actually does all the tricky work involved in producing
 * a SDE, which is stored in the appropriate s->sde[][][] element
 * for later output in the correct order.
 */
static void encode_sde(struct jbg_enc_state *s,
		       long stripe, int layer, int plane)
{
  unsigned char *hp, *lp1, *lp2, *p0, *p1, *q1, *q2;
  unsigned long hl, ll, hx, hy, lx, ly, hbpl, lbpl;
  unsigned long line_h0 = 0, line_h1 = 0;
  unsigned long line_h2, line_h3, line_l1, line_l2, line_l3;
  struct enc_state *se;
  unsigned long i, j, y;
  int t, ltp, ltp_old, cx;
  unsigned long c_all, c[MX_MAX + 1], cmin, cmax, clmin, clmax;
  int tmax, at_determined;
  int new_tx, new_tx_line = -1;
  struct jbuf *new_jbuf;

#ifdef DEBUG
  static long tp_lines, tp_exceptions, tp_pixels, dp_pixels;
  static long encoded_pixels;
#endif

  /* return immediately if this stripe has already been encoded */
  if (s->sde[stripe][layer][plane] != SDE_TODO)
    return;

#ifdef DEBUG
  if (stripe == 0)
    tp_lines = tp_exceptions = tp_pixels = dp_pixels = encoded_pixels = 0;
  printf("encode_sde: s/d/p = %2ld/%2d/%2d\n",
	 stripe, layer, plane);
#endif

  /* number of lines per stripe in highres image */
  hl = s->l0 << layer;
  /* number of lines per stripe in lowres image */
  ll = hl >> 1;
  /* current line number in highres image */
  y = stripe * hl;
  /* number of pixels in highres image */
  hx = ceil_half(s->xd, s->d - layer);
  hy = ceil_half(s->yd, s->d - layer);
  /* number of pixels in lowres image */
  lx = ceil_half(hx, 1);
  ly = ceil_half(hy, 1);
  /* bytes per line in highres and lowres image */
  hbpl = (((hx-1) | 7) + 1) >> 3;
  lbpl = (((lx-1) | 7) + 1) >> 3;
  /* pointer to first image byte of highres stripe */
  hp = s->lhp[s->highres][plane] + stripe * hl * hbpl;
  lp2 = s->lhp[1 - s->highres][plane] + stripe * ll * lbpl;
  lp1 = lp2 + lbpl;
  
  /* initialize arithmetic encoder */
  se = s->s + plane;
  arith_encode_init(se, stripe != 0);
  s->sde[stripe][layer][plane] = jbuf_init(&s->free_list);
  se->byte_out = jbuf_write;
  se->file = s->sde[stripe][layer][plane];

  /* initialize adaptive template movement algorithm */
  c_all = 0;
  for (t = 0; t <= s->mx; t++)
    c[t] = 0;
  if (stripe == 0)
    s->tx[plane] = 0;
  new_tx = -1;
  at_determined = 0;  /* we haven't yet decided the template move */
  if (s->mx == 0)
    at_determined = 1;

  /* initialize typical prediction */
  ltp = 0;
  if (stripe == 0)
    ltp_old = 0;
  else {
    ltp_old = 1;
    p1 = hp - hbpl;
    if (y > 1) {
      q1 = p1 - hbpl;
      while (p1 < hp && (ltp_old = (*p1++ == *q1++)));
    } else
      while (p1 < hp && (ltp_old = (*p1++ == 0)));
  }

  if (layer == 0) {

    /*
     *  Encode lowest resolution layer
     */

    for (i = 0; i < hl && y < hy; i++, y++) {

      /* check whether it is worth to perform an ATMOVE */
      if (!at_determined && c_all > 2048) {
	cmin = clmin = 0xffffffff;
	cmax = clmax = 0;
	tmax = 0;
	for (t = (s->options & JBG_LRLTWO) ? 5 : 3; t <= s->mx; t++) {
	  if (c[t] > cmax) cmax = c[t];
	  if (c[t] < cmin) cmin = c[t];
	  if (c[t] > c[tmax]) tmax = t;
	}
	clmin = (c[0] < cmin) ? c[0] : cmin;
	clmax = (c[0] > cmax) ? c[0] : cmax;
	if (c_all - cmax < (c_all >> 3) &&
	    cmax - c[s->tx[plane]] > c_all - cmax &&
	    cmax - c[s->tx[plane]] > (c_all >> 4) &&
	    /*                     ^ T.82 says here < !!! Typo ? */
	    cmax - (c_all - c[s->tx[plane]]) > c_all - cmax &&
	    cmax - (c_all - c[s->tx[plane]]) > (c_all >> 4) &&
	    cmax - cmin > (c_all >> 2) &&
	    (s->tx[plane] || clmax - clmin > (c_all >> 3))) {
	  /* we have decided to perform an ATMOVE */
	  new_tx = tmax;
	  if (!(s->options & JBG_DELAY_AT)) {
	    new_tx_line = i;
	    s->tx[plane] = new_tx;
	  }
	}
	at_determined = 1;
      }
      
      /* typical prediction */
      if (s->options & JBG_TPBON) {
	ltp = 1;
	p1 = hp;
	if (y > 0) {
	  q1 = hp - hbpl;
	  while (q1 < hp && (ltp = (*p1++ == *q1++)));
	} else
	  while (p1 < hp + hbpl && (ltp = (*p1++ == 0)));
	arith_encode(se, (s->options & JBG_LRLTWO) ? TPB2CX : TPB3CX,
		     ltp == ltp_old);
#ifdef DEBUG
	tp_lines += ltp;
#endif
	ltp_old = ltp;
	if (ltp) {
	  /* skip next line */
	  hp += hbpl;
	  continue;
	}
      }

      /*
       * Layout of the variables line_h1, line_h2, line_h3, which contain
       * as bits the neighbour pixels of the currently coded pixel X:
       *
       *          76543210765432107654321076543210     line_h3
       *          76543210765432107654321076543210     line_h2
       *  76543210765432107654321X76543210             line_h1
       */
      
      line_h1 = line_h2 = line_h3 = 0;
      if (y > 0) line_h2 = *(hp - hbpl) << 8;
      if (y > 1) line_h3 = *(hp - hbpl - hbpl) << 8;
      
      /* encode line */
      for (j = 0; j < hx; hp++) {
	line_h1 |= *hp;
	if (j < hbpl * 8 - 8 && y > 0) {
	  line_h2 |= *(hp - hbpl + 1);
	  if (y > 1)
	    line_h3 |= *(hp - hbpl - hbpl + 1);
	}
	if (s->options & JBG_LRLTWO) {
	  /* two line template */
	  do {
	    line_h1 <<= 1;  line_h2 <<= 1;  line_h3 <<= 1;
	    if (s->tx[plane])
	      arith_encode(se, (((line_h2 >> 10) & 0x3e0) |
				((line_h1 >> (4 + s->tx[plane])) & 0x010) |
				((line_h1 >>  9) & 0x00f)),
			   line_h1 >> 8 & 1);
	    else
	      arith_encode(se, (((line_h2 >> 10) & 0x3f0) |
				((line_h1 >>  9) & 0x00f)),
			   line_h1 >> 8 & 1);
#ifdef DEBUG
	    encoded_pixels++;
#endif
	    /* statistics for adaptive template changes */
	    if (!at_determined && j >= s->mx && j < hx-2) {
	      c[0] += !(((line_h2 >> 6) ^ line_h1) & 0x100);
	      for (t = 5; t <= s->mx; t++)
		c[t] += !(((line_h1 >> t) ^ line_h1) & 0x100);
	      ++c_all;
	    }
	  } while (++j & 7 && j < hx);
	} else {
	  /* three line template */
	  do {
	    line_h1 <<= 1;  line_h2 <<= 1;  line_h3 <<= 1;
	    if (s->tx[plane]) 
	      arith_encode(se, (((line_h3 >>  8) & 0x380) |
				((line_h2 >> 12) & 0x078) |
				((line_h1 >> (6 + s->tx[plane])) & 0x004) |
				((line_h1 >>  9) & 0x003)),
			   line_h1 >> 8 & 1);
	    else
	      arith_encode(se, (((line_h3 >>  8) & 0x380) |
				((line_h2 >> 12) & 0x07c) |
				((line_h1 >>  9) & 0x003)),
			   line_h1 >> 8 & 1);
#ifdef DEBUG
	    encoded_pixels++;
#endif
	    /* statistics for adaptive template changes */
	    if (!at_determined && j >= s->mx && j < hx-2) {
	      c[0] += !(((line_h2 >> 6) ^ line_h1) & 0x100);
	      for (t = 3; t <= s->mx; t++)
		c[t] += !(((line_h1 >> t) ^ line_h1) & 0x100);
	      ++c_all;
	    }
	  } while (++j & 7 && j < hx);
	} /* if (s->options & JBG_LRLTWO) */
      } /* for (j = ...) */
    } /* for (i = ...) */

  } else {

    /*
     *  Encode differential layer
     */
    
    for (i = 0; i < hl && y < hy; i++, y++) {

      /* check whether it is worth to perform an ATMOVE */
      if (!at_determined && c_all > 2048) {
	cmin = clmin = 0xffffffff;
	cmax = clmax = 0;
	tmax = 0;
	for (t = 3; t <= s->mx; t++) {
	  if (c[t] > cmax) cmax = c[t];
	  if (c[t] < cmin) cmin = c[t];
	  if (c[t] > c[tmax]) tmax = t;
	}
	clmin = (c[0] < cmin) ? c[0] : cmin;
	clmax = (c[0] > cmax) ? c[0] : cmax;
	if (c_all - cmax < (c_all >> 3) &&
	    cmax - c[s->tx[plane]] > c_all - cmax &&
	    cmax - c[s->tx[plane]] > (c_all >> 4) &&
	    /*                     ^ T.82 says here < !!! Typo ? */
	    cmax - (c_all - c[s->tx[plane]]) > c_all - cmax &&
	    cmax - (c_all - c[s->tx[plane]]) > (c_all >> 4) &&
	    cmax - cmin > (c_all >> 2) &&
	    (s->tx[plane] || clmax - clmin > (c_all >> 3))) {
	  /* we have decided to perform an ATMOVE */
	  new_tx = tmax;
	  if (!(s->options & JBG_DELAY_AT)) {
	    new_tx_line = i;
	    s->tx[plane] = new_tx;
	  }
#ifdef DEBUG
	  printf("ATMOVE: line=%ld, tx=%d, c_all=%ld\n", i, new_tx, c_all);
#endif
	}
	at_determined = 1;
      }
      
      if (i >> 1 >= ll - 1 || y >> 1 >= ly - 1)
	lp1 = lp2;

      /* typical prediction */
      if (s->options & JBG_TPDON && (i & 1) == 0) {
	q1 = lp1; q2 = lp2;
	p0 = p1 = hp;
	if (i < hl - 1 && y < hy - 1)
	  p0 = hp + hbpl;
	if ((y >> 1) > 0)
	  line_l3 = *(q2 - lbpl) << 8;
	else
	  line_l3 = 0;
	line_l2 = *q2 << 8;
	line_l1 = *q1 << 8;
	ltp = 1;
	for (j = 0; j < lx && ltp; q1++, q2++) {
	  if (j < lbpl * 8 - 8) {
	    if ((y >> 1) > 0)
	      line_l3 |= *(q2 - lbpl + 1);
	    line_l2 |= *(q2 + 1);
	    line_l1 |= *(q1 + 1);
	  }
	  do {
	    if (j >> 2 < hbpl) {
	      line_h1 = *(p1++);
	      line_h0 = *(p0++);
	    }
	    do {
	      line_l3 <<= 1;
	      line_l2 <<= 1;
	      line_l1 <<= 1;
	      line_h1 <<= 2;
	      line_h0 <<= 2;
	      cx = ((line_l3 >> 15 & 0x007) |
		    (line_l2 >> 12 & 0x038) |
		    (line_l1 >> 9  & 0x1c0));
	      if (cx == 0x000)
		if ((line_h1 & 0x300) == 0 && (line_h0 & 0x300) == 0)
		  s->tp[j] = 0;
		else {
		  ltp = 0;
#ifdef DEBUG
		  tp_exceptions++;
#endif
		}
	      else if (cx == 0x1ff)
		if ((line_h1 & 0x300) == 0x300 && (line_h0 & 0x300) == 0x300)
		  s->tp[j] = 1;
		else {
		  ltp = 0;
#ifdef DEBUG
		  tp_exceptions++;
#endif
		}
	      else
		s->tp[j] = 2;
	    } while (++j & 3 && j < lx);
	  } while (j & 7 && j < lx);
	} /* for (j = ...) */
	arith_encode(se, TPDCX, !ltp);
#ifdef DEBUG
	tp_lines += ltp;
#endif
      }


      /*
       * Layout of the variables line_h1, line_h2, line_h3, which contain
       * as bits the high resolution neighbour pixels of the currently coded
       * highres pixel X:
       *
       *            76543210 76543210 76543210 76543210     line_h3
       *            76543210 76543210 76543210 76543210     line_h2
       *   76543210 76543210 7654321X 76543210              line_h1
       *
       * Layout of the variables line_l1, line_l2, line_l3, which contain
       * the low resolution pixels near the currently coded pixel as bits.
       * The lowres pixel in which the currently coded highres pixel is
       * located is marked as Y:
       *
       *            76543210 76543210 76543210 76543210     line_l3
       *            76543210 7654321Y 76543210 76543210     line_l2
       *            76543210 76543210 76543210 76543210     line_l1
       */
      

      line_h1 = line_h2 = line_h3 = line_l1 = line_l2 = line_l3 = 0;
      if (y > 0) line_h2 = *(hp - hbpl) << 8;
      if (y > 1) line_h3 = *(hp - hbpl - hbpl) << 8;
      if ((y >> 1) > 0)
	line_l3 = *(lp2 - lbpl) << 8;
      line_l2 = *lp2 << 8;
      line_l1 = *lp1 << 8;
      
      /* encode line */
      for (j = 0; j < hx; lp1++, lp2++) {
	if ((j >> 1) < lbpl * 8 - 8) {
	  if ((y >> 1) > 0)
	    line_l3 |= *(lp2 - lbpl + 1);
	  line_l2 |= *(lp2 + 1);
	  line_l1 |= *(lp1 + 1);
	}
	do {

	  assert(hp - (s->lhp[s->highres][plane] + (stripe * hl + i) * hbpl)
		 == j >> 3);

	  assert(lp2 - (s->lhp[1-s->highres][plane] + (stripe * ll + (i>>1))
			* lbpl)
		 == j >> 4);

	  line_h1 |= *(hp++);
	  if (j < hbpl * 8 - 8) {
	    if (y > 0) {
	      line_h2 |= *(hp - hbpl);
	      if (y > 1)
		line_h3 |= *(hp - hbpl - hbpl);
	    }
	  }
	  do {
	    line_l1 <<= 1;  line_l2 <<= 1;  line_l3 <<= 1;
	    if (ltp && s->tp[j >> 1] < 2) {
	      /* pixel are typical and have not to be encoded */
	      line_h1 <<= 2;  line_h2 <<= 2;  line_h3 <<= 2;
#ifdef DEBUG
	      do {
		++tp_pixels;
	      } while (++j & 1 && j < hx);
#else
	      j += 2;
#endif
	    } else
	      do {
		line_h1 <<= 1;  line_h2 <<= 1;  line_h3 <<= 1;

		/* deterministic prediction */
		if (s->options & JBG_DPON)
		  if ((y & 1) == 0) {
		    if ((j & 1) == 0) {
		      /* phase 0 */
		      if (s->dppriv[(line_l3 >> 16 & 0x003) |
				    (line_l2 >> 14 & 0x00c) |
				    (line_h1 >> 5  & 0x010) |
				    (line_h2 >> 10 & 0x0e0)] < 2) {
#ifdef DEBUG
			++dp_pixels;
#endif
			continue;
		      }
		    } else {
		      /* phase 1 */
		      if (s->dppriv[((line_l3 >> 16 & 0x003) |
				     (line_l2 >> 14 & 0x00c) |
				     (line_h1 >> 5  & 0x030) |
				     (line_h2 >> 10 & 0x1c0)) + 256] < 2) {
#ifdef DEBUG
			++dp_pixels;
#endif
			continue;
		      }
		    }
		  } else {
		    if ((j & 1) == 0) {
		      /* phase 2 */
		      if (s->dppriv[((line_l3 >> 16 & 0x003) |
				     (line_l2 >> 14 & 0x00c) |
				     (line_h1 >> 5  & 0x010) |
				     (line_h2 >> 10 & 0x0e0) |
				     (line_h3 >> 7 & 0x700)) + 768] < 2) {
#ifdef DEBUG
			++dp_pixels;
#endif
			continue;
		      }
		    } else {
		      /* phase 3 */
		      if (s->dppriv[((line_l3 >> 16 & 0x003) |
				     (line_l2 >> 14 & 0x00c) |
				     (line_h1 >> 5  & 0x030) |
				     (line_h2 >> 10 & 0x1c0) |
				     (line_h3 >> 7  & 0xe00)) + 2816] < 2) {
#ifdef DEBUG
			++dp_pixels;
#endif
			continue;
		      }
		    }	
		  }	
		
		/* determine context */
		if (s->tx[plane])
		  cx = ((line_h1 >> 9  & 0x003) |
			(line_h1 >> (4 + s->tx[plane]) & 0x010) |
			(line_h2 >> 13 & 0x00c) |
			(line_h3 >> 11 & 0x020));
		else
		  cx = ((line_h1 >> 9  & 0x003) |
			(line_h2 >> 13 & 0x01c) |
			(line_h3 >> 11 & 0x020));
		if (j & 1) 
		  cx |= ((line_l2 >> 9  & 0x0c0) |
			 (line_l1 >> 7  & 0x300)) | (1 << 10);
		else
		  cx |= ((line_l2 >> 10 & 0x0c0) |
			 (line_l1 >> 8  & 0x300));
		cx |= (y & 1) << 11;
		
		arith_encode(se, cx, line_h1 >> 8 & 1);
#ifdef DEBUG
		encoded_pixels++;
#endif
		
		/* statistics for adaptive template changes */
		if (!at_determined && j >= s->mx) {
		  c[0] += !(((line_h2 >> 6) ^ line_h1) & 0x100);
		  for (t = 3; t <= s->mx; t++)
		    c[t] += !(((line_h1 >> t) ^ line_h1) & 0x100);
		  ++c_all;
		}
		
	      } while (++j & 1 && j < hx);
	  } while (j & 7 && j < hx);
	} while (j & 15 && j < hx);
      } /* for (j = ...) */

      /* low resolution pixels are used twice */
      if ((i & 1) == 0) {
	lp1 -= lbpl;
	lp2 -= lbpl;
      }
      
    } /* for (i = ...) */
  }
  
  arith_encode_flush(se);
  jbuf_remove_zeros(s->sde[stripe][layer][plane]);
  jbuf_write(MARKER_ESC, s->sde[stripe][layer][plane]);
  jbuf_write(MARKER_SDNORM, s->sde[stripe][layer][plane]);

  /* add ATMOVE */
  if (new_tx != -1) {
    if (s->options & JBG_DELAY_AT) {
      /* ATMOVE will become active at the first line of the next stripe */
      s->tx[plane] = new_tx;
      jbuf_write(MARKER_ESC, s->sde[stripe][layer][plane]);
      jbuf_write(MARKER_ATMOVE, s->sde[stripe][layer][plane]);
      jbuf_write(0, s->sde[stripe][layer][plane]);
      jbuf_write(0, s->sde[stripe][layer][plane]);
      jbuf_write(0, s->sde[stripe][layer][plane]);
      jbuf_write(0, s->sde[stripe][layer][plane]);
      jbuf_write(s->tx[plane], s->sde[stripe][layer][plane]);
      jbuf_write(0, s->sde[stripe][layer][plane]);
    } else {
      /* ATMOVE has already become active during this stripe
       * => we have to prefix the SDE data with an ATMOVE marker */
      new_jbuf = jbuf_init(&s->free_list);
      jbuf_write(MARKER_ESC, new_jbuf);
      jbuf_write(MARKER_ATMOVE, new_jbuf);
      jbuf_write((new_tx_line >> 24) & 0xff, new_jbuf);
      jbuf_write((new_tx_line >> 16) & 0xff, new_jbuf);
      jbuf_write((new_tx_line >> 8) & 0xff, new_jbuf);
      jbuf_write(new_tx_line & 0xff, new_jbuf);
      jbuf_write(new_tx, new_jbuf);
      jbuf_write(0, new_jbuf);
      jbuf_prefix(new_jbuf, &s->sde[stripe][layer][plane]);
    }
  }

#ifdef DEBUG
  if (stripe == s->stripes - 1)
    printf("tp_lines = %ld, tp_exceptions = %ld, tp_pixels = %ld, "
	   "dp_pixels = %ld, encoded_pixels = %ld\n",
	   tp_lines, tp_exceptions, tp_pixels, dp_pixels, encoded_pixels);
#endif

  return;
}


/*
 * Create the next lower resolution version of an image
 */
static void resolution_reduction(struct jbg_enc_state *s, int plane,
				 int higher_layer)
{
  unsigned long hx, hy, lx, ly, hbpl, lbpl;
  unsigned char *hp1, *hp2, *hp3, *lp;
  unsigned long line_h1, line_h2, line_h3, line_l2;
  unsigned long i, j;
  int pix, k, l;

  /* number of pixels in highres image */
  hx = ceil_half(s->xd, s->d - higher_layer);
  hy = ceil_half(s->yd, s->d - higher_layer);
  /* number of pixels in lowres image */
  lx = ceil_half(hx, 1);
  ly = ceil_half(hy, 1);
  /* bytes per line in highres and lowres image */
  hbpl = (((hx-1) | 7) + 1) >> 3;
  lbpl = (((lx-1) | 7) + 1) >> 3;
  /* pointers to first image bytes */
  hp2 = s->lhp[s->highres][plane];
  hp1 = hp2 + hbpl;
  hp3 = hp2 - hbpl;
  lp = s->lhp[1 - s->highres][plane];
  
#ifdef DEBUG
  printf("resolution_reduction: plane = %d, higher_layer = %d\n",
	 plane, higher_layer);
#endif

  /*
   * Layout of the variables line_h1, line_h2, line_h3, which contain
   * as bits the high resolution neighbour pixels of the currently coded
   * lowres pixel /\:
   *              \/
   *
   *   76543210 76543210 76543210 76543210     line_h3
   *   76543210 76543210 765432/\ 76543210     line_h2
   *   76543210 76543210 765432\/ 76543210     line_h1
   *
   * Layout of the variable line_l2, which contains the low resolution
   * pixels near the currently coded pixel as bits. The lowres pixel
   * which is currently coded is marked as X:
   *
   *   76543210 76543210 76543210 76543210     line_l2
   *                            X
   */
      
  for (i = 0; i < ly; i++) {
    if (2*i + 1 >= hy)
      hp1 = hp2;
    pix = 0;
    line_h1 = line_h2 = line_h3 = line_l2 = 0;
    for (j = 0; j < lbpl * 8; j += 8) {
      *lp = 0;
      line_l2 |= i ? lp[-lbpl] : 0;
      for (k = 0; k < 8 && j + k < lx; k += 4) {
	if ((j + k) >> 2 < hbpl) {
	  line_h3 |= i ? *hp3 : 0;
	  ++hp3;
	  line_h2 |= *(hp2++);
	  line_h1 |= *(hp1++);
	}
	for (l = 0; l < 4 && j + k + l < lx; l++) {
	  line_h3 <<= 2;
	  line_h2 <<= 2;
	  line_h1 <<= 2;
	  line_l2 <<= 1;
	  pix = s->res_tab[(line_h1 >> 8 & 0x007) |
			   (line_h2 >> 5 & 0x038) |
			   (line_h3 >> 2 & 0x1c0) |
			   pix << 9 | (line_l2 << 2 & 0xc00)];
	  *lp = *lp << 1 | pix;
	}
      }
      ++lp;
    }
    *(lp - 1) <<= lbpl * 8 - lx;
    hp1 += hbpl;
    hp2 += hbpl;
    hp3 += hbpl;
  }

#ifdef DEBUG
  {
    FILE *f;
    char fn[50];
    
    sprintf(fn, "dbg_d=%02d.pbm", higher_layer - 1);
    f = fopen(fn, "wb");
    fprintf(f, "P4\n%lu %lu\n", lx, ly);
    fwrite(s->lhp[1 - s->highres][plane], 1, lbpl * ly, f);
    fclose(f);
  }
#endif

  return;
}


/* 
 * This function is called inside the three loops of jbg_enc_out() in
 * order to write the next SDE. It has first to generate the required
 * SDE and all SDEs which have to be encoded before this SDE can be
 * created. The problem here is that if we want to output a lower
 * resolution layer, we have to allpy the resolution reduction
 * algorithm in order to get it. As we try to safe as much memory as
 * possible, the resolution reduction will overwrite previous higher
 * resolution bitmaps. Consequently, we have to encode and buffer SDEs
 * which depend on higher resolution layers before we can start the
 * resolution reduction. All this logic about which SDE has to be
 * encoded before resolution reduction is allowed is handled here.
 * This approach might be a little bit more complex than alternative
 * ways to do it, but it allows us to do the encoding with the minimal
 * possible amount of temporary memory.
 */
static void output_sde(struct jbg_enc_state *s,
		       long stripe, int layer, int plane)
{
  int lfcl;     /* lowest fully coded layer */
  long i, j;
  
  /* Determine the smallest resolution layer in this plane for which
   * not yet all stripes have been encoded into SDEs. This layer will
   * have to be completely coded, before we can apply the next
   * resolution reduction step. */
  lfcl = 0;
  for (i = s->d; i >= 0; i--)
    if (s->sde[s->stripes - 1][i][plane] == SDE_TODO) {
      lfcl = i + 1;
      break;
    }
  if (lfcl > s->d && s->d > 0 && stripe == 0) {
    /* perform the first resolution reduction */
    resolution_reduction(s, plane, s->d);
  }
  /* In case HITOLO is not used, we have to encode and store the higher
   * resolution layers first, although we do not need them right now. */
  while (lfcl - 1 > layer) {
    for (j = 0; j < s->stripes; j++)
      encode_sde(s, j, lfcl - 1, plane);
    --lfcl;
    s->highres ^= 1;
    if (lfcl > 1)
      resolution_reduction(s, plane, lfcl - 1);
  }
  
  encode_sde(s, stripe, layer, plane);

#ifdef DEBUG
  printf("writing SDE: s/d/p = %2ld/%2d/%2d\n", stripe, layer, plane);
#endif
  jbuf_output(&s->sde[stripe][layer][plane], s->data_out, s->file);
  s->sde[stripe][layer][plane] = SDE_DONE;
  
  if (stripe == s->stripes - 1 && layer > 0 &&
      s->sde[0][layer-1][plane] == SDE_TODO) {
    s->highres ^= 1;
    if (layer > 1)
      resolution_reduction(s, plane, layer - 1);
  }
  
  return;
}


/*
 * Convert the table which controls the deterministic prediction
 * process from the internal format into the representation required
 * for the 1728 byte long DPTABLE element of a BIH.
 *
 * The bit order of the DPTABLE format (see also ITU-T T.82 figure 13) is
 *
 * high res:   4  5  6     low res:  0  1
 *             7  8  9               2  3
 *            10 11 12
 *
 * were 4 table entries are packed into one byte, while we here use
 * internally an unpacked 6912 byte long table indexed by the following
 * bit order:
 *
 * high res:   7  6  5     high res:   8  7  6     low res:  1  0
 * (phase 0)   4  .  .     (phase 1)   5  4  .               3  2
 *             .  .  .                 .  .  .
 *
 * high res:  10  9  8     high res:  11 10  9
 * (phase 2)   7  6  5     (phase 3)   8  7  6
 *             4  .  .                 5  4  .
 */
void jbg_int2dppriv(unsigned char *dptable, const char *internal)
{
  int i, j, k;
  int trans0[ 8] = { 1, 0, 3, 2, 7, 6, 5, 4 };
  int trans1[ 9] = { 1, 0, 3, 2, 8, 7, 6, 5, 4 };
  int trans2[11] = { 1, 0, 3, 2, 10, 9, 8, 7, 6, 5, 4 };
  int trans3[12] = { 1, 0, 3, 2, 11, 10, 9, 8, 7, 6, 5, 4 };
  
  for (i = 0; i < 1728; dptable[i++] = 0);

#define FILL_TABLE1(offset, len, trans) \
  for (i = 0; i < len; i++) { \
    k = 0; \
    for (j = 0; j < 8; j++) \
      k |= ((i >> j) & 1) << trans[j]; \
    dptable[(i + offset) >> 2] |= \
      (internal[k + offset] & 3) << ((3 - (i&3)) << 1); \
  }

  FILL_TABLE1(   0,  256, trans0);
  FILL_TABLE1( 256,  512, trans1);
  FILL_TABLE1( 768, 2048, trans2);
  FILL_TABLE1(2816, 4096, trans3);

  return;
}


/*
 * Convert the table which controls the deterministic prediction
 * process from the 1728 byte long DPTABLE format into the 6912 byte long
 * internal format.
 */
void jbg_dppriv2int(char *internal, const unsigned char *dptable)
{
  int i, j, k;
  int trans0[ 8] = { 1, 0, 3, 2, 7, 6, 5, 4 };
  int trans1[ 9] = { 1, 0, 3, 2, 8, 7, 6, 5, 4 };
  int trans2[11] = { 1, 0, 3, 2, 10, 9, 8, 7, 6, 5, 4 };
  int trans3[12] = { 1, 0, 3, 2, 11, 10, 9, 8, 7, 6, 5, 4 };
  
#define FILL_TABLE2(offset, len, trans) \
  for (i = 0; i < len; i++) { \
    k = 0; \
    for (j = 0; j < 8; j++) \
      k |= ((i >> j) & 1) << trans[j]; \
    internal[k + offset] = \
      (dptable[(i + offset) >> 2] >> ((3 - (i & 3)) << 1)) & 3; \
  }

  FILL_TABLE2(   0,  256, trans0);
  FILL_TABLE2( 256,  512, trans1);
  FILL_TABLE2( 768, 2048, trans2);
  FILL_TABLE2(2816, 4096, trans3);

  return;
}


/*
 * Encode one full BIE and pass the generated data to the specified
 * call-back function
 */
void jbg_enc_out(struct jbg_enc_state *s)
{
  long i;
  int j, k;
  unsigned char bih[20];
  unsigned long xd, yd, mask;
  long ii[3], is[3], ie[3];    /* generic variables for the 3 nested loops */ 
  long stripe;
  int layer, plane;
  int order;
  unsigned char dpbuf[1728];
  extern char dptable[];

  /* some sanity checks */
  s->order &= JBG_HITOLO | JBG_SEQ | JBG_ILEAVE | JBG_SMID;
  order = s->order & (JBG_SEQ | JBG_ILEAVE | JBG_SMID);
  if (index[order][0] < 0)
    s->order = JBG_SMID | JBG_ILEAVE;
  if (s->options & JBG_DPON && s->dppriv != dptable)
    s->options |= JBG_DPPRIV;
  if (s->mx > MX_MAX)
    s->mx = MX_MAX;
  s->my = 0;
  if (s->mx && s->mx < ((s->options & JBG_LRLTWO) ? 5 : 3))
    s->mx = 0;
  if (s->d > 255 || s->d < 0 || s->dh > s->d || s->dh < 0 ||
      s->dl < 0 || s->dl > s->dh)
    return;

  /* calculate number of stripes that will be required */
  s->stripes =
    ((s->yd >> s->d) + ((((1 << s->d) - 1) & s->xd) != 0) + s->l0 - 1) / s->l0;

  /* allocate buffers for SDE pointers */
  if (s->sde == NULL) {
    s->sde = (struct jbuf ****)
      checked_malloc(s->stripes * sizeof(struct jbuf ***));
    for (i = 0; i < s->stripes; i++) {
      s->sde[i] = (struct jbuf ***)
	checked_malloc((s->d + 1) * sizeof(struct jbuf **));
      for (j = 0; j < s->d + 1; j++) {
	s->sde[i][j] = (struct jbuf **)
	  checked_malloc(s->planes * sizeof(struct jbuf *));
	for (k = 0; k < s->planes; k++)
	  s->sde[i][j][k] = SDE_TODO;
      }
    }
  }

  /* output BIH */
  bih[0] = s->dl;
  bih[1] = s->dh;
  bih[2] = s->planes;
  bih[3] = 0;
  mask = (1 << (s->d - s->dh)) - 1;     /* the lowest (layer-d) bits are 1 */
  xd = (s->xd >> (s->d - s->dh)) + ((mask & s->xd) != 0);
  yd = (s->yd >> (s->d - s->dh)) + ((mask & s->yd) != 0);
  bih[4] = xd >> 24;
  bih[5] = xd >> 16 & 0xff;
  bih[6] = xd >> 8 & 0xff;
  bih[7] = xd & 0xff;
  bih[8] = yd >> 24;
  bih[9] = yd >> 16 & 0xff;
  bih[10] = yd >> 8 & 0xff;
  bih[11] = yd & 0xff;
  bih[12] = s->l0 >> 24;
  bih[13] = s->l0 >> 16 & 0xff;
  bih[14] = s->l0 >> 8 & 0xff;
  bih[15] = s->l0 & 0xff;
  bih[16] = s->mx;
  bih[17] = s->my;
  bih[18] = s->order;
  bih[19] = s->options & 0x7f;
  s->data_out(bih, 20, s->file);
  if ((s->options & (JBG_DPON | JBG_DPPRIV | JBG_DPLAST)) ==
      (JBG_DPON | JBG_DPPRIV)) {
    /* write private table */
    jbg_int2dppriv(dpbuf, s->dppriv);
    s->data_out(dpbuf, 1728, s->file);
  }

  /*
   * Generic loops over all SDEs. Which loop represents layer, plane and
   * stripe depends on the option flags.
   */

  /* start and end value vor each loop */
  is[index[order][STRIPE]] = 0;
  ie[index[order][STRIPE]] = s->stripes - 1;
  is[index[order][LAYER]] = s->dl;
  ie[index[order][LAYER]] = s->dh;
  is[index[order][PLANE]] = 0;
  ie[index[order][PLANE]] = s->planes - 1;

  for (ii[0] = is[0]; ii[0] <= ie[0]; ii[0]++)
    for (ii[1] = is[1]; ii[1] <= ie[1]; ii[1]++)
      for (ii[2] = is[2]; ii[2] <= ie[2]; ii[2]++) {
	
	stripe = ii[index[order][STRIPE]];
	if (s->order & JBG_HITOLO)
	  layer = s->dh - (ii[index[order][LAYER]] - s->dl);
	else
	  layer = ii[index[order][LAYER]];
	plane = ii[index[order][PLANE]];

	output_sde(s, stripe, layer, plane);

      }

  return;
}


void jbg_enc_free(struct jbg_enc_state *s)
{
  int i, j, k;

#ifdef DEBUG
  printf("jbg_enc_free(%p)\n", s);
#endif

  /* clear buffers for SDEs */
  if (s->sde) {
    for (i = 0; i < s->stripes; i++) {
      for (j = 0; j < s->d + 1; j++) {
	for (k = 0; k < s->planes; k++)
	  if (s->sde[i][j][k] != SDE_DONE && s->sde[i][j][k] != SDE_TODO)
	    jbuf_free(&s->sde[i][j][k]);
	checked_free(s->sde[i][j]);
      }
      checked_free(s->sde[i]);
    }
    checked_free(s->sde);
  }

  /* clear free_list */
  jbuf_free(&s->free_list);

  /* clear memory for arithmetic encoder states */
  checked_free(s->s);

  /* clear memory for differential-layer typical prediction buffer */
  checked_free(s->tp);

  /* clear memory for adaptive template pixel offsets */
  checked_free(s->tx);

  /* clear lowres image buffers */
  if (s->lhp[1]) {
    for (i = 0; i < s->planes; i++)
      checked_free(s->lhp[1][i]);
    checked_free(s->lhp[1]);
  }

  return;
}


/*
 * Convert the error codes used by jbg_dec_in() into a string
 * written in the selected language and character set.
 */
const char *jbg_strerror(int errnum, int language)
{
  if (errnum < 0 || errnum >= NEMSG)
    return "Unknown error code passed to jbg_strerror()";
  if (language < 0 || language >= NEMSG_LANG)
    return "Unknown language code passed to jbg_strerror()";

  return errmsg[language][errnum];
}


/*
 * The constructor for a decoder 
 */
void jbg_dec_init(struct jbg_dec_state *s)
{
  s->order = 0;
  s->d = -1;
  s->bie_len = 0;
  s->buf_len = 0;
  s->dppriv = NULL;
  s->xmax = 4294967295U;
  s->ymax = 4294967295U;
  s->dmax = 256;

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
  struct dec_state *se;
  unsigned long x;
  int n;
  int pix, cx = 0, slntp, shift, tx;

  /* SDE loop variables */
  stripe = s->ii[index[s->order & 7][STRIPE]];
  layer = s->ii[index[s->order & 7][LAYER]];
  plane = s->ii[index[s->order & 7][PLANE]];

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
  hx = ceil_half(s->xd, s->d - layer);
  hy = ceil_half(s->yd, s->d - layer);
  /* number of pixels in lowres image */
  lx = ceil_half(hx, 1);
  ly = ceil_half(hy, 1);
  /* bytes per line in highres and lowres image */
  hbpl = (((hx-1) | 7) + 1) >> 3;
  lbpl = (((lx-1) | 7) + 1) >> 3;
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

  if (s->x == 0 && s->i == 0 &&
      (stripe == 0 || s->reset[plane][layer - s->dl])) {
    s->tx[plane][layer - s->dl] = s->ty[plane][layer - s->dl] = 0;
    if (s->pseudo)
      s->lntp = 1;
  }

#ifdef DEBUG
  if (s->x == 0 && s->i == 0 && s->pseudo)
    printf("decode_pscd(%p, %p, %ld): s/d/p = %2lu/%2u/%2u\n",
	   s, data, (long) len, stripe, layer, plane);
#endif

  if (layer == 0) {

    /*
     *  Decode lowest resolution layer
     */

    for (; s->i < hl && y < hy; s->i++, y++) {

      /* adaptive template changes */
      if (x == 0)
	for (n = 0; n < s->at_moves; n++)
	  if (s->at_line[n] == s->i) {
	    s->tx[plane][layer - s->dl] = s->at_tx[n];
	    s->ty[plane][layer - s->dl] = s->at_ty[n];
#ifdef DEBUG
	    printf("ATMOVE: line=%lu, tx=%d, ty=%d.\n", s->i,
		   s->tx[plane][layer - s->dl], s->ty[plane][layer - s->dl]);
#endif
	  }
      tx = s->tx[plane][layer - s->dl];
      shift =  tx - ((s->options & JBG_LRLTWO) ? 5 : 3);

      /* typical prediction */
      if (s->options & JBG_TPBON && s->pseudo) {
	slntp = arith_decode(se, (s->options & JBG_LRLTWO) ? TPB2CX : TPB3CX);
	if (se->result == MORE || se->result == MARKER)
	  goto leave;
	s->lntp = !(slntp ^ s->lntp);
	if (s->lntp) {
	  /* this line is 'not typical' and has to be coded completely */
	  s->pseudo = 0;
	} else {
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
      }
      
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
	  line_h2 = *(hp - hbpl) << 8;
	if (s->i > 1 || (y > 1 && !s->reset[plane][layer - s->dl]))
	  line_h3 = *(hp - hbpl - hbpl) << 8;
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
	    if (tx)
	      pix = arith_decode(se, (((line_h2 >> 9) & 0x3e0) |
				      ((line_h1 >> shift) & 0x010) |
				      (line_h1 & 0x00f)));
	    else
	      pix = arith_decode(se, (((line_h2 >> 9) & 0x3f0) |
				      (line_h1 & 0x00f)));
	    if (se->result == MORE || se->result == MARKER)
	      goto leave;
	    line_h1 = line_h1 << 1 | pix;  
	    line_h2 <<= 1;
	  } while ((++x & 7) && x < hx);
	} else {
	  /* three line template */
	  do {
	    if (tx) 
	      pix = arith_decode(se, (((line_h3 >>  7) & 0x380) |
				      ((line_h2 >> 11) & 0x078) |
				      ((line_h1 >> shift) & 0x004) |
				      (line_h1 & 0x003)));
	    else
	      pix = arith_decode(se, ((line_h3 >>  7 & 0x380) |
				      (line_h2 >> 11 & 0x07c) |
				      (line_h1 & 0x003)));
	    if (se->result == MORE || se->result == MARKER)
	      goto leave;
	    
	    line_h1 = line_h1 << 1 | pix;  
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

    for (; s->i < hl && y < hy; s->i++, y++) {

      /* adaptive template changes */
      if (x == 0)
	for (n = 0; n < s->at_moves; n++)
	  if (s->at_line[n] == s->i) {
	    s->tx[plane][layer - s->dl] = s->at_tx[n];
	    s->ty[plane][layer - s->dl] = s->at_ty[n];
#ifdef DEBUG
	    printf("ATMOVE: line=%lu, tx=%d, ty=%d.\n", s->i,
		   s->tx[plane][layer - s->dl], s->ty[plane][layer - s->dl]);
#endif
	  }
      tx = s->tx[plane][layer - s->dl];
      shift = tx - 3;

      /* handle lower border of low-resolution image */
      if (s->i >> 1 >= ll - 1 || y >> 1 >= ly - 1)
	lp1 = lp2;

      /* typical prediction */
      if (s->options & JBG_TPDON && s->pseudo) {
	s->lntp = arith_decode(se, TPDCX);
	if (se->result == MORE || se->result == MARKER)
	  goto leave;
	s->pseudo = 0;
      }


      /*
       * Layout of the variables line_h1, line_h2, line_h3, which contain
       * as bits the high resolution neighbour pixels of the currently
       * decoded highres pixel X:
       *
       *                     76543210 76543210 76543210 76543210     line_h3
       *                     76543210 76543210 76543210 76543210     line_h2
       *   76543210 76543210 76543210 76543210 X                     line_h1
       *
       * Layout of the variables line_l1, line_l2, line_l3, which contain
       * the low resolution pixels near the currently decoded pixel as bits.
       * The lowres pixel in which the currently coded highres pixel is
       * located is marked as Y:
       *
       *                     76543210 76543210 76543210 76543210     line_l3
       *                     76543210 76543210 Y6543210 76543210     line_l2
       *                     76543210 76543210 76543210 76543210     line_l1
       */
      

      if (x == 0) {
	line_h1 = line_h2 = line_h3 = line_l1 = line_l2 = line_l3 = 0;
	if (s->i > 0 || (y > 0 && !s->reset[plane][layer - s->dl])) {
	  line_h2 = *(hp - hbpl) << 8;
	  if (s->i > 1 || (y > 1 && !s->reset[plane][layer - s->dl]))
	    line_h3 = *(hp - hbpl - hbpl) << 8;
	}
	if ((s->i >> 1) > 0 || ((y >> 1) > 0 && !s->reset[plane][layer-s->dl]))
	  line_l3 = *(lp2 - lbpl) << 8;
	line_l2 = *lp2 << 8;
	line_l1 = *lp1 << 8;
      }
      
      /* decode line */
      while (x < hx) {
	if ((x & 15) == 0)
	  if ((x >> 1) < lbpl * 8 - 8) {
	    line_l1 |= *(lp1 + 1);
	    line_l2 |= *(lp2 + 1);
	    if ((s->i >> 1) > 0 || 
		((y >> 1) > 0 && !s->reset[plane][layer - s->dl]))
	      line_l3 |= *(lp2 - lbpl + 1);
	  }
	do {

	  assert(hp  - (s->lhp[ layer     &1][plane] + (stripe * hl + s->i)
			* hbpl) == x >> 3);
	  assert(lp2 - (s->lhp[(layer-1) &1][plane] + (stripe * ll + (s->i>>1))
			* lbpl) == x >> 4);

	  if ((x & 7) == 0)
	    if (x < hbpl * 8 - 8) {
	      if (s->i > 0 || (y > 0 && !s->reset[plane][layer - s->dl])) {
		line_h2 |= *(hp + 1 - hbpl);
		if (s->i > 1 || (y > 1 && !s->reset[plane][layer - s->dl]))
		  line_h3 |= *(hp + 1 - hbpl - hbpl);
	      }
	    }
	  do {
	    if (!s->lntp)
              cx = ((line_l3 >> 14 & 0x007) |
                    (line_l2 >> 11 & 0x038) |
                    (line_l1 >> 8  & 0x1c0));
	    if (!s->lntp && (cx == 0x000 || cx == 0x1ff)) {
	      /* pixels are typical and have not to be decoded */
	      do {
		line_h1 = line_h1 << 1 | (cx & 1);
	      } while ((++x & 1) && x < hx);
	      line_h2 <<= 2;  line_h3 <<= 2;
	    } else 
	      do {
		
		/* deterministic prediction */
		if (s->options & JBG_DPON)
		  if ((y & 1) == 0)
		    if ((x & 1) == 0) 
		      /* phase 0 */
		      pix = s->dppriv[(line_l3 >> 15 & 0x003) |
				      (line_l2 >> 13 & 0x00c) |
				      (line_h1 <<  4 & 0x010) |
				      (line_h2 >>  9 & 0x0e0)];
		    else
		      /* phase 1 */
		      pix = s->dppriv[((line_l3 >> 15 & 0x003) |
				       (line_l2 >> 13 & 0x00c) |
				       (line_h1 <<  4 & 0x030) |
				       (line_h2 >>  9 & 0x1c0)) + 256];
		  else
		    if ((x & 1) == 0)
		      /* phase 2 */
		      pix = s->dppriv[((line_l3 >> 15 & 0x003) |
				       (line_l2 >> 13 & 0x00c) |
				       (line_h1 <<  4 & 0x010) |
				       (line_h2 >>  9 & 0x0e0) |
				       (line_h3 >>  6 & 0x700)) + 768];
		    else
		      /* phase 3 */
		      pix = s->dppriv[((line_l3 >> 15 & 0x003) |
				       (line_l2 >> 13 & 0x00c) |
				       (line_h1 <<  4 & 0x030) |
				       (line_h2 >>  9 & 0x1c0) |
				       (line_h3 >>  6 & 0xe00)) + 2816];
		else
		  pix = 2;
		
		if (pix & 2) {
		  if (tx)
		    cx = ((line_h1       & 0x003) |
			  ((line_h1 << 2) >> shift & 0x010) |
			  (line_h2 >> 12 & 0x00c) |
			  (line_h3 >> 10 & 0x020));
		  else
		    cx = ((line_h1       & 0x003) |
			  (line_h2 >> 12 & 0x01c) |
			  (line_h3 >> 10 & 0x020));
		  if (x & 1) 
		    cx |= ((line_l2 >> 8  & 0x0c0) |
			   (line_l1 >> 6  & 0x300)) | (1 << 10);
		  else
		    cx |= ((line_l2 >> 9 & 0x0c0) |
			   (line_l1 >> 7 & 0x300));
		  cx |= (y & 1) << 11;
		  
		  pix = arith_decode(se, cx);
		  if (se->result == MORE || se->result == MARKER)
		    goto leave;
		}

		line_h1 = line_h1 << 1 | pix;
		line_h2 <<= 1;
		line_h3 <<= 1;
		
	      } while ((++x & 1) && x < hx);
	    line_l1 <<= 1; line_l2 <<= 1;  line_l3 <<= 1;
	  } while ((x & 7) && x < hx);
	  *hp++ = line_h1;
	} while ((x & 15) && x < hx);
	++lp1;
	++lp2;
      } /* while */
      x = 0;
      
      *(hp - 1) <<= hbpl * 8 - hx;
      if ((s->i & 1) == 0) {
	/* low resolution pixels are used twice */
	lp1 -= lbpl;
	lp2 -= lbpl;
      } else
	s->pseudo = 1;
      
    } /* for (i = ...) */
    
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
  long hsize, lsize;
  extern char dptable[];
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
    x = s->buffer[4]<<24 | s->buffer[5]<<16 | s->buffer[ 6]<<8 | s->buffer[ 7];
    y = s->buffer[8]<<24 | s->buffer[9]<<16 | s->buffer[10]<<8 | s->buffer[11];
    if (s->dl != 0 && (s->xd << (s->d - s->dl + 1) != x &&
		       s->yd << (s->d - s->dl + 1) != y))
      return JBG_ENOCONT;
    s->xd = x;
    s->yd = y;
    s->l0 = 
      s->buffer[12]<<24 | s->buffer[13]<<16 | s->buffer[14]<<8 | s->buffer[15];
    if (!s->planes || !s->xd || !s->yd || !s->l0)
      return JBG_EINVAL;
    s->mx = s->buffer[16];
    if (s->mx > 127)
      return JBG_EINVAL;
    s->my = s->buffer[17];
    if (s->mx > 32 || s->my > 0) 
      return JBG_EIMPL;
    s->order = s->buffer[18];
    if (index[s->order & 7][0] < 0)
      return JBG_EINVAL;
    /* HITOLO and SEQ currently not yet implemented */
    if (s->dl != s->d && (s->order & JBG_HITOLO || s->order & JBG_SEQ))
      return JBG_EIMPL;
    s->options = s->buffer[19];

    /* calculate number of stripes that will be required */
    s->stripes =
      ((s->yd >> s->d) + ((((1 << s->d) - 1) & s->xd) != 0) +
       s->l0 - 1) / s->l0;

    /* some initialization */
    s->ii[index[s->order & 7][STRIPE]] = 0;
    s->ii[index[s->order & 7][LAYER]] = s->dl;
    s->ii[index[s->order & 7][PLANE]] = 0;
    /* bytes required for resolution layer D and D-1 */
    hsize = ((((s->xd - 1) | 7) + 1) >> 3) * s->yd;
    lsize = ((((ceil_half(s->xd, 1) - 1) | 7) + 1) >> 3) * ceil_half(s->yd, 1);
    if (s->dl == 0) {
      s->s = checked_malloc(s->planes * sizeof(struct dec_state *));
      s->tx = checked_malloc(s->planes * sizeof(int *));
      s->ty = checked_malloc(s->planes * sizeof(int *));
      s->reset = checked_malloc(s->planes * sizeof(int *));
      s->lhp[0] = checked_malloc(s->planes * sizeof(unsigned char *));
      s->lhp[1] = checked_malloc(s->planes * sizeof(unsigned char *));
      for (i = 0; i < s->planes; i++) {
	s->s[i] = checked_malloc((s->d - s->dl + 1)*sizeof(struct dec_state));
	s->tx[i] = checked_malloc((s->d - s->dl + 1) * sizeof(int));
	s->ty[i] = checked_malloc((s->d - s->dl + 1) * sizeof(int));
	s->reset[i] = checked_malloc((s->d - s->dl + 1) * sizeof(int));
	s->lhp[s->d    &1][i] = checked_malloc(sizeof(unsigned char) * hsize);
	s->lhp[(s->d-1)&1][i] = checked_malloc(sizeof(unsigned char) * lsize);
      }
    } else {
      for (i = 0; i < s->planes; i++) {
	s->s[i] = checked_realloc(s->s[i],
				  (s->d - s->dl + 1)*sizeof(struct dec_state));
	s->tx[i] = checked_realloc(s->tx[i], (s->d - s->dl + 1) * sizeof(int));
	s->ty[i] = checked_realloc(s->ty[i], (s->d - s->dl + 1) * sizeof(int));
	s->reset[i] = checked_realloc(s->reset[i],
				      (s->d - s->dl +1) * sizeof(int));
	s->lhp[s->d    &1][i] = checked_realloc(s->lhp[s->d    & 1][i],
						sizeof(unsigned char) * hsize);
	s->lhp[(s->d-1)&1][i] = checked_realloc(s->lhp[(s->d-1)&1][i],
						sizeof(unsigned char) * lsize);
      }
    }
    for (i = 0; i < s->planes; i++)
      for (j = 0; j <= s->d - s->dl; j++)
	arith_decode_init(s->s[i] + j, 0);
    if (s->dl == 0 || (s->options & JBG_DPON && !(s->options & JBG_DPPRIV)))
      s->dppriv = dptable;
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
    if (!s->dppriv || s->dppriv == dptable)
      s->dppriv = checked_malloc(sizeof(char) * 1728);
    jbg_dppriv2int(s->dppriv, s->buffer);
  }

  /*
   * BID processing loop
   */
  
  while (*cnt < len) {

    /* process floating marker segments */

    /* skip COMMENT contents */
    if (s->comment_skip) {
      *cnt += s->comment_skip < len - *cnt ?
	s->comment_skip : len - *cnt;
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
	s->comment_skip =  (s->buffer[2] << 24 | s->buffer[3] << 16 |
			    s->buffer[4] <<  8 | s->buffer[5]);
	break;
      case MARKER_ATMOVE:
	if (s->at_moves < JBG_ATMOVES_MAX) {
	  s->at_line[s->at_moves] =  (s->buffer[2] << 24 | s->buffer[3] << 16 |
				      s->buffer[4] <<  8 | s->buffer[5]);
	  s->at_tx[s->at_moves] = (signed char) s->buffer[6];
	  s->at_ty[s->at_moves] = s->buffer[7];
	  if (s->at_tx[s->at_moves] < -s->mx ||
	      s->at_tx[s->at_moves] >  s->mx ||
	      s->at_ty[s->at_moves] >  s->my ||
	      (s->at_ty[s->at_moves] == 0 && s->at_tx[s->at_moves] < 0))
	    return JBG_EINVAL;
	  s->at_moves++;
	} else
	  return JBG_EINVAL;
	break;
      case MARKER_NEWLEN:
	y =  (s->buffer[2] << 24 | s->buffer[3] << 16 |
	      s->buffer[4] <<  8 | s->buffer[5]);
	if (y > s->yd || !(s->options & JBG_VLENGTH))
	  return JBG_EINVAL;
	/* calculate again number of stripes that will be required */
	s->stripes =
	  ((s->yd >> s->d) + ((((1 << s->d) - 1) & s->xd) != 0) +
	   s->l0 - 1) / s->l0;
	break;
      case MARKER_ABORT:
	return JBG_EABORT;
	
      case MARKER_SDNORM:
      case MARKER_SDRST:
	/* decode final pixels based on trailing zero bytes */
	decode_pscd(s, s->buffer, 2);

	arith_decode_init(s->s[s->ii[index[s->order & 7][PLANE]]] + 
			  s->ii[index[s->order & 7][LAYER]] - s->dl,
			  s->ii[index[s->order & 7][STRIPE]] != s->stripes - 1
			  && s->buffer[1] != MARKER_SDRST);
	
	s->reset[s->ii[index[s->order & 7][PLANE]]]
	  [s->ii[index[s->order & 7][LAYER]] - s->dl] =
	    (s->buffer[1] == MARKER_SDRST);
	
	/* prepare for next SDE */
	s->x = 0;
	s->i = 0;
	s->pseudo = 1;
	s->at_moves = 0;
	
	/* increment layer/stripe/plane loop variables */
	/* start and end value for each loop: */
	is[index[s->order & 7][STRIPE]] = 0;
	ie[index[s->order & 7][STRIPE]] = s->stripes - 1;
	is[index[s->order & 7][LAYER]] = s->dl;
	ie[index[s->order & 7][LAYER]] = s->d;
	is[index[s->order & 7][PLANE]] = 0;
	ie[index[s->order & 7][PLANE]] = s->planes - 1;
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
	  s->bie_len = 0;
	  return JBG_EOK;
	}

	/* check whether we have to abort because of xmax/ymax */
	if (index[s->order & 7][LAYER] == 0 && i < 0) {
	  /* LAYER is the outermost loop and we have just gone to next layer */
	  if (ceil_half(s->xd, s->d - s->ii[0]) > s->xmax ||
	      ceil_half(s->yd, s->d - s->ii[0]) > s->ymax) {
	    s->xmax = 4294967295U;
	    s->ymax = 4294967295U;
	    return JBG_EOK_INTR;
	  }
	  if (s->ii[0] > s->dmax) {
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
      if (*cnt < len && data[*cnt] != 0xff)
	return JBG_EINVAL;
      
    }
  }  /* of BID processing loop 'while (*cnt < len) ...' */

  return JBG_EAGAIN;
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call this
 * function in order to find out the width of the image.
 */
long jbg_dec_getwidth(struct jbg_dec_state *s)
{
  if (s->d < 0)
    return -1;
  if (index[s->order & 7][LAYER] == 0)
    if (s->ii[0] < 1)
      return -1;
    else
      return ceil_half(s->xd, s->d - (s->ii[0] - 1));
  
  return s->xd;
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call this
 * function in order to find out the hight of the image.
 */
long jbg_dec_gethight(struct jbg_dec_state *s)
{
  if (s->d < 0)
    return -1;
  if (index[s->order & 7][LAYER] == 0)
    if (s->ii[0] < 1)
      return -1;
    else
      return ceil_half(s->yd, s->d - (s->ii[0] - 1));
  
  return s->yd;
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call this
 * function in order to get a pointer to the image.
 */
unsigned char *jbg_dec_getimage(struct jbg_dec_state *s, int plane)
{
  if (s->d < 0)
    return NULL;
  if (index[s->order & 7][LAYER] == 0)
    if (s->ii[0] < 1)
      return NULL;
    else
      return s->lhp[(s->ii[0] - 1) & 1][plane];
  
  return s->lhp[s->d & 1][plane];
}


/*
 * After jbg_dec_in() returned JBG_EOK or JBG_EOK_INTR, you can call
 * this function in order to find out the size in bytes of one
 * bitplane of the image.
 */
long jbg_dec_getsize(struct jbg_dec_state *s)
{
  if (s->d < 0)
    return -1;
  if (index[s->order & 7][LAYER] == 0)
    if (s->ii[0] < 1)
      return -1;
    else
      return ((((ceil_half(s->xd, s->d - (s->ii[0] - 1)) - 1) | 7) + 1) >> 3) *
	ceil_half(s->yd, s->d - (s->ii[0] - 1));
  
  return ((((s->xd - 1) | 7) + 1) >> 3) * s->yd;
}


/* 
 * The destructor function which releases any resources obtained by the
 * other decoder functions.
 */
void jbg_dec_free(struct jbg_dec_state *s)
{
  int i;

  if (s->d < 0)
    return;
  s->d = -2;

  for (i = 0; i < s->planes; i++) {
    checked_free(s->s[i]);
    checked_free(s->tx[i]);
    checked_free(s->ty[i]);
    checked_free(s->reset[i]);
    checked_free(s->lhp[0][i]);
    checked_free(s->lhp[1][i]);
  }
  
  checked_free(s->s);
  checked_free(s->tx);
  checked_free(s->ty);
  checked_free(s->reset);
  checked_free(s->lhp[0]);
  checked_free(s->lhp[1]);

  return;
}
