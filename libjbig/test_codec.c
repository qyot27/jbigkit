/*
 *  A sequence of test procedures for this JBIG implementation
 * 
 *  Run this test sequence after each modification on the JBIG library.
 *
 *  Markus Kuhn -- mskuhn@cip.informatik.uni-erlangen.de
 *
 *  $Id: tstcodec.c,v 1.2 1995-06-08 16:43:07 mskuhn Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "jbig.h"

#define TESTBUF1_SIZE 500000
#define TESTBUF2_SIZE 500000

unsigned char testbuf1[TESTBUF1_SIZE];
unsigned char testbuf2[TESTBUF1_SIZE];
unsigned char testpic[477995];

long testbuf1_len, testbuf2_len;


void testbuf1_write(int v, void *dummy)
{
  if (testbuf1_len + 1 < TESTBUF1_SIZE)
    testbuf1[testbuf1_len++] = v;
  return;
}


void testbuf1_writel(unsigned char *start, size_t len, void *dummy)
{
  if (testbuf1_len + len < TESTBUF1_SIZE)
    memcpy(testbuf1 + testbuf1_len, start, len);
  testbuf1_len += len;
  return;
}


/*
 * Store the artificial test image defined in T.82, clause 7.2.1 at
 * pic. The image requires 477995 bytes of memory, is 1960x1951 pixels
 * large and has one plane.
 */ 
void testimage(unsigned char *pic)
{
  unsigned long i, j, sum;
  unsigned int prsg, repeat[8];
  unsigned char *p;
  
  memset(pic, 0, 477995);
  p = pic;
  prsg = 1;
  for (j = 0; j < 1951; j++)
    for (i = 0; i < 1960; i++) {
      if (j >= 192) {
	if (j < 1023 || ((i >> 3) & 3) == 0) {
	  sum = (prsg & 1) + ((prsg >> 2) & 1) + ((prsg >> 11) & 1) +
	    ((prsg >> 15) & 1);
	  prsg = (prsg << 1) + (sum & 1);
	  if ((prsg & 3) == 0) {
	    *p |= 1 << (7 - (i & 7));
	    repeat[i & 7] = 1;
	  } else {
	    repeat[i & 7] = 0;
	  }
	} else {
	  if (repeat[i & 7])
	    *p |= 1 << (7 - (i & 7));
	}
      }
      if ((i & 7) == 7) ++p;
    }

  /* verify test image */
  sum = 0;
  for (i = 0; i < 477995; i++)
    for (j = 0; j < 8; j++)
      sum += (pic[i] >> j) & 1;
  if (sum != 861965)
    printf("WARNING: Artificial test image has %lu (not 861965) "
	   "foreground pixels!\n", sum);
  
#if 0
  {
    FILE *f;


    for (i = 0; i < 477995; i++) {
      sum = 0;
      for (j = 0; j < 8; j++)
	sum |= ((pic[i] >> j) & 1) << (7-j);
      pic[i] = sum;
    }

    f = fopen("t82demo.lit", "wb");
    fwrite(pic, 1, 477995, f);
    fclose(f);
  }
#endif
  
  return;
}
  

/*
 * Perform a full test cycle with one set of parameters. Encode an image
 * and compare the length of the result with correct_length. Then decode
 * the image again both in one single chunk or byte by byte and compare
 * the results with the original input image.
 */
int test_cycle(unsigned char **orig_image, int width, int height, int options,
	       int order, int layers, int planes, int l0, int mx,
	       long correct_length)
{
  struct jbg_enc_state sje;
  struct jbg_dec_state sjd;
  int trouble = 0;
  long l;
  size_t plane_size;
  int i, result;
  unsigned char **image;

  plane_size = ((((width - 1L) | 7) + 1) >> 3) * height;
  image = malloc(planes * sizeof(unsigned char *));
  if (!image)
    printf("Sorry, not enough memory available!\n");
  for (i = 0; i < planes; i++) {
    image[i] = malloc(plane_size);
    if (!image[i])
      printf("Sorry, not enough memory available!\n");
    memcpy(image[i], orig_image[i], plane_size);
  }

  puts("\nTesting encoder ...");
  testbuf1_len = 0;
  jbg_enc_init(&sje, width, height, planes, image, testbuf1_writel, NULL);
  jbg_enc_layers(&sje, layers);
  jbg_enc_options(&sje, order, options, l0, mx, 0);
  jbg_enc_out(&sje);
  jbg_enc_free(&sje);
  printf("Encoded BIE has %6ld bytes: ", testbuf1_len);
  if (testbuf1_len == correct_length)
    puts("PASSED");
  else {
    trouble++;
    printf("FAILED, correct would have been %ld\n", correct_length);
  }

  puts("Testing decoder with whole chunk ...");
  jbg_dec_init(&sjd);
  result = jbg_dec_in(&sjd, testbuf1, testbuf1_len, NULL);
  if (result != JBG_EOK) {
    printf("Decoder complained with return value %d: FAILED\n"
	   "Cause: '%s'\n", result, jbg_strerror(result, JBG_EN));
    trouble++;
  }
  if (result == JBG_EOK) {
    printf("Image comparison: ");
    result = 1;
    for (i = 0; i < planes && result; i++) {
      if (memcmp(orig_image[i], sjd.lhp[layers & 1][i],
		 ((((width - 1L) | 7) + 1) >> 3) * height)) {
	result = 0;
	trouble++;
	printf("FAILED for plane %d\n", i);
      }
    }
    if (result)
      puts("PASSED");
  }
  jbg_dec_free(&sjd);

  puts("Testing decoder with single byte feed ...");
  jbg_dec_init(&sjd);
  result = JBG_EAGAIN;
  for (l = 0; l < testbuf1_len; l++) {
    result = jbg_dec_in(&sjd, testbuf1 + l, 1, NULL);
    if (l < testbuf1_len - 1 && result != JBG_EAGAIN) {
      printf("Decoder complained with return value %d at byte %ld: FAILED\n"
	     "Cause: '%s'\n", result, l, jbg_strerror(result, JBG_EN));
      trouble++;
      break;
    }
  }
  if (l == testbuf1_len && result != JBG_EOK) {
    printf("Decoder complained with return value %d at final byte: FAILED\n"
	   "Cause: '%s'\n", result, jbg_strerror(result, JBG_EN));
    trouble++;
  }

  if (result == JBG_EOK) {
    printf("Image comparison: ");
    result = 1;
    for (i = 0; i < planes && result; i++) {
      if (memcmp(orig_image[i], sjd.lhp[layers & 1][i],
		 ((((width - 1L) | 7) + 1) >> 3) * height)) {
	result = 0;
	trouble++;
	printf("FAILED for plane %d\n", i);
      }
    }
    if (result)
      puts("PASSED");
  }

  jbg_dec_free(&sjd);
  puts("");

  return trouble != 0;
}


int main()
{
  int trouble, problems = 0;
  struct enc_state se;
  struct dec_state sd;
  long i;
  int pix;
  size_t st;
  unsigned char *pp;

  int t82pix[16] = {
    0x05e0, 0x0000, 0x8b00, 0x01c4, 0x1700, 0x0034, 0x7fff, 0x1a3f,
    0x951b, 0x05d8, 0x1d17, 0xe770, 0x0000, 0x0000, 0x0656, 0x0e6a
  };
  int t82cx[16] = {
    0x0fe0, 0x0000, 0x0f00, 0x00f0, 0xff00, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000
  };
  unsigned char t82sde[32] = {
    0x69, 0x89, 0x99, 0x5c, 0x32, 0xea, 0xfa, 0xa0,
    0xd5, 0xff, 0x00, 0x52, 0x7f, 0xff, 0x00, 0xff,
    0x00, 0xff, 0x00, 0xc0, 0x00, 0x00, 0x00, 0x3f,
    0xff, 0x00, 0x2d, 0x20, 0x82, 0x91, 0xff, 0x02
  };
  
  
  printf("Automatic JBIG Compatibility Test Suite\n"
	 "---------------------------------------\n\n"
	 "This test will take a few minutes.\n\n");
  
#if 1
  /* test a few properties of the machine architecture */
  testbuf1[0] = 42;
  testbuf1[0x10000L] = 0x42;
  st = 1 << 16;
  testbuf1[st]++;
  pp = testbuf1 + 0x4000;
  pp += 0x4000;
  pp += 0x4000;
  pp += 0x4000;
  if (testbuf1[0] == 43 || *pp != 0x43)
    printf("WARNING: (char *) x + (size_t) (1 << 16) == (char *) x !!!\n\n"
	   "Pointer arithmetic with this compiler is not at least 32-bit.\n"
	   "Are you sure, you have not compiled this program on a 8-bit\n"
	   "or 16-bit architecture? This compiler mode can obviously not\n"
           "handle arrays with a size of more than 65536 bytes. :-(\n\n");
#endif

#if 1
  puts("1) Arithmetic encoder test sequence from ITU-T T.82, clause 7.1\n"
       "---------------------------------------------------------------\n");
  arith_encode_init(&se, 0);
  testbuf1_len = 0;
  se.byte_out = testbuf1_write;
  for (i = 0; i < 16 * 16; i++)
    arith_encode(&se, (t82cx[i >> 4] >> ((15 - i) & 15)) & 1,
		 (t82pix[i >> 4] >> ((15 - i) & 15)) & 1);
  arith_encode_flush(&se);
  printf("result of encoder:\n  ");
  for (i = 0; i < testbuf1_len && i < TESTBUF1_SIZE; i++)
    printf("%02x", testbuf1[i]);
  printf("\nexpected result:\n  ");
  for (i = 0; i < 30; i++)
    printf("%02x", t82sde[i]);
  printf("\n\nTest result: ");
  if (testbuf1_len != 30 || memcmp(testbuf1, t82sde, 30)) {
    problems++;
    printf("FAILED");
  } else
    printf("PASSED");
  printf("\n\n");


  puts("2) Arithmetic decoder test sequence from ITU-T T.82, clause 7.1\n"
       "---------------------------------------------------------------\n");
  printf("Whole chunk test...\n");
  arith_decode_init(&sd, 0);
  sd.pscd_ptr = t82sde;
  sd.pscd_end = t82sde + 32;
  trouble = 0;
  for (i = 0; i < 16 * 16 && !trouble; i++) {
    pix = arith_decode(&sd, (t82cx[i >> 4] >> ((15 - i) & 15)) & 1);
    if (pix < 0) {
      printf("Problem at Pixel %ld, result code %d.\n\n", i+1, sd.result);
      trouble++;
      break;
    }
    if (pix != ((t82pix[i >> 4] >> ((15 - i) & 15)) & 1)) {
      printf("Wrong PIX answer at Pixel %ld.\n\n", i+1);
      trouble++;
      break;
    }
  }
  if (!trouble && sd.result != READY) {
    printf("Result is %d instead of READY.\n\n", sd.result);
    trouble++;
  }
  printf("Test result: ");
  if (trouble) {
    problems++;
    puts("FAILED");
  } else
    puts("PASSED");
  printf("\n");

  printf("Single byte feed test ...\n");
  arith_decode_init(&sd, 0);
  pp = t82sde;
  sd.pscd_ptr = pp;
  sd.pscd_end = pp + 1;
  trouble = 0;
  for (i = 0; i < 16 * 16 && !trouble; i++) {
    pix = arith_decode(&sd, (t82cx[i >> 4] >> ((15 - i) & 15)) & 1);
    while ((sd.result == MORE || sd.result == MARKER) &&
	   sd.pscd_end < t82sde + 32) {
      pp++;
      sd.pscd_end = pp + 1;
      if (sd.result == MARKER)
	sd.pscd_ptr = pp - 1;
      else
	sd.pscd_ptr = pp;
      pix = arith_decode(&sd, (t82cx[i >> 4] >> ((15 - i) & 15)) & 1);
    }
    if (pix < 0) {
      printf("Problem at Pixel %ld, result code %d.\n\n", i+1, sd.result);
      trouble++;
      break;
    }
    if (pix != ((t82pix[i >> 4] >> ((15 - i) & 15)) & 1)) {
      printf("Wrong PIX answer at Pixel %ld.\n\n", i+1);
      trouble++;
      break;
    }
  }
  if (!trouble && sd.result != READY) {
    printf("Result is %d instead of READY.\n\n", sd.result);
    trouble++;
  }
  printf("Test result: ");
  if (trouble) {
    problems++;
    puts("FAILED");
  } else
    puts("PASSED");
  printf("\n");
#endif
  
  puts("3) Parametric algorithm test sequence from ITU-T T.82, clause 7.2\n"
       "-----------------------------------------------------------------\n");
  puts("Generating test image ...");
  testimage(testpic);
  putchar('\n');
  pp = testpic;

  puts("Test 1: TPBON=0, Mx=0, LRLTWO=0, L0=1951");
  problems += test_cycle(&pp, 1960, 1951, JBG_DELAY_AT,
			 0, 0, 1, 1951, 0, 317384);
  puts("Test 2: TPBON=0, Mx=0, LRLTWO=1, L0=1951");
  problems += test_cycle(&pp, 1960, 1951, JBG_DELAY_AT | JBG_LRLTWO,
			 0, 0, 1, 1951, 0, 317132);
  puts("Test 3: TPBON=1, DPON=1, TPDON=1, Mx=8, LRLTWO=0, L0=128");
  problems += test_cycle(&pp, 1960, 1951, JBG_DELAY_AT | JBG_TPBON,
			 0, 0, 1, 128, 8, 253653);
  puts("Test 4: TPBON=1, DPON=1, TPDON=1, Mx=8, LRLTWO=0, L0=2, 6 layers");
  problems += test_cycle(&pp, 1960, 1951,
			 JBG_DELAY_AT | JBG_TPBON | JBG_TPDON | JBG_DPON,
			 0, 6, 1, 2, 8, 279314);
#if 0
  puts("Test 5: as TEST 4 but with order bit SEQ set");
  problems += test_cycle(&pp, 1960, 1951,
			 JBG_DELAY_AT | JBG_TPBON | JBG_TPDON | JBG_DPON,
			 JBG_SEQ, 6, 1, 2, 8, 279314);
#endif

  printf("\nTest result summary: the library has %s the test suite.\n",
	 problems ? "FAILED" : "PASSED");
  if (!problems)
    puts("Congratulations, everything is fine.\n");

  return problems != 0;
}
