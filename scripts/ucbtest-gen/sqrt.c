/*
Copyright (C) 1988-1994 Sun Microsystems, Inc. 2550 Garcia Avenue
Mountain View, California  94043 All rights reserved.

Any person is hereby authorized to download, copy, use, create bug fixes,
and distribute, subject to the following conditions:

	1.  the software may not be redistributed for a fee except as
	    reasonable to cover media costs;
	2.  any copy of the software must include this notice, as well as
	    any other embedded copyright notices; and
	3.  any distribution of this software or derivative works thereof
	    must comply with all applicable U.S. export control laws.

THE SOFTWARE IS MADE AVAILABLE "AS IS" AND WITHOUT EXPRESS OR IMPLIED
WARRANTY OF ANY KIND, INCLUDING BUT NOT LIMITED TO THE IMPLIED
WARRANTIES OF DESIGN, MERCHANTIBILITY, FITNESS FOR A PARTICULAR
PURPOSE, NON-INFRINGEMENT, PERFORMANCE OR CONFORMANCE TO
SPECIFICATIONS.

BY DOWNLOADING AND/OR USING THIS SOFTWARE, THE USER WAIVES ALL CLAIMS
AGAINST SUN MICROSYSTEMS, INC. AND ITS AFFILIATED COMPANIES IN ANY
JURISDICTION, INCLUDING BUT NOT LIMITED TO CLAIMS FOR DAMAGES OR
EQUITABLE RELIEF BASED ON LOSS OF DATA, AND SPECIFICALLY WAIVES EVEN
UNKNOWN OR UNANTICIPATED CLAIMS OR LOSSES, PRESENT AND FUTURE.

IN NO EVENT WILL SUN MICROSYSTEMS, INC. OR ANY OF ITS AFFILIATED
COMPANIES BE LIABLE FOR ANY LOST REVENUE OR PROFITS OR OTHER SPECIAL,
INDIRECT AND CONSEQUENTIAL DAMAGES, EVEN IF IT HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGES.

This file is provided with no support and without any obligation on the
part of Sun Microsystems, Inc. ("Sun") or any of its affiliated
companies to assist in its use, correction, modification or
enhancement.  Nevertheless, and without creating any obligation on its
part, Sun welcomes your comments concerning the software and requests
that they be sent to fdlibm-comments@sunpro.sun.com.
*/

/*
 * Adapted from UCBTEST's ucbtest/sqr.c (ucbsqrtest, by K.C. Ng, based on
 * notes of W. Kahan): instead of testing the host's square root, this
 * program emits the generated binary64 cases as TestFloat-format vectors
 * for `testfloat-check f64_sqrt`.
 *
 * For small integers C = 1 mod 8, a recurrence solving Z^2 = C mod 2^k
 * (see the mathematical background in the original sqr.c) yields exactly
 * representable integral arguments X with
 *	sqrt(X) = (N + 0.5) - eps   or   sqrt(X) = (N - 0.5) + eps
 * for an m-bit integer N, i.e. square roots that miss the halfway point
 * between consecutive representable numbers as narrowly as possible and
 * must round to N. The expected root N comes out of the recurrence, which
 * uses only exact additions and scalings, never the sqrt under test.
 */

#include "ucbgen.h"

#define SQRT2M1   0.414213562373095048801688724209698 /* sqrt(2)-1 */
#define TWOMSQRT2 0.585786437626904951198311275790302 /* 2-sqrt(2) */

#define sgn(a) ((a) >= ZERO ? ONE : -ONE)

static GENERIC twohM,   /* for 2^(m/2) */
	twoM,           /* for 2^m */
	twomm1;
static UINT32 emitted;

/*
 * xfmod return result = fmod(x,y) where y is a pow of 2.
 */
static void xfmod(GENERIC *x, GENERIC *y, GENERIC *result)
{
	GENERIC r, z, s, ns;
	r = (*x >= ZERO) ? *x : -(*x);
	z = (*y >= ZERO) ? *y : -(*y);

	/* the following algorithm takes the advantage of z is a pow of 2 */
	s = z * twomm1;
	ns = s + r;
	ns -= s;
	if (r >= ns)
		r = r - ns;
	else
		r = r - (ns - z);
	*result = (*x > ZERO) ? r : -r;
}

/*
 * eqmod check if (z=z(k-1))^2 = C mod 2^k ; note that z < 2^(k-3).
 */
static int eqmod(GENERIC *z, GENERIC *c, int k)
{
	GENERIC twok, two7, x, y, s, t, tt, tl, t1, t2, t3;
	int i;
	twok = 1; for (i = 1; i <= k; i++) twok += twok; /* twok = 2^k */

	/* small z */
	if (*z < twohM) {
		t = (*z)*(*z) - (*c);
		xfmod(&t, &twok, &s);
		return s == ZERO;
	}

	/* big z */
	two7 = (GENERIC)128;

	/* break z to x + y (with y to be m/2 bit). Note that x is also at
	 * most m/2 bit since k only go up to m+2 (hence z=z(m+1)<2^(m-1)
	 */
	xfmod(z, &twohM, &y);
	x = *z - y;

	/* Form t1 = x^2, t2 = 2xy, t3 = y^2; hence t1+t2+t3=z^2.
	 * Here, to sum t1,t2,t3, we chopped off their last 7 bits
	 * to guarantee the exactness of the sum. The "tail" will
	 * be sum independently.
	 */
	s = x*x;
	xfmod(&s, &twok, &t1);
	xfmod(&t1, &two7, &tt);
	t1 -= tt;	/* chopped off tail 7 bits of t1 */
	tl = tt;	/* sum the tail part in tl */
	s = (GENERIC)2*x*y;
	xfmod(&s, &twok, &t2);
	xfmod(&t2, &two7, &tt);
	t2 -= tt;	/* chopped off tail 7 bits of t2 */
	tl += tt;	/* sum the tail part in tl */
	s = y*y;
	xfmod(&s, &twok, &t3);
	xfmod(&t3, &two7, &tt);
	t3 -= tt;	/* chopped off tail 7 bits of t3 */
	tl += tt;	/* sum the tail part in tl */

	s = t1+t2+t3;
	xfmod(&s, &twok, &x);	/* now x = t1+t2+t3 */

	xfmod(c, &twok, &y);	/* y = c mod 2^k */
	xfmod(&y, &two7, &tt);	/* tt = lower bit of y */
	y -= tt;	/* chopped off tail 7 bits of c mod 2^k */
	tl -= tt;	/* subtract c's tail from tl */
			/* now tl = low order bit of z^2 - c mod 2^k */
	if (k > 7) {	/* if for large k lower bit of tl != 0, return 0 */
		xfmod(&tl, &two7, &s);
		if (s != ZERO) return 0;
	}
	s = x-y;
	xfmod(&s, &twok, &x);	/* x=high order bit of z^2 - c mod 2^k */
	s = x+tl;		/* x + tl should be exact since lower bit
				 * of tl has been checked */
	xfmod(&s, &twok, &x);
	return x == ZERO;
}

static void getZk(GENERIC *c, int m, GENERIC *z1, GENERIC *z2)
{
	GENERIC z, p;
	int k;
	z = 1; p = 4;
	for (k = 4; k <= m+1; k++) {
		if (eqmod(&z, c, k) != 1) z = p - z;
		p += p;
	}
	*z1 = z;
	if (eqmod(&z, c, m+2) != 1) z = p - z;
	*z2 = z;
}

static void tstsqrt(GENERIC X, GENERIC N)
{
	volatile GENERIC w;

	w = sqrt(X);
	if (w != N)
		die("sqrt(%016" PRIX64 "): host root %016" PRIX64
		    " disagrees with expected %016" PRIX64,
		    bits_of(X), bits_of(w), bits_of(N));
	emit1(X, N);
	emitted++;
}

int main(int argc, char **argv)
{
	GENERIC X, N, C, z1, z2, k1, k2;
	UINT32 m, i, L;
	INT32 j, j3;

	if (argc > 1)
		ntests = (UINT32)strtoul(argv[1], NULL, 0);
	L = ntests/4;
	m = significand_length;

	/* set twoM = 2^m, twohM = 2^(m/2 chopped) */
	twohM = 1; for (i = 1; i <= m/2; i++) twohM += twohM;
	twoM = 1; for (i = 1; i <= m; i++) twoM += twoM;
	twomm1 = twoM * 0.5;

	/* set k1 = (sqrt2-1)*2^m, k2 = (2-sqrt2)*2^m */
	k1 = SQRT2M1*twoM;
	k2 = TWOMSQRT2*twoM;

	/* generate C=1+8*j for j=0,+-1,+-2,... */
	j = 0;
	for (i = 0; i <= L; i++) {
		j3 = j << 3;
		C = ONE + (GENERIC)j3;
		if ((i & 1) == 0) j = 1 - j; else j = -j;
		if ((C - j3) - 1 != 0) { /* check for rounding error in C */
			fprintf(stderr, "ucbgen-sqrt: exhausted the binary64"
			        " format at i=%u\n", i);
			break;
		}
		getZk(&C, m, &z1, &z2);		/* z1=Z(m+1), z2=Z(m+2) */
		if (z1 < k1) {
			/* case 1: 2^m <= Z < 2^m * sqrt(2) */
			N = 0.5*twoM + 0.5*(z1 - sgn(C));
			X = N*(N + sgn(C)) + 0.25*(ONE - C);
			tstsqrt(X, N);
		}
		if (z2 < k2) {
			/* case 2: 2^m * sqrt(2) <= Z < 2^(m+1) */
			N = twoM - 0.5*(z2 + sgn(C));
			X = N*(N + sgn(C)) + 0.25*(ONE - C);
			tstsqrt(X, N);
		}
	}

	fprintf(stderr, "ucbgen-sqrt: wrote %u f64_sqrt vectors\n", emitted);
	return 0;
}
