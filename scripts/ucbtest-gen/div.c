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
 * Adapted from UCBTEST's ucbtest/div.c (ucbdivtest, by K.C. Ng, based on
 * W. Kahan's "Checking Whether Floating-Point Division is Correctly
 * Rounded"): instead of testing the host's division, this program emits
 * the generated binary64 cases as TestFloat-format vectors for
 * `testfloat-check f64_div`.
 *
 * For a random m-bit integer divisor Y, Kahan's recurrence (see the
 * mathematical background in the original div.c) yields integers X and Q
 * with
 *	(2Q +- 1) * Y - 2^m * X = +- R,   R minimal,
 * so that the exact quotient 2^(m-1) * X / Y = Q -+ (0.5 - R/(2Y)) misses
 * the halfway point between consecutive representable numbers as narrowly
 * as possible and must round to Q. The emitted expected quotient is
 * X/Y = Q / 2^(m-1), an exact power-of-two scaling. The recurrence uses
 * only exact additions and scalings, never the division under test.
 */

#include "ucbgen.h"

static GENERIC twonz, twohnz, twomm1; /* twomm1 = 2^(m-1) */
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
		r -= ns;
	else
		r -= ns - z;
	*result = (*x > ZERO) ? r : -r;
}

static int paritY(GENERIC *NN)
{
	GENERIC N, t;
	N = *NN;
	xfmod(&N, &twonz, &t);
	return t == twohnz;
}

static void tstdiv(GENERIC Y, GENERIC X, GENERIC Q)
{
	volatile GENERIC W;
	GENERIC q = Q / twomm1; /* exact: twomm1 is a power of two */

	W = X / Y;
	if (W != q)
		die("div %016" PRIX64 " / %016" PRIX64
		    ": host quotient %016" PRIX64 " disagrees with expected %016" PRIX64,
		    bits_of(X), bits_of(Y), bits_of(W), bits_of(q));
	emit2(X, Y, q);
	emitted++;
}

int main(int argc, char **argv)
{
	GENERIC R, Y, X, Q, Xm, Qm, t, twop, twokm1, two31 = 2147483648.0;
	UINT32 i, k, L, m;

	if (argc > 1)
		ntests = (UINT32)strtoul(argv[1], NULL, 0);
	L = ntests;

	m = significand_length;
	twomm1 = 1; for (i = 1; i <= m-1; i++) twomm1 += twomm1;

	while (L--) {
		/* generating random m-bit integer */
		Y = ZERO; i = m; twop = ONE;
		while (i >= 31) {
			Y += twop*((GENERIC)Rand());
			twop *= two31;
			i -= 31;
		}
		Y += twop*((GENERIC)((Rand() >> (31-i)) | (0x40000000L >> (31-i))));

		/* how many trailing zero does Y have ? */
		twonz = 2.0;
		xfmod(&Y, &twonz, &t);
		while (t == ZERO) { twonz += twonz; xfmod(&Y, &twonz, &t); }
		twohnz = 0.5*twonz;
		R = twohnz; /* set R to the minimum value */

		/* get Xm and Qm */
		Xm = (Y-R)*0.5; Qm = 0;
		twokm1 = 1.0;
		for (k = 1; k < m; k++) {
			if (paritY(&Xm) == 1) {
				Qm += twokm1;
				Xm = 0.5*(Xm+Y);
			} else
				Xm = 0.5*Xm;
			twokm1 += twokm1;
		}

		/* first try A: X = Y + Xm, Q = 2^(m-1) + Qm, if X is exact */
		X = Y + Xm;
		if ((X-Y)-Xm == ZERO) {
			Q = Qm + twokm1;
			tstdiv(Y, X, Q);
		}

		/* second try B: X = 2Y - Xm, Q = 2^m - Qm, if X is exact */
		X = (Y+Y)-Xm;
		if ((X-(Y+Y))+Xm == ZERO) {
			Q = (twokm1+twokm1) - Qm;
			tstdiv(Y, X, Q);
		}
	}

	fprintf(stderr, "ucbgen-div: wrote %u f64_div vectors\n", emitted);
	return 0;
}
