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
 * Adapted from UCBTEST's ucbtest/mul.c (ucbmultest, by Bonnie Toy and
 * others, based on W. Kahan's "To Test Whether Binary Floating-Point
 * Multiplication is Correctly Rounded"): instead of testing the host's
 * multiplication, this program emits the generated binary64 cases as
 * TestFloat-format vectors for `testfloat-check f64_mul`.
 *
 * Two families of cases are generated, as in the original:
 *
 *  - halfway: products x*y that are exactly half an odd integer in the
 *    binade (2^52, 2^53), which must round to the nearest even integer.
 *    The expected product uu is derived from u = x*y - 1/2, computed
 *    EXACTLY as (x-1)*j + (x-1)/2 + j (all three terms representable),
 *    by the round-to-even rule: uu = u if u is even, else u+1.
 *
 *  - nearly halfway: products X[L]*Y[M] that come as close as possible to
 *    a halfway case without hitting one. The expected product H is the
 *    rounded sum of two exactly representable terms S and D, valid
 *    whenever the exact residue T = (S-H)+D satisfies 2|T| < ulp(H);
 *    cases failing that bound are skipped.
 *
 * In both families the expected result is obtained from additions and
 * exact operations only, never from the multiplication under test. The
 * round2() host-rounding probe of the original is dropped, and the
 * one-ulp-increment nextout() is replaced by nextafter from libm.
 */

#include "ucbgen.h"

static int bits;
static GENERIC two_to_bits, twobp1, twobm2;
static UINT32 halfway_emitted, nearhalf_emitted, nearhalf_skipped;

static GENERIC pow2n(int n)
{
	return ldexp(1.0, n);
}

/* computes nearest integral value of x, halfway rounded to even */
static GENERIC rintnearest(GENERIC x)
{
	GENERIC threshold, r;

	r = x;
	threshold = pow2n(significand_length - 1);
	if (0 < x && x < threshold) {
		r = x + threshold;
		r = r - threshold;
	}
	if (0 < -x && -x < threshold) {
		r = x - threshold;
		r = r + threshold;
	}
	return r;
}

/*  Based upon the Euclidean algorithm for a Greatest Common Divisor,
 *  this program starts from a given t = 2^(bits-3) and a given randomly
 *  chosen positive integer i < t, and yields a sequence of triples
 *  <g_n, j_n, k_n> that all satisfy g_n = i*j_n - 4t*k_n while each
 *  new |g_n| <= |g_n|/2 until at last |g_n| = GCD(i, 4t) = 1.
 */
static void gcd(GENERIC i, GENERIC t, GENERIC *j_ret, GENERIC *k_ret)
{
	GENERIC j, k;
	GENERIC g, g_0, g_1, j_0, j_1, k_0, k_1, m;

	/* initialize */
	g = FOUR*t; g_1 = i; j = ZERO; k_1 = ZERO; j_1 = ONE; k = -ONE;

	/* find factors */
	do {
		g_0 = g; g = g_1; j_0 = j; j = j_1; k_0 = k; k = k_1;
		m = rintnearest(g_0/g);
		g_1 = g_0 - m*g; j_1 = j_0 - m*j; k_1 = k_0 - m*k;
	} while (g_1 != 0);

	/* adjust factors if nec. */
	if (g < ZERO) { g = -g; j = -j; k = -k; }
	if (j < ZERO) { j = FOUR*t + j; k = k + i; }

	*j_ret = j; *k_ret = k;
}

/*	Every product x*y with y = j + 1/2 turns out to be half an odd
 *	integer in a binade where it must round to the nearest even integer
 *	for IEEE 754. To compute that expected product, calculate
 *		u := (x-1)*j + (x-1)/2 + j
 *	exactly and round x*y = u + 1/2 to even: uu = u if u is even,
 *	otherwise u+1. The rounded sum [x*j + x/2] of two exactly
 *	computable terms must agree (the original's consistency test).
 */
static void test_half(GENERIC j, GENERIC x)
{
	GENERIC y, u, uu, xj;
	volatile GENERIC xy;

	y = j + 0.5;
	xj = x*j + x/2;
	u = (x-1.0)*j + (x-1.0)/2.0 + j;
	uu = (fmod(u, 2.0) == 0.0) ? u : u + 1.0;
	if (xj != uu)
		die("mul halfway %016" PRIX64 " * %016" PRIX64
		    ": rounded sum %016" PRIX64 " disagrees with expected %016" PRIX64,
		    bits_of(x), bits_of(y), bits_of(xj), bits_of(uu));
	xy = x * y;
	if (xy != uu)
		die("mul halfway %016" PRIX64 " * %016" PRIX64
		    ": host product %016" PRIX64 " disagrees with expected %016" PRIX64,
		    bits_of(x), bits_of(y), bits_of(xy), bits_of(uu));
	emit2(x, y, uu);
	halfway_emitted++;
}

/* This method generates products x*y that are all half-odd-integers
 *  in the binade 2^(bits-1) < x*y < 2^bits, for which [x*y] should
 *  round to the nearest even integer for IEEE 754.
 */
static void halfway(void)
{
	GENERIC jl, ju, j, x;
	volatile GENERIC temp;
	UINT32 i;

	/* Generate a random odd integer in the interval (2, 2^bits) */
	x = ZERO;
	while ((x < TWO || x > twobm2) || fmod(x, 2.0) == 0.0)
		x = Rand();

	/*	Compute in floating-point two integers
	 *		jl := ceil((2^pi - (x-1))/(2x))
	 *		ju := floor((2^(pi+1) - (x+1))/(2x)).
	 *	Each quotient can be computed with only one rounding error
	 *	which, ideally, should be directed upward for jl, downward
	 *	for ju.
	 */
	round_positive();
	temp = x - ONE;
	jl = ceil((two_to_bits - temp)/(TWO*x));
	round_zero();
	temp = x + ONE;
	ju = floor((twobp1 - temp)/(TWO*x));
	round_nearest();

	/*	Then choose at random any integers j between jl and ju, as
	 *	well as jl and ju, from which to construct test arguments
	 *	y := j + 1/2 representable exactly in floating-point.
	 */
	test_half(jl, x);
	test_half(ju, x);
	if (ju - jl < ntests)
		for (j = jl + 1; j < ju; j++)
			test_half(j, x);
	else
		for (i = 2; i < ntests; i++) {
			j = jl + 1.0 + floor(Rand() * (ju - jl) / (0x7fffffffL + 1.0));
			test_half(j, x);
		}
}

/* See the original mul.c for the full derivation: from the trio (i, j, k)
 * with 1 = gcd(i, 4t) = i*j - 4t*k, sixteen products X[L]*Y[M] of odd
 * integers are constructed whose exact values differ from a multiple of 2t
 * by +-1, i.e. miss a halfway case as narrowly as possible. Each product
 * is expressed as a sum of two exactly computable terms,
 *	X[L]*Y[M] = H[L][M] + T[L][M],
 * so that H is the expected rounded product whenever 2|T| < ulp(H).
 */
static void test_nearhalf(GENERIC i, GENERIC t)
{
	GENERIC j, k, ii[4], jj[4], kk[4][4], X[4], Y[4];
	GENERIC ell[4][4], lambda_1[4][4], lambda_2[4][4];
	GENERIC S[4][4], D[4][4], H[4][4], T[4][4];
	GENERIC sum, ulp_of_Hlm, two_Tlm;
	GENERIC k_temp, ij_temp, temp;
	volatile GENERIC prod;
	int d, L, M;

	gcd(i, t, &j, &k);

	/* delta := sign(2t - j) = (+|-)1 according as 2t (>|<) j */
	d = (2*t > j) ? 1 : -1;

	ii[0] = i;
	X[0] = 4*t + ii[0];   jj[0] = j;          Y[0] = 4*t + jj[0];
	ii[1] = 2*t + i;
	X[1] = 4*t + ii[1];   jj[1] = 2*d*t + j;  Y[1] = 4*t + jj[1];
	ii[2] = 4*t - ii[0];
	X[2] = 4*t + ii[2];   jj[2] = 4*t - jj[0]; Y[2] = 4*t + jj[2];
	ii[3] = 4*t - ii[1];
	X[3] = 4*t + ii[3];   jj[3] = 4*t - jj[1]; Y[3] = 4*t + jj[3];
	kk[0][0] = k;
	kk[0][1] = k + i*d/2.0;
	kk[1][0] = k + j/2.0;
	k_temp = kk[0][1] + kk[1][0];
	kk[1][1] = k_temp - k + d*t;

	for (L = 0; L < 2; L++) {
		for (M = 0; M < 2; M++) {
			kk[L][M+2] = ii[L] - kk[L][M];
			kk[L+2][M] = jj[M] - kk[L][M];
			ij_temp = ii[L] + jj[M];
			kk[L+2][M+2] = 4*t - ij_temp + kk[L][M];
		}
	}
	for (L = 0; L < 4; L++)
		for (M = 0; M < 4; M++)
			ell[L][M] = (L - 1.5)*(M - 1.5) < 0 ? -1 : 1;

	for (L = 0; L < 4; L++) {
		for (M = 0; M < 4; M++) {
			temp = floor(kk[L][M]/2.0);
			lambda_1[L][M] = kk[L][M] - 2.0*temp;
			lambda_2[L][M] = kk[L][M] - lambda_1[L][M];
			sum = ii[L] + jj[M];
			sum = 4*t + sum + lambda_2[L][M];
			S[L][M] = 4*t*sum;
			D[L][M] = 4*t*lambda_1[L][M] + ell[L][M];
			H[L][M] = S[L][M] + D[L][M];
			T[L][M] = (S[L][M] - H[L][M]) + D[L][M];
		}
	}
	for (L = 0; L < 4; L++) {
		for (M = 0; M < 4; M++) {
			ulp_of_Hlm = nextafter(H[L][M], INFINITY) - H[L][M];
			two_Tlm = 2.0 * fabs(T[L][M]);
			if (two_Tlm >= ulp_of_Hlm) {
				/* H is not provably the correctly rounded
				 * product; not a usable test vector */
				nearhalf_skipped++;
				continue;
			}
			prod = X[L] * Y[M];
			if (prod != H[L][M])
				die("mul nearhalf %016" PRIX64 " * %016" PRIX64
				    ": host product %016" PRIX64
				    " disagrees with expected %016" PRIX64,
				    bits_of(X[L]), bits_of(Y[M]),
				    bits_of(prod), bits_of(H[L][M]));
			emit2(X[L], Y[M], H[L][M]);
			nearhalf_emitted++;
		}
	}
}

/*  Generate odd integers X and Y at random, in the binade between
 *   2^(pi-1) and 2^pi, whose products come as close as possible to
 *   half-way cases without hitting one.
 */
static void nearhalf(void)
{
	GENERIC t, i;
	UINT32 n;

	/*	Abbreviate t := 2^(pi-3), so that all integers between
	 *	(+|-)8t are representable exactly in floating-point. The
	 *	first step is to choose at random an odd integer i in the
	 *	interval 0 < i < t; i := 1, i := 3, i := t-1 and i := t-3
	 *	are good choices too.
	 */
	t = pow2n(bits - 3);
	test_nearhalf(1.0, t);
	test_nearhalf(3.0, t);
	test_nearhalf(t - 1.0, t);
	test_nearhalf(t - 3.0, t);
	for (n = 4; n < ntests; n++) {
		i = fmod((GENERIC)Rand(), t);            /* i < t  */
		i = (fmod(i, 2.0) == ONE) ? i : i + ONE; /* odd(i) */
		test_nearhalf(i, t);
	}
}

int main(int argc, char **argv)
{
	if (argc > 1)
		ntests = (UINT32)strtoul(argv[1], NULL, 0);

	bits = significand_length;
	two_to_bits = pow2n(bits);
	twobp1 = pow2n(bits + 1);
	twobm2 = pow2n(bits - 2);

	halfway();
	nearhalf();

	fprintf(stderr, "ucbgen-mul: wrote %u f64_mul vectors"
	        " (%u halfway, %u nearly halfway; %u cases skipped)\n",
	        halfway_emitted + nearhalf_emitted,
	        halfway_emitted, nearhalf_emitted, nearhalf_skipped);
	return 0;
}
