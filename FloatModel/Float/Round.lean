/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Basic
public import FloatModel.Float.FloatSpec
public import FloatModel.Sign

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

/--
Suppose given a real number `x : ℝ`. We can compute the binary expansion
`x = ± x₀.x₁x₂x₃…`. Truncating the expansion at some point gives an
approximation of `x` as a floating-point number `y`. We can then ask how `y`
is related to `x`. If `y` is exactly `x`, then we say that we are exact.
Otherwise we are inexact, and `y` is slightly smaller than `x`, but `x` is also
smaller than `y + 1ulp`. Together with the information that we are inexact,
we store the information of where `x` is located relative to `y + ½ulp`. This
is sufficient information for rounding `y` according to the various IEEE rounding
rules.
-/
inductive Accuracy where
  /--
  The approximation is exactly equal to the infinitely precise value.
  -/
  | exact
  /--
  The approximation is strictly smaller than the infinitely precise value,
  and the given `Ordering` describes the result of comparing the
  infinitely precise value and the approximation plus half a ulp.
  -/
  | inexact (relativeToPlusOneHalfUlp : Ordering)

namespace Accuracy

/--
Rounds the given mantissa with the given accuracy according to the
round-to-nearest strategy, with ties breaking in favor of even
mantissas.
-/
def roundToNearestEven (mantissa : Int) : Accuracy → Int
  | .exact => mantissa
  | .inexact .lt => mantissa
  | .inexact .eq => mantissa + mantissa.emod 2
  | .inexact .gt => mantissa + 1

end Accuracy

/--
Pairs a mantissa with two residual bits, the 'round bit' and the 'sticky bit',
which carry precisely the required information to compute the `Accuracy` of the
mantissa.
-/
structure ExtendedMantissa where
  /-- The mantissa. -/
  mantissa : Int
  /-- The next bit after the least significant bit of the mantissa. -/
  roundBit : Bool
  /-- The bitwise `or` of all bits that are even less significant than the round bit.  -/
  stickyBit : Bool

namespace ExtendedMantissa

/-- Extract the accuracy of the mantissa. -/
def accuracy : ExtendedMantissa → Accuracy
  | ⟨_, false, false⟩ => .exact
  | ⟨_, false, true⟩ => .inexact .lt
  | ⟨_, true, false⟩ => .inexact .eq
  | ⟨_, true, true⟩ => .inexact .gt

/--
Extracts the mantissa, rounded according to the data in the residual bits.
-/
def roundedMantissa (em : ExtendedMantissa) : Int :=
  em.accuracy.roundToNearestEven em.mantissa

/--
Transforms a mantissa and an accuracy into an extended mantissa with the
residual bits initialized to represent the given accuracy.
-/
def ofMantissaAndAccuracy (mantissa : Int) (accuracy : Accuracy) : ExtendedMantissa :=
  match accuracy with
  | .exact => ⟨mantissa, false, false⟩
  | .inexact .lt => ⟨mantissa, false, true⟩
  | .inexact .eq => ⟨mantissa, true, false⟩
  | .inexact .gt => ⟨mantissa, true, true⟩

/--
Shift the mantissa right by one, propagating information into the residual bits
as required.
-/
def shiftRightOne (em : ExtendedMantissa) : ExtendedMantissa where
  mantissa := em.mantissa.tdiv 2
  roundBit := em.mantissa.tmod 2 != 0
  stickyBit := em.roundBit || em.stickyBit

instance : HShiftRight ExtendedMantissa Nat ExtendedMantissa where
  hShiftRight em n := n.repeat shiftRightOne em

end ExtendedMantissa

/--
Computes the target exponent for the given mantissa and exponent and shifts the
mantissa and exponent and initial accuracy to the target exponent, returning
the new extended mantissa and exponent.
-/
def shiftToTargetExponent (spec : FloatSpec) (mantissa : Int) (exponent : Int)
    (accuracy : Accuracy) : ExtendedMantissa × Int :=
  let totalExponent := mantissa.natAbs.log2 + 1 + exponent
  let targetExponent := spec.targetExponent totalExponent
  let shiftAmount := (targetExponent - exponent).toNat -- negative to 0
  let initialExtendedMantissa := ExtendedMantissa.ofMantissaAndAccuracy mantissa accuracy
  (initialExtendedMantissa >>> shiftAmount, exponent + shiftAmount)

/--
Given a finite float represented by a sign, mantissa and exponent, together with an
`Accuracy` datum, round it to conform to the given `FloatSpec`.

Important: this function will only drop bits from the mantissa and increase the exponent,
not the other way around.
-/
def roundWithAccuracy (spec : FloatSpec) (sign : Sign) (mantissa : Int) (exponent : Int) (accuracy : Accuracy) : Float :=
  -- First shift: this performs the bulk of the shifting
  let (em₁, e₁) := shiftToTargetExponent spec mantissa exponent accuracy
  -- Round mantissa
  let roundedEm₁ := em₁.roundedMantissa
  -- Rounding the mantissa may have overflowed it, causing it to take too many bits, so we shift
  -- once more (no rounding is necessary after this):
  let (finalExtendedMantissa, finalExponent) := shiftToTargetExponent spec roundedEm₁ e₁ .exact
  let finalMantissa := finalExtendedMantissa.mantissa
  if finalMantissa = 0 then
    .zero sign
  else if h : 0 < finalMantissa then
    .finite sign finalMantissa.toNat finalExponent (by simpa)
  else
    .notANumber -- cannot happen??

end FloatModel
