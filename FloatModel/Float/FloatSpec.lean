/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

/--
A floating point format is specified by two pieces of information: the number
of bits in the mantissa, and the range of the exponent, which is represented by
the exponent that denotes infinity.
-/
structure FloatSpec where
  /-- The number of bits in the mantissa, including the implicit bit. -/
  mantissaBits : Nat
  /-- The exponent used to denote inifinities. -/
  infinityExponent : Nat

namespace FloatSpec

/-- Specification corresponding to the IEEE `binary64` format. -/
def binary64 : FloatSpec where
  mantissaBits := 53
  infinityExponent := 1024

/--
The smallest exponent possible for a number using the given specification,
including subnormals.
-/
def minExponent (spec : FloatSpec) : Int :=
  3 - spec.infinityExponent - spec.mantissaBits

/--
Suppose we have written a number where `totalExponent` is the position of the
most significant digit with the unit digit corresponds to `1`. So, for example,
`1.0b` has total exponent `1`, `0.1b` has total exponent `0`, `0.01` has total
exponent `-1`, and so on. This function computes which exponent that number
should have according to the given `FloatSpec`. So, for example, for the number
`0.1b` in `binary64` format, it wants us to use the exponent `-53`, corresponding
to the representation `2^52 * 2^(-53)`.
-/
def targetExponent (spec : FloatSpec) (totalExponent : Int) : Int :=
  max (totalExponent - spec.mantissaBits) spec.minExponent

#eval binary64.targetExponent 0

end FloatSpec

end FloatModel
