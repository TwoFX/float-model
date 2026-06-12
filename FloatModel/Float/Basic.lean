/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Sign

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

/--
An inductive type representing a floating-point number with constructors for signed infinity,
not-a-number without payload, signed zero, and finite floats with a sign, positive natural
mantissa and integral exponent.

Note that finite floats do not have a unique representation in this format: multiplying the
mantissa by two and decreasing the exponent by one yields a finite float that represents the
same rational number.
-/
inductive UnpackedFloat where
  /-- Signed infinity. -/
  | infinity (sign : Sign) : UnpackedFloat
  /-- Not a number. There is no payload attached to a NaN in this format. -/
  | notANumber : UnpackedFloat
  /-- Signed zero. -/
  | zero (sign : Sign) : UnpackedFloat
  /-- Finite floats consisting of a sign bit, a positive natural mantissa and an exponent. -/
  | finite (sign : Sign) (mantissa : Nat) (exponent : Int) (mantissa_pos : 0 < mantissa) : UnpackedFloat
deriving Repr, BEq

end FloatModel
