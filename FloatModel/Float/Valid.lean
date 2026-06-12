/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Basic
public import FloatModel.Float.FloatSpec

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

/--
Predicate asserting that a `(mantissa, exponent)` pair has the correct format for the given
`FloatSpec`.
-/
structure FloatSpec.ValidMantissaExponent (spec : FloatSpec) (mantissa : Nat) (exponent : Int) : Prop where
  /--
  The exponent has the correct exponent for the total exponent of the `(mantissa, exponent)` pair.
  This ensures that the mantissa always takes the right number of bits, depending on whether the
  number is normal or subnormal.
  -/
  eq_targetExponent : exponent = spec.targetExponent (totalExponent mantissa exponent)
  /--
  The exponent does not overflow the available bits.
  -/
  le_sub : exponent ≤ spec.infinityExponent - spec.mantissaBits

/--
Predicate asserting that an `UnpackedFloat` is in the correct format for the given `FloatSpec`.
-/
inductive FloatSpec.Valid (spec : FloatSpec) : UnpackedFloat → Prop where
  | infinity s : Valid spec (.infinity s)
  | notANumber : Valid spec .notANumber
  | zero s : Valid spec (.zero s)
  | finite s m e hm : spec.ValidMantissaExponent m e → Valid spec (.finite s m e hm)

attribute [simp] FloatSpec.Valid.infinity FloatSpec.Valid.notANumber FloatSpec.Valid.zero

end FloatModel
