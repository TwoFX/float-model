/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Lemmas
public import FloatModel.Float.Operations

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

/-- Unpack a `Float.Model` into the corresponding `UnpackedFloat`. -/
def Float.Model.unpack (f : Float.Model) : UnpackedFloat :=
  FloatModel.unpack FloatSpec.binary64 f.toBits.toBitVec

/--
Pack an `UnpackedFloat` into the corresponding `Float.Model`.
This operation only gives a meaningful result if the float is
already correctly rounded for the `FloatSpec.binary64` format.
-/
def Float.Model.pack (f : UnpackedFloat) : Float.Model where
  toBits := UInt64.ofBitVec (FloatModel.pack FloatSpec.binary64 f)
  valid := by simp

/--
Compute the sum of two `Float.Model`.
-/
def Float.Model.add (a b : Float.Model) : Float.Model :=
  pack (UnpackedFloat.add FloatSpec.binary64 a.unpack b.unpack)

/--
Construct a `Float.Model` from its bit representation. This operation canonicalizes
all `NaN` inputs into the canonical `NaN`.
-/
def Float.Model.ofBits (a : UInt64) : Float.Model :=
  pack (FloatModel.unpack FloatSpec.binary64 a.toBitVec)

end FloatModel
