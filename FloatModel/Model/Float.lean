/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

prelude
public import FloatModel.Model.Format.Valid
public import FloatModel.Model.Unpacked.Pack.Lemmas
public import FloatModel.Model.Unpacked.Operations

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

/-- The logical model for the `Float` type. -/
structure Float.Model where
  /-- The underlying bit pattern of the `Float`. -/
  toBits : UInt64
  /-- The underlying bit pattern is valid according to the format. -/
  valid : Float.Model.Format.binary64.Valid toBits.toBitVec

namespace Float.Model

/-- Unpack a `Float.Model` into the corresponding `UnpackedFloat`. -/
def unpack (f : Float.Model) : UnpackedFloat :=
  UnpackedFloat.unpack Format.binary64 f.toBits.toBitVec

/--
Pack an `UnpackedFloat` into the corresponding `Float.Model`.
This operation only gives a meaningful result if the float is
already correctly rounded for the `Format.binary64` format.
-/
def pack (f : UnpackedFloat) : Float.Model where
  toBits := UInt64.ofBitVec (UnpackedFloat.pack Format.binary64 f)
  valid := by simp

/--
Compute the sum of two `Float.Model`.
-/
def add (a b : Float.Model) : Float.Model :=
  pack (UnpackedFloat.add Format.binary64 a.unpack b.unpack)

/--
Construct a `Float.Model` from its bit representation. This operation canonicalizes
all `NaN` inputs into the canonical `NaN`.
-/
def ofBits (a : UInt64) : Float.Model :=
  pack (UnpackedFloat.unpack Format.binary64 a.toBitVec)

end Float.Model
