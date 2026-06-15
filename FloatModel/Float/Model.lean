/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Pack

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

/--
Predicate asserting that the bit representation of a float is valid according to the given
format. By 'valid' we mean that `NaN` is encoded using the canonical `NaN`.
-/
structure FloatSpec.Valid (spec : FloatSpec) (b : BitVec spec.numBits) : Prop where
  /-- If the bit vector encodes a `NaN`, then it is the canonical `NaN`. -/
  eq_packedNaN : unpackExponent b = (-1#_) → unpackMantissa b ≠ 0#_ → b = packedNaN spec

/-- The logical model for the `Float` type. -/
structure Float.Model where
  /-- The underlying bit pattern of the `Float`. -/
  toBits : UInt64
  /-- The underlying bit pattern is valid according to the format. -/
  valid : FloatSpec.binary64.Valid toBits.toBitVec

/-- The logical model for the `Float32` type. -/
structure Float32.Model where
  /-- The underlying bit pattern of the `Float32`. -/
  toBits : UInt32
  /-- The underlying bit pattern is valid according to the format. -/
  valid : FloatSpec.binary32.Valid toBits.toBitVec

end FloatModel
