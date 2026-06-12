/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Valid

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

/-- The type of unpacked floats that are in the specified format. -/
structure ModelFloat (spec : FloatSpec) where
  /-- The underlying unpacked float. -/
  unpackedFloat : UnpackedFloat
  /-- The unpacked float is valid according to the specification. -/
  valid : spec.Valid unpackedFloat

/-- The logical model for the `Float` type. -/
structure Float.Model where
  /-- The logical model of the float. -/
  modelFloat : ModelFloat FloatSpec.binary64

/-- The logical model for the `Float32` type. -/
structure Float32.Model where
  /-- The logical model of the float. -/
  modelFloat : ModelFloat FloatSpec.binary32

end FloatModel
