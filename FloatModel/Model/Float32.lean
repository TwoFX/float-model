/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Model.Format.Valid

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

/-- The logical model for the `Float32` type. -/
structure Float32.Model where
  /-- The underlying bit pattern of the `Float32`. -/
  toBits : UInt32
  /-- The underlying bit pattern is valid according to the format. -/
  valid : Float.Model.Format.binary32.Valid toBits.toBitVec
