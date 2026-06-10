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

inductive UnpackedFloat where
  | infinity (sign : Sign) : UnpackedFloat
  | notANumber : UnpackedFloat
  | zero (sign : Sign) : UnpackedFloat
  | finite (sign : Sign) (mantissa : Nat) (exponent : Int) (mantissa_pos : 0 < mantissa) : UnpackedFloat

end FloatModel
