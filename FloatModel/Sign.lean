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
Inductive with two constructors `negative` and `positive` for representing sign bits in
floating-point models.
-/
inductive Sign where
  | negative : Sign
  | positive : Sign

end FloatModel
