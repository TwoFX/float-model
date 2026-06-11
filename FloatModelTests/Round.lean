/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel
meta import FloatModel

open FloatModel

def spec : FloatModel.FloatSpec where
  mantissaBitsWithoutImplicit := 3
  hm := by decide
  exponentBits := 5
  he := by decide

/-- info: true -/
#guard_msgs in
#eval normalize spec 0b1110111 (-2) .positive == .finite .positive 15 1 (by decide)

/-- info: true -/
#guard_msgs in
#eval normalize spec 0b111001 (-1) .positive == .finite .positive 14 1 (by decide)
