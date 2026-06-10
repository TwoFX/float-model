/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel.Float.Round
meta import FloatModel.Sign
meta import FloatModel.Float.Basic
meta import FloatModel.Float.Round

open FloatModel

def spec : FloatModel.FloatSpec where
  mantissaBits := 4
  infinityExponent := 32

#guard normalize spec 0b1110111 (-2) .positive == .finite .positive 15 1 (by decide)
#guard normalize spec 0b111001 (-1) .positive == .finite .positive 14 1 (by decide)
