/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel.Float.Sub
meta import FloatModel.Sign
meta import FloatModel.Float.Basic
meta import FloatModel.Float.Round
meta import FloatModel.Float.Sub

open FloatModel

def spec : FloatModel.FloatSpec where
  mantissaBits := 4
  infinityExponent := 32

#guard
  (UnpackedFloat.finite .positive 0b1000 2 (by decide)).sub
  spec
  (UnpackedFloat.finite .positive 0b1101 (-2) (by decide)) ==
  (UnpackedFloat.finite .positive 0b1110 1 (by decide))

#guard
  (UnpackedFloat.finite .positive 0b1000 2 (by decide)).sub
  spec
  (UnpackedFloat.finite .positive 0b1001 (-2) (by decide)) ==
  (UnpackedFloat.finite .positive 0b1111 1 (by decide))
