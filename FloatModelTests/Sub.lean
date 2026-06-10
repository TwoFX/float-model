/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel.Float.Sub
import FloatModel.Float.Pack
meta import FloatModel.Sign
meta import FloatModel.Float.Basic
meta import FloatModel.Float.Round
meta import FloatModel.Float.Sub
meta import FloatModel.Float.Pack

open FloatModel

def spec : FloatModel.FloatSpec where
  mantissaBitsWithoutImplicit := 3
  hm := by decide
  exponentBits := 5
  he := by decide

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

def subSoft (x y : Float) : Float :=
  (UnpackedFloat.sub FloatSpec.binary64 (.ofFloat x) (.ofFloat y)).toFloat

def floats : Array Float := #[1, 0, 121.12341212, 0.123]

def check : Bool := Id.run do
  for a in floats do
    for b in floats do
      if a - b != subSoft a b then
        return false
  return true

#guard check
