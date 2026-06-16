/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel
meta import FloatModel

open Float.Model Float.Model.UnpackedFloat

def spec : Float.Model.Format where
  mantissaBitsWithoutImplicit := 3
  hm := by decide
  exponentBits := 5
  he := by decide

/-- info: true -/
#guard_msgs in
#eval
  (UnpackedFloat.finite .positive 0b1000 2 (by decide)).sub
  spec
  (UnpackedFloat.finite .positive 0b1101 (-2) (by decide)) ==
  (UnpackedFloat.finite .positive 0b1110 1 (by decide))

/-- info: true -/
#guard_msgs in
#eval
  (UnpackedFloat.finite .positive 0b1000 2 (by decide)).sub
  spec
  (UnpackedFloat.finite .positive 0b1001 (-2) (by decide)) ==
  (UnpackedFloat.finite .positive 0b1111 1 (by decide))

def subSoft (x y : Float) : Float :=
  (UnpackedFloat.sub Format.binary64 (.ofFloat x) (.ofFloat y)).toFloat

def floats : Array Float := #[
  1,
  0,
  121.12341212,
  0.123,
  Float.ofBits (UInt64.ofNat 0x403001fffffffeff),
  Float.ofBits (UInt64.ofNat 0x402ffffdfffffffe),
  Float.ofBits (UInt64.ofNat 0x0000000000000001),
  Float.ofBits (UInt64.ofNat 0x000fffffffffffff),
  Float.ofBits (UInt64.ofNat 0xffefffcfffffffff),
  Float.ofBits (UInt64.ofNat 0x7fe0000000000001)
]

def check : Bool := Id.run do
  for a in floats do
    for b in floats do
      if a - b != subSoft a b then
        return false
  return true

/-- info: true -/
#guard_msgs in
#eval check
