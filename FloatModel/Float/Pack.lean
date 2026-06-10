/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Basic
public import FloatModel.Float.FloatSpec
public import FloatModel.Float.Sub

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

def packComponents (spec : FloatSpec) (sign : Sign)
    (exponent : BitVec spec.exponentBits)
    (mantissa : BitVec spec.mantissaBitsWithoutImplicit) : BitVec spec.numBits :=
  sign.toBitVec ++ exponent ++ mantissa

def pack (spec : FloatSpec) : UnpackedFloat → BitVec spec.numBits
  | .notANumber => packComponents spec .positive (-1#_) (1#_ <<< (spec.mantissaBitsWithoutImplicit - 1))
  | .infinity s => packComponents spec s (-1#_) 0
  | .zero s => packComponents spec s 0 0
  | .finite s m e _ =>
    let biasedExponent := (e + spec.exponentBias).toNat -- negative to 0 (subnormal)
    -- Observe that the transformation of the mantissa clears the implicit bit in the normal case
    packComponents spec s (BitVec.ofNat _ biasedExponent) (BitVec.ofNat _ m)

@[simp]
theorem BitVec.toNat_pos {n : Nat} (b : BitVec n) : 0 < b.toNat ↔ 0#_ < b := by
  simp [BitVec.lt_def]

theorem BitVec.pos_iff_ne_zero {n : Nat} (b : BitVec n) : 0#_ < b ↔ b ≠ 0#_ := by
  rw [BitVec.lt_def, Ne, ← BitVec.toNat_inj, BitVec.toNat_zero, Nat.pos_iff_ne_zero]

theorem Nat.eq_iff_testBit_eq {n m : Nat} : n = m ↔ ∀ i, Nat.testBit n i = Nat.testBit m i := by
  refine ⟨?_, Nat.eq_of_testBit_eq⟩
  rintro rfl
  simp

@[simp]
theorem Nat.or_pos {n m : Nat} : 0 < n ||| m ↔ 0 < n ∨ 0 < m := by
  simp [Nat.pos_iff_ne_zero]
  simp [Nat.eq_iff_testBit_eq]
  grind

@[simp]
theorem Nat.shiftLeft_pos {n m : Nat} : 0 < n <<< m ↔ 0 < n := by
  simp [Nat.pos_iff_ne_zero]

def unpack (spec : FloatSpec) (b : BitVec spec.numBits) : UnpackedFloat :=
  let mantissaVec : BitVec spec.mantissaBitsWithoutImplicit := b.extractLsb (spec.mantissaBitsWithoutImplicit - 1) 0 |>.cast (by grind [spec.hm])
  let exponentVec : BitVec spec.exponentBits := b.extractLsb (spec.mantissaBitsWithoutImplicit + spec.exponentBits - 1) spec.mantissaBitsWithoutImplicit |>.cast (by grind [spec.he])
  let exponent : Int := (exponentVec.toNat : Int) - spec.exponentBias
  let signVec : BitVec 1 := b.extractLsb (spec.mantissaBitsWithoutImplicit + spec.exponentBits) (spec.mantissaBitsWithoutImplicit + spec.exponentBits)  |>.cast (by grind)
  let sign := Sign.ofBitVec signVec
  if exponentVec = -1#_ then
    if mantissaVec = 0#_ then
      .infinity sign
    else
      .notANumber
  else if exponentVec = 0#_ then
    if h : mantissaVec = 0#_ then
      .zero sign
    else
      -- subnormal
      .finite sign mantissaVec.toNat exponent (by simpa [BitVec.pos_iff_ne_zero])
  else
    -- normal
    .finite sign (1#1 ++ mantissaVec).toNat exponent (by simp)

def UnpackedFloat.ofFloat (f : Float) : UnpackedFloat :=
  unpack FloatSpec.binary64 f.toBits.toBitVec

def UnpackedFloat.toFloat (f : UnpackedFloat) : Float :=
  Float.ofBits (UInt64.ofBitVec (pack FloatSpec.binary64 f))

end FloatModel
