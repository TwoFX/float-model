/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Basic
public import FloatModel.Float.FloatSpec

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

namespace FloatModel

/-- Creates a packed float from a sign, an exponent and a mantissa. -/
def packComponents (spec : FloatSpec) (sign : Sign)
    (exponent : BitVec spec.exponentBits)
    (mantissa : BitVec spec.mantissaBitsWithoutImplicit) : BitVec spec.numBits :=
  sign.toBitVec ++ exponent ++ mantissa

/-- Creates the packed signed infinity representation for the given specification. -/
def mkInfinity (spec : FloatSpec) (sign : Sign) : BitVec spec.numBits :=
  packComponents spec sign (-1#_) 0

/-- Creates the canonical packed `NaN` for the given specification. -/
def mkNaN (spec : FloatSpec) :=
  packComponents spec .positive (-1#_) (1#_ <<< (spec.mantissaBitsWithoutImplicit - 1))

/-- Creates the packed signed zero representation for the given specification. -/
def mkZero (spec : FloatSpec) (sign : Sign) :=
  packComponents spec sign 0 0

/--
Packs the given float into the format given by the specification.

This function assumes that the float is already correctly rounded for the given specification.
This means that the exponent must be equal to the exponent computed by `spec.targetExponent`.
-/
def pack (spec : FloatSpec) : UnpackedFloat → BitVec spec.numBits
  | .notANumber => mkNaN spec
  | .infinity s => mkInfinity spec s
  | .zero s => mkZero spec s
  | .finite s m e _ =>
    let actualMantissaBits := m.log2
    let biasedExponent := (e + spec.exponentBias + spec.mantissaBitsWithoutImplicit).toNat
    if 2 ^ spec.exponentBits ≤ biasedExponent + 1 then
      mkInfinity spec s
    else if actualMantissaBits + 1 = spec.mantissaBits then
      -- normal
      -- Observe that the transformation of the mantissa clears the implicit bit
      packComponents spec s (BitVec.ofNat _ biasedExponent) (BitVec.ofNat _ m)
    else
      -- subnormal
      packComponents spec s 0#_ (BitVec.ofNat _ m)

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

/--
Unpacks the given float according to the given specification.

The resulting float may be assumed to be correctly rounded for the given specification.
-/
def unpack (spec : FloatSpec) (b : BitVec spec.numBits) : UnpackedFloat :=
  let mantissaVec : BitVec spec.mantissaBitsWithoutImplicit := b.extractLsb (spec.mantissaBitsWithoutImplicit - 1) 0 |>.cast (by grind [spec.hm])
  let exponentVec : BitVec spec.exponentBits := b.extractLsb (spec.mantissaBitsWithoutImplicit + spec.exponentBits - 1) spec.mantissaBitsWithoutImplicit |>.cast (by grind [spec.he])
  let unbiasedExponent : Int := (exponentVec.toNat : Int) - spec.exponentBias
  let exponent := unbiasedExponent - spec.mantissaBitsWithoutImplicit
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
      .finite sign mantissaVec.toNat (exponent + 1) (by simpa [BitVec.pos_iff_ne_zero])
  else
    -- normal
    .finite sign (1#1 ++ mantissaVec).toNat exponent (by simp)

def UnpackedFloat.ofFloat (f : Float) : UnpackedFloat :=
  unpack FloatSpec.binary64 f.toBits.toBitVec

def UnpackedFloat.toFloat (f : UnpackedFloat) : Float :=
  Float.ofBits (UInt64.ofBitVec (pack FloatSpec.binary64 f))

end FloatModel
