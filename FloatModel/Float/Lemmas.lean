/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Model

namespace FloatModel

@[simp]
theorem unpackMantissa_packComponents {spec : FloatSpec} {sign exponent mantissa} :
    unpackMantissa (packComponents spec sign exponent mantissa) = mantissa := by
  ext i hi
  simp [unpackMantissa, packComponents, BitVec.getLsbD_eq_getElem, BitVec.getLsbD_append, hi]

@[simp]
theorem unpackExponent_packComponents {spec : FloatSpec} {sign exponent mantissa} :
    unpackExponent (packComponents spec sign exponent mantissa) = exponent := by
  ext i hi
  simp [unpackExponent, packComponents, BitVec.getLsbD_eq_getElem, BitVec.getLsbD_append, hi]

@[simp]
theorem BitVec.zero_eq_neg_one_iff {w : Nat} : 0#w = (-1#w) ↔ w = 0 := by
  simp [← BitVec.toNat_inj]
  match w with
  | 0 => simp
  | w + 1 =>
    suffices 0 ≠ 2 ^ (w + 1) - 1 by simpa
    have : 2 ≤ 2 ^ (w + 1) := Nat.le_pow (by simp)
    omega

theorem valid_pack {spec : FloatSpec} {f : UnpackedFloat} : spec.Valid (pack spec f) := by
  refine ⟨?_⟩
  fun_cases pack with
  | case1 => simp
  | case2 s => simp [packedInfinity]
  | case3 s => simp [packedZero]
  | case4 s m e hm biasedExponent h => simp [packedInfinity]
  | case5 s m e hm actualMantissaBits biasedExponent h₁ h₂ =>
    suffices biasedExponent % 2 ^ spec.exponentBits ≠ 2 ^ spec.exponentBits - 1 by
      simp [BitVec.neg_one_eq_allOnes, ← BitVec.toNat_inj, this]
    clear h₂ actualMantissaBits hm m s
    suffices biasedExponent < 2 ^ spec.exponentBits - 1 by
      intro h
      rw [Nat.mod_eq_of_lt (by omega)] at h
      omega
    omega
  | case6 s m e hm actualMantissaBits biasedExponent h₁ h₂ =>
    simp [Nat.pos_iff_ne_zero.1 spec.he]

end FloatModel
