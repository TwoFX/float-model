/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

prelude
public import Init.Data.BitVec.Bootstrap
public import Init.Data.BitVec.Lemmas
public import Init.Data.Nat.Bitwise.Lemmas
public import Init.Data.Nat.Lemmas
public import Init.Data.Int.Pow
public import Init.Omega

/-!
Miscellaneous lemmas about `Nat`, `BitVec`, etc. that are used throughout the model but
are not specific to floating-point numbers.
-/

@[expose] public section

namespace FloatModel.Model

theorem BitVec.toNat_pos {n : Nat} (b : BitVec n) : 0 < b.toNat ↔ 0#_ < b := by
  simp [BitVec.lt_def]

theorem BitVec.pos_iff_ne_zero {n : Nat} (b : BitVec n) : 0#_ < b ↔ b ≠ 0#_ := by
  rw [BitVec.lt_def, Ne, ← BitVec.toNat_inj, BitVec.toNat_zero, Nat.pos_iff_ne_zero]

theorem Nat.eq_iff_testBit_eq {n m : Nat} : n = m ↔ ∀ i, Nat.testBit n i = Nat.testBit m i := by
  refine ⟨?_, Nat.eq_of_testBit_eq⟩
  rintro rfl
  simp

@[simp]
theorem Nat.or_eq_zero {n m : Nat} : n ||| m = 0 ↔ n = 0 ∧ m = 0 := by
  simp [Nat.eq_iff_testBit_eq, forall_and]

@[simp]
theorem Nat.or_pos {n m : Nat} : 0 < n ||| m ↔ 0 < n ∨ 0 < m := by
  rw [Nat.pos_iff_ne_zero, ne_eq, Nat.or_eq_zero, Decidable.not_and_iff_not_or_not,
    Nat.pos_iff_ne_zero, Nat.pos_iff_ne_zero]

@[simp]
theorem Nat.shiftLeft_pos {n m : Nat} : 0 < n <<< m ↔ 0 < n := by
  simp [Nat.pos_iff_ne_zero]

@[simp]
theorem BitVec.zero_eq_neg_one_iff {w : Nat} : 0#w = (-1#w) ↔ w = 0 := by
  simp [← BitVec.toNat_inj]
  match w with
  | 0 => simp
  | w + 1 =>
    suffices 0 ≠ 2 ^ (w + 1) - 1 by simpa
    have : 2 ≤ 2 ^ (w + 1) := Nat.le_pow (by simp)
    omega

end FloatModel.Model
