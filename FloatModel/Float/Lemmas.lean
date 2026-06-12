/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float.Valid
public import FloatModel.Float.Round

namespace FloatModel

@[simp]
theorem snd_shiftToTargetExponent {spec : FloatSpec} {mantissa : Nat} {exponent : Int}
    {accuracy : Accuracy} :
    (shiftToTargetExponent spec mantissa exponent accuracy).2 = max exponent (spec.targetExponent (totalExponent mantissa exponent)) := by
  grind [shiftToTargetExponent]

theorem totalExponent_shiftToTargetExponent {spec : FloatSpec} {mantissa : Nat} {exponent : Int}
    {accuracy : Accuracy} :
    totalExponent (shiftToTargetExponent spec mantissa exponent accuracy).1.mantissa (shiftToTargetExponent spec mantissa exponent accuracy).2 =
    totalExponent mantissa exponent := by
  sorry

theorem valid_roundWithAccuracy {spec : FloatSpec} {sign : Sign} {mantissa : Nat} {exponent : Int}
    {accuracy : Accuracy} (h : exponent ≤ spec.targetExponent (totalExponent mantissa exponent)) :
    spec.Valid (roundWithAccuracy spec sign mantissa exponent accuracy) := by
  fun_cases roundWithAccuracy with
  | case1 => simp
  | case2 em₁ e₁ hshift roundedEm₁ finalExtendedMantissa finalExponent hshift' finalMantissa hne =>
    refine .finite _ _ _ _ ⟨?_, ?_⟩
    · sorry
    · sorry

end FloatModel
