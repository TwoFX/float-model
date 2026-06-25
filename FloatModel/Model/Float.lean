/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

prelude
public import FloatModel.Model.Format.Valid
public import FloatModel.Model.Unpacked.Pack.Lemmas
public import FloatModel.Model.Unpacked.Operations
public import Init.Data.Order.Factories

-- This file is part of the logical model for floats which authors of float libraries
-- need to rely on.
@[expose] public section

/-- The logical model for the `Float` type. -/
structure FloatModel.Model where
  /-- The underlying bit pattern of the `Float`. -/
  toBits : UInt64
  /-- The underlying bit pattern is valid according to the format. -/
  valid : FloatModel.Model.Format.binary64.Valid toBits.toBitVec

namespace FloatModel.Model

/-- Unpack a `FloatModel.Model` into the corresponding `UnpackedFloat`. -/
def unpack (f : FloatModel.Model) : UnpackedFloat :=
  UnpackedFloat.unpack Format.binary64 f.toBits.toBitVec

/--
Pack an `UnpackedFloat` into the corresponding `FloatModel.Model`.
This operation only gives a meaningful result if the float is
already correctly rounded for the `Format.binary64` format.
-/
def pack (f : UnpackedFloat) : FloatModel.Model where
  toBits := UInt64.ofBitVec (UnpackedFloat.pack Format.binary64 f)
  valid := by simp

/--
Compute the sum of two `FloatModel.Model`.
-/
def add (a b : FloatModel.Model) : FloatModel.Model :=
  pack (UnpackedFloat.add Format.binary64 a.unpack b.unpack)

/--
Compute the difference of two `FloatModel.Model`.
-/
def sub (a b : FloatModel.Model) : FloatModel.Model :=
  pack (UnpackedFloat.sub Format.binary64 a.unpack b.unpack)

/--
Compute the product of two `FloatModel.Model`.
-/
def mul (a b : FloatModel.Model) : FloatModel.Model :=
  pack (UnpackedFloat.mul Format.binary64 a.unpack b.unpack)

/--
Compute the quotient of two `FloatModel.Model`.
-/
def div (a b : FloatModel.Model) : FloatModel.Model :=
  pack (UnpackedFloat.div Format.binary64 a.unpack b.unpack)

instance : Add FloatModel.Model where
  add a b := a.add b

instance : Sub FloatModel.Model where
  sub a b := a.sub b

instance : Mul FloatModel.Model where
  mul a b := a.mul b

instance : Div FloatModel.Model where
  div a b := a.div b

/--
Compute the square root of a `FloatModel.Model`.
-/
def sqrt (a : FloatModel.Model) : FloatModel.Model :=
  pack (UnpackedFloat.sqrt Format.binary64 a.unpack)

/--
Negate a `FloatModel.Model`.
-/
def neg (a : FloatModel.Model) : FloatModel.Model :=
  pack a.unpack.neg

/--
Return a `FloatModel.Model` with positive sign.
-/
def abs (a : FloatModel.Model) : FloatModel.Model :=
  pack a.unpack.abs

/--
Compute the ordering between two `FloatModel.Model` as specified by IEEE. Returns an
`Option Ordering` to account for the fact that `NaN` is incomparable with everything.
Also, positive and negative zero are equal.
-/
protected def compare (a b : FloatModel.Model) : Option Ordering :=
  a.unpack.compare b.unpack

/--
Determine whether `a` is less than or equal to `b` according to IEEE rules.

This is not a total ordering, and `≤` is not reflexive.
-/
protected def le (a b : FloatModel.Model) : Bool :=
  a.unpack.le b.unpack

/--
Determine whether `a` is less than `b` according to IEEE rules.

This is not a total ordering.
-/
protected def lt (a b : FloatModel.Model) : Bool :=
  a.unpack.lt b.unpack

/--
Determine whether `a` is equal to `b` according to IEEE rules.

This is not a reflexive relation.
-/
protected def beq (a b : FloatModel.Model) : Bool :=
  a.unpack.beq b.unpack

instance : LE FloatModel.Model where
  le a b := a.le b

instance : DecidableLE FloatModel.Model :=
  inferInstanceAs (∀ (a b : FloatModel.Model), Decidable (a.le b))

instance : LT FloatModel.Model where
  lt a b := a.lt b

instance : DecidableLT FloatModel.Model :=
  inferInstanceAs (∀ (a b : FloatModel.Model), Decidable (a.lt b))

instance : BEq FloatModel.Model where
  beq a b := a.beq b

instance : Min FloatModel.Model :=
  Min.leftLeaningOfLE _

instance : Max FloatModel.Model :=
  Max.leftLeaningOfLE _

/--
Returns `true` if the float represents a real number, i.e., it is neither infinite nor `NaN`.
-/
def isFinite (a : FloatModel.Model) : Bool :=
  a.unpack.isFinite

/--
Returns `true` if the float is positive or negative infinity.
-/
def isInf (a : FloatModel.Model) : Bool :=
  a.unpack.isInf

/--
Returns `true` if the float is `NaN`.
-/
def isNaN (a : FloatModel.Model) : Bool :=
  a.unpack.isNaN

/--
Construct a `FloatModel.Model` from its bit representation. This operation canonicalizes
all `NaN` inputs into the canonical `NaN`.
-/
def ofBits (a : UInt64) : FloatModel.Model :=
  pack (UnpackedFloat.unpack Format.binary64 a.toBitVec)

/-- Converts an `Int` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofInt (n : Int) : FloatModel.Model :=
  pack (UnpackedFloat.ofInt Format.binary64 n)

/-- Converts a `Nat` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofNat (n : Nat) : FloatModel.Model :=
  pack (UnpackedFloat.ofNat Format.binary64 n)

/-- Converts a `UInt8` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofUInt8 (n : UInt8) : FloatModel.Model :=
  pack (UnpackedFloat.ofUInt8 Format.binary64 n)

/-- Converts a `UInt16` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofUInt16 (n : UInt16) : FloatModel.Model :=
  pack (UnpackedFloat.ofUInt16 Format.binary64 n)

/-- Converts a `UInt32` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofUInt32 (n : UInt32) : FloatModel.Model :=
  pack (UnpackedFloat.ofUInt32 Format.binary64 n)

/-- Converts a `UInt64` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofUInt64 (n : UInt64) : FloatModel.Model :=
  pack (UnpackedFloat.ofUInt64 Format.binary64 n)

/-- Converts a `USize` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofUSize (n : USize) : FloatModel.Model :=
  pack (UnpackedFloat.ofUSize Format.binary64 n)

/-- Converts an `Int8` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofInt8 (n : Int8) : FloatModel.Model :=
  pack (UnpackedFloat.ofInt8 Format.binary64 n)

/-- Converts an `Int16` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofInt16 (n : Int16) : FloatModel.Model :=
  pack (UnpackedFloat.ofInt16 Format.binary64 n)

/-- Converts an `Int32` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofInt32 (n : Int32) : FloatModel.Model :=
  pack (UnpackedFloat.ofInt32 Format.binary64 n)

/-- Converts an `Int64` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofInt64 (n : Int64) : FloatModel.Model :=
  pack (UnpackedFloat.ofInt64 Format.binary64 n)

/-- Converts an `ISize` to a `FloatModel.Model`, returning positive zero on zero. -/
def ofISize (n : ISize) : FloatModel.Model :=
  pack (UnpackedFloat.ofISize Format.binary64 n)

/-- Converts a `FloatModel.Model` to a `UInt8`. -/
def toUInt8 (f : FloatModel.Model) : UInt8 := f.unpack.toUInt8

/-- Converts a `FloatModel.Model` to a `UInt16`. -/
def toUInt16 (f : FloatModel.Model) : UInt16 := f.unpack.toUInt16

/-- Converts a `FloatModel.Model` to a `UInt32`. -/
def toUInt32 (f : FloatModel.Model) : UInt32 := f.unpack.toUInt32

/-- Converts a `FloatModel.Model` to a `UInt64`. -/
def toUInt64 (f : FloatModel.Model) : UInt64 := f.unpack.toUInt64

/-- Converts a `FloatModel.Model` to a `USize`. -/
def toUSize (f : FloatModel.Model) : USize := f.unpack.toUSize

/-- Converts a `FloatModel.Model` to an `Int8`. -/
def toInt8 (f : FloatModel.Model) : Int8 := f.unpack.toInt8

/-- Converts a `FloatModel.Model` to an `Int16`. -/
def toInt16 (f : FloatModel.Model) : Int16 := f.unpack.toInt16

/-- Converts a `FloatModel.Model` to an `Int32`. -/
def toInt32 (f : FloatModel.Model) : Int32 := f.unpack.toInt32

/-- Converts a `FloatModel.Model` to an `Int64`. -/
def toInt64 (f : FloatModel.Model) : Int64 := f.unpack.toInt64

/-- Converts a `FloatModel.Model` to an `ISize`. -/
def toISize (f : FloatModel.Model) : ISize := f.unpack.toISize

end FloatModel.Model
