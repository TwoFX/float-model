/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel

/-!
Helpers shared by `TestFloatCheck`, which checks the model against the committed
TestFloat-format test vectors for both `binary32` and `binary64`.
-/

open Float.Model

public def hexToUInt64? (s : String) : Option UInt64 :=
  if s.isEmpty then none
  else s.foldl (init := some 0) fun acc c => do
    let acc ← acc
    let d ←
      if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
      else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
      else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
      else none
    some (acc * 16 + UInt64.ofNat d)

/-- Render `x` as a zero-padded hexadecimal string of `width` digits. -/
private def toHexWidth (width : Nat) (x : UInt64) : String :=
  let s := String.ofList (Nat.toDigits 16 x.toNat)
  String.ofList (List.replicate (width - s.length) '0') ++ s

/-- Render a `binary64` bit pattern as 16 hexadecimal digits. -/
public def toHex (x : UInt64) : String := toHexWidth 16 x

/-- Render a `binary32` bit pattern (held in the low 32 bits) as 8 hexadecimal digits. -/
public def toHex32 (x : UInt64) : String := toHexWidth 8 x

/-- Whether a `binary64` bit pattern is a `NaN` (maximal exponent, nonzero mantissa). -/
public def isNaNBits (x : UInt64) : Bool :=
  (x >>> 52) &&& 0x7FF == 0x7FF && (x &&& 0x000FFFFFFFFFFFFF) != 0

/--
Whether a `binary32` bit pattern (held in the low 32 bits of `x`) is a `NaN`
(maximal 8-bit exponent, nonzero 23-bit mantissa).
-/
public def isNaNBits32 (x : UInt64) : Bool :=
  (x >>> 23) &&& 0xFF == 0xFF && (x &&& 0x7FFFFF) != 0

public def modelBinop (op : Float.Model → Float.Model → Float.Model)
    (a b : UInt64) : UInt64 :=
  (op (Float.Model.ofBits a) (Float.Model.ofBits b)).toBits

public def modelUnop (op : Float.Model → Float.Model)
    (a : UInt64) : UInt64 :=
  (op (Float.Model.ofBits a)).toBits

/--
Adapts a comparison on `Float.Model`s to operate on `binary64` bit patterns,
returning `1` for a true result and `0` for a false one to match the boolean
result column TestFloat emits for `f64_eq`, `f64_le`, and `f64_lt`.
-/
public def modelCompare (op : Float.Model → Float.Model → Bool) (a b : UInt64) : UInt64 :=
  if op (Float.Model.ofBits a) (Float.Model.ofBits b) then 1 else 0

/-- `binary32` counterpart of `modelBinop`; bit patterns are held in the low 32 bits. -/
public def modelBinop32 (op : Float32.Model → Float32.Model → Float32.Model)
    (a b : UInt64) : UInt64 :=
  (op (Float32.Model.ofBits a.toUInt32) (Float32.Model.ofBits b.toUInt32)).toBits.toUInt64

/-- `binary32` counterpart of `modelUnop`. -/
public def modelUnop32 (op : Float32.Model → Float32.Model)
    (a : UInt64) : UInt64 :=
  (op (Float32.Model.ofBits a.toUInt32)).toBits.toUInt64

/-- `binary32` counterpart of `modelCompare`. -/
public def modelCompare32 (op : Float32.Model → Float32.Model → Bool) (a b : UInt64) : UInt64 :=
  if op (Float32.Model.ofBits a.toUInt32) (Float32.Model.ofBits b.toUInt32) then 1 else 0

public inductive Operation where
  /-- A binary operation on bit patterns. -/
  | binary (symbol : Char) (op : UInt64 → UInt64 → UInt64)
  /-- A unary operation on bit patterns. -/
  | unary (name : String) (op : UInt64 → UInt64)

/--
A checkable operation together with the precision-specific helpers needed to
compare and display its results: `isNaN` classifies a result as a `NaN` (so
`NaN`s are compared as a class rather than bit-for-bit), and `toHex` renders a
bit pattern for failure messages.
-/
public structure Check where
  /-- The operation under test, acting on bit patterns. -/
  op : Operation
  /-- Classifies a result bit pattern as a `NaN`. -/
  isNaN : UInt64 → Bool
  /-- Renders a bit pattern as hexadecimal for display. -/
  toHex : UInt64 → String

/-- A `binary64` operation paired with its `binary64` `NaN` test and hex renderer. -/
public def f64Check (op : Operation) : Check := { op, isNaN := isNaNBits, toHex := toHex }

/-- A `binary32` operation paired with its `binary32` `NaN` test and hex renderer. -/
public def f32Check (op : Operation) : Check := { op, isNaN := isNaNBits32, toHex := toHex32 }
