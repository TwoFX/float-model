/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

public import FloatModel.Float

/-!
Helpers shared by the executables that check the model against external test suites
(`TestFloatCheck` and `UCBTestCheck`).
-/

open FloatModel

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

public def toHex (x : UInt64) : String :=
  let s := String.ofList (Nat.toDigits 16 x.toNat)
  String.ofList (List.replicate (16 - s.length) '0') ++ s

public def isNaNBits (x : UInt64) : Bool :=
  (x >>> 52) &&& 0x7FF == 0x7FF && (x &&& 0x000FFFFFFFFFFFFF) != 0

public def modelBinop (op : FloatModel.FloatSpec → UnpackedFloat → UnpackedFloat → UnpackedFloat)
    (a b : UInt64) : UInt64 :=
  let ua := unpack FloatSpec.binary64 a.toBitVec
  let ub := unpack FloatSpec.binary64 b.toBitVec
  UInt64.ofBitVec (pack FloatSpec.binary64 (op FloatSpec.binary64 ua ub))

public def modelUnop (op : FloatModel.FloatSpec → UnpackedFloat → UnpackedFloat)
    (a : UInt64) : UInt64 :=
  let ua := unpack FloatSpec.binary64 a.toBitVec
  UInt64.ofBitVec (pack FloatSpec.binary64 (op FloatSpec.binary64 ua))

public inductive Operation where
  /-- A binary operation on `binary64` bit patterns. -/
  | binary (symbol : Char) (op : UInt64 → UInt64 → UInt64)
  /-- A unary operation on `binary64` bit patterns. -/
  | unary (name : String) (op : UInt64 → UInt64)
