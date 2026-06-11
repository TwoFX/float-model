/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel.Float.Add
import FloatModel.Float.Sub
import FloatModel.Float.Pack

/-!
Checks the model's operations against test vectors produced by Berkeley TestFloat:

  vendor/berkeley-testfloat-3/build/Linux-x86_64-GCC/testfloat_gen -rnear_even f64_sub \
    | lake exe testfloat-check f64_sub

The first argument selects the operation to check (`f64_add` or `f64_sub`).

Each input line has the form `<operand1> <operand2> <expected> <flags>` with all fields
in hexadecimal and floats given as their `binary64` bit patterns. The exception flags are
ignored since the model does not compute them. NaN results are compared as a class, not
bit-for-bit, since the model produces a canonical NaN rather than propagating payloads.
-/

open FloatModel

def hexToUInt64? (s : String) : Option UInt64 :=
  if s.isEmpty then none
  else s.foldl (init := some 0) fun acc c => do
    let acc ← acc
    let d ←
      if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
      else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
      else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
      else none
    some (acc * 16 + UInt64.ofNat d)

def toHex (x : UInt64) : String :=
  let s := String.ofList (Nat.toDigits 16 x.toNat)
  String.ofList (List.replicate (16 - s.length) '0') ++ s

def isNaNBits (x : UInt64) : Bool :=
  (x >>> 52) &&& 0x7FF == 0x7FF && (x &&& 0x000FFFFFFFFFFFFF) != 0

def modelBinop (op : FloatModel.FloatSpec → UnpackedFloat → UnpackedFloat → UnpackedFloat)
    (a b : UInt64) : UInt64 :=
  let ua := unpack FloatSpec.binary64 a.toBitVec
  let ub := unpack FloatSpec.binary64 b.toBitVec
  UInt64.ofBitVec (pack FloatSpec.binary64 (op FloatSpec.binary64 ua ub))

def operations : List (String × Char × (UInt64 → UInt64 → UInt64)) :=
  [("f64_add", '+', modelBinop UnpackedFloat.add),
   ("f64_sub", '-', modelBinop UnpackedFloat.sub)]

public def main (args : List String) : IO UInt32 := do
  let some (_, opChar, modelOp) := args.head?.bind fun name =>
      operations.find? (·.1 == name)
    | IO.eprintln s!"usage: testfloat-check <operation> [--all]\n\
        known operations: {", ".intercalate (operations.map (·.1))}"
      return 2
  let stdin ← IO.getStdin
  let maxShown := if args.contains "--all" then 1000000000 else 20
  let mut total := 0
  let mut failures := 0
  let mut done := false
  while !done do
    let line ← stdin.getLine
    if line.isEmpty then
      done := true
    else
      let tokens := line.trimAscii.toString.split " " |>.filter (!·.isEmpty) |>.toStringList
      match tokens with
      | aHex :: bHex :: expectedHex :: _ =>
        let some a := hexToUInt64? aHex
          | IO.eprintln s!"malformed line: {line.trimAscii.toString}"; return 2
        let some b := hexToUInt64? bHex
          | IO.eprintln s!"malformed line: {line.trimAscii.toString}"; return 2
        let some expected := hexToUInt64? expectedHex
          | IO.eprintln s!"malformed line: {line.trimAscii.toString}"; return 2
        let actual := modelOp a b
        total := total + 1
        let ok := actual == expected || (isNaNBits actual && isNaNBits expected)
        unless ok do
          failures := failures + 1
          if failures ≤ maxShown then
            IO.eprintln s!"FAIL: {toHex a} {opChar} {toHex b} = {toHex expected}, model returned {toHex actual}"
      | [] => pure ()
      | _ =>
        IO.eprintln s!"malformed line: {line.trimAscii.toString}"
        return 2
  if failures > maxShown then
    IO.eprintln s!"... ({failures - maxShown} further failures suppressed)"
  IO.println s!"{total} tests, {failures} failures"
  return if failures == 0 then 0 else 1
