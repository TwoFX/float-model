/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel
import FloatModelTests.CheckUtil

/-!
Checks the model's operations against the `binary64` test vectors shipped with the
UCBTEST suite (https://www.netlib.org/fp/ucbtest.tgz, extracted to `vendor/ucb`):

  lake exe ucbtest-check vendor/ucb/ucblib/addd.input vendor/ucb/ucblib/subd.input \
    vendor/ucb/ucblib/muld.input vendor/ucb/ucblib/divd.input vendor/ucb/ucblib/sqrtd.input

With no file arguments the vectors are read from stdin.

Each test vector has the form `<op> <rounding> <relation> <flags> <words...>` where

* `<op>` names the operation; lines naming operations the model does not implement
  (other precisions, libm functions) as well as comment lines are skipped,
* `<rounding>` is one of `n`/`z`/`p`/`m`; only `n` (round to nearest, ties to even)
  is checked since the model implements no other rounding mode,
* `<relation>` is `eq` (the result must match bit-for-bit) or `uo` (the result must
  be a NaN); as in `TestFloatCheck`, NaNs are compared as a class since the model
  produces a canonical NaN,
* `<flags>` lists the expected exception flags and is ignored since the model does
  not compute them,
* `<words...>` gives the operands followed by the expected result, each `binary64`
  value as two 32-bit hexadecimal words, most significant first.
-/

open Float.Model

def operations : List (String × Operation) :=
  [("addd", .binary '+' (modelBinop UnpackedFloat.add)),
   ("subd", .binary '-' (modelBinop UnpackedFloat.sub)),
   ("muld", .binary '*' (modelBinop UnpackedFloat.mul)),
   ("divd", .binary '/' (modelBinop UnpackedFloat.div)),
   ("sqrtd", .unary "sqrt" (modelUnop UnpackedFloat.sqrt))
   ]

/-- Combines a list of 32-bit hexadecimal words into `binary64` bit patterns. -/
def wordsToUInt64s? (words : List String) : Option (List UInt64) :=
  match words with
  | [] => some []
  | hi :: lo :: rest => do
    let hi ← hexToUInt64? hi
    let lo ← hexToUInt64? lo
    guard (hi < 0x100000000 && lo < 0x100000000)
    return ((hi <<< 32) ||| lo) :: (← wordsToUInt64s? rest)
  | _ => none

public def main (args : List String) : IO UInt32 := do
  let (flags, paths) := args.partition (· == "--all")
  let maxShown := if flags.isEmpty then 20 else 1000000000
  let mut lines : Array String := #[]
  if paths.isEmpty then
    let stdin ← IO.getStdin
    let mut done := false
    while !done do
      let line ← stdin.getLine
      if line.isEmpty then
        done := true
      else
        lines := lines.push line
  else
    for path in paths do
      lines := lines ++ (← IO.FS.lines path)
  let mut total := 0
  let mut failures := 0
  let mut skippedRounding := 0
  for line in lines do
    let tokens := line.trimAscii.toString.split " " |>.filter (!·.isEmpty) |>.toStringList
    match tokens with
    | opName :: rounding :: relation :: _flags :: words =>
      -- Comment lines and operations outside the model are skipped here since their
      -- first token does not name a known operation.
      let some (_, operation) := operations.find? (·.1 == opName) | continue
      if rounding != "n" then
        skippedRounding := skippedRounding + 1
        continue
      let parsed : Option (String × UInt64 × UInt64) :=
        match operation, wordsToUInt64s? words with
        | .binary symbol op, some [a, b, expected] =>
          some (s!"{toHex a} {symbol} {toHex b}", op a b, expected)
        | .unary name op, some [a, expected] =>
          some (s!"{name}({toHex a})", op a, expected)
        | _, _ => none
      let some (description, actual, expected) := parsed
        | IO.eprintln s!"malformed line: {line.trimAscii.toString}"; return 2
      let ok ←
        match relation with
        | "eq" => pure (actual == expected || (isNaNBits actual && isNaNBits expected))
        | "uo" => pure (isNaNBits actual)
        | _ => IO.eprintln s!"unsupported relation: {line.trimAscii.toString}"; return 2
      total := total + 1
      unless ok do
        failures := failures + 1
        if failures ≤ maxShown then
          IO.eprintln s!"FAIL: {description} = {toHex expected}, model returned {toHex actual}"
    | _ => continue
  if failures > maxShown then
    IO.eprintln s!"... ({failures - maxShown} further failures suppressed)"
  IO.println s!"{total} tests, {failures} failures \
    ({skippedRounding} vectors for unimplemented rounding modes skipped)"
  return if failures == 0 then 0 else 1
