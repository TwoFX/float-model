/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel.Float
import FloatModelTests.CheckUtil

/-!
Checks the model's operations against test vectors produced by Berkeley TestFloat:

  vendor/berkeley-testfloat-3/build/Linux-x86_64-GCC/testfloat_gen -rnear_even f64_sub \
    | lake exe testfloat-check f64_sub

The first argument selects the operation to check (e.g. `f64_add` or `f64_sqrt`).
`scripts/run-testfloat-level2.sh` runs the level 2 vectors for all operations listed
below in parallel on all CPU cores.

Each input line has the form `<operand1> [<operand2>] <expected> <flags>` (the second
operand is absent for unary operations) with all fields in hexadecimal and floats given
as their `binary64` bit patterns. The exception flags are
ignored since the model does not compute them. NaN results are compared as a class, not
bit-for-bit, since the model produces a canonical NaN rather than propagating payloads.
-/

open FloatModel

/--
The operations the checker knows about. A binary operation is checked against lines
of the form `<a> <b> <expected> <flags>`, a unary operation against lines of the form
`<a> <expected> <flags>`.
-/
def operations : List (String × Operation) :=
  [("f64_add", .binary '+' (modelBinop UnpackedFloat.add)),
   ("f64_sub", .binary '-' (modelBinop UnpackedFloat.sub)),
   ("f64_mul", .binary '*' (modelBinop UnpackedFloat.mul)),
   ("f64_div", .binary '/' (modelBinop UnpackedFloat.div)),
   ("f64_sqrt", .unary "sqrt" (modelUnop UnpackedFloat.sqrt)),
   ("f64_eq", .binary '=' (modelCompare UnpackedFloat.beq)),
   ("f64_le", .binary '≤' (modelCompare UnpackedFloat.le)),
   ("f64_lt", .binary '<' (modelCompare UnpackedFloat.lt))
   ]

public def main (args : List String) : IO UInt32 := do
  let some (_, operation) := args.head?.bind fun name =>
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
      unless tokens.isEmpty do
        let parsed : Option (String × UInt64 × UInt64) :=
          match operation, tokens.mapM hexToUInt64? with
          | .binary symbol op, some (a :: b :: expected :: _) =>
            some (s!"{toHex a} {symbol} {toHex b}", op a b, expected)
          | .unary name op, some (a :: expected :: _) =>
            some (s!"{name}({toHex a})", op a, expected)
          | _, _ => none
        let some (description, actual, expected) := parsed
          | IO.eprintln s!"malformed line: {line.trimAscii.toString}"; return 2
        total := total + 1
        let ok := actual == expected || (isNaNBits actual && isNaNBits expected)
        unless ok do
          failures := failures + 1
          if failures ≤ maxShown then
            IO.eprintln s!"FAIL: {description} = {toHex expected}, model returned {toHex actual}"
  if failures > maxShown then
    IO.eprintln s!"... ({failures - maxShown} further failures suppressed)"
  IO.println s!"{total} tests, {failures} failures"
  return if failures == 0 then 0 else 1
