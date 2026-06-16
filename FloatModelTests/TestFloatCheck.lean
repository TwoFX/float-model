/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Julia M. Himmel
-/
module

import FloatModel
import FloatModelTests.CheckUtil

/-!
Checks the model's `binary32` and `binary64` operations against the test vectors
committed under `test-vectors/`. Running

  lake exe testfloat-check

with no arguments discovers every `*.txt.gz` vector file under `test-vectors/`,
decompresses it (by shelling out to `gzip -dc`), and checks it; an optional list
of substring filters restricts the run to matching files, e.g.

  lake exe testfloat-check f64_mul       -- only files whose label contains "f64_mul"

The vector files are in Berkeley TestFloat format: each line is
`<operand1> [<operand2>] <expected> <flags>` (the second operand is absent for
unary operations) with all fields in hexadecimal and floats given as their bit
patterns — 16 digits for `binary64`, 8 for `binary32`. The exception-flags field
is ignored since the model does not compute flags. NaN results are compared as a
class, not bit-for-bit, since the model produces a canonical NaN rather than
propagating payloads.

The committed vectors come from three suites (one subdirectory each); see
`test-vectors/README.md` for how they are produced. By default only the first 20
failures are shown; pass `--all` to show them all.
-/

open Float.Model

/-- The directory holding the committed test-vector files, relative to the repo root. -/
def vectorsDir : System.FilePath := "test-vectors"

/--
The operations the checker knows about, keyed by the `<precision>_<operation>`
basename of a vector file (e.g. `f64_add`, `f32_sqrt`). A binary operation is
checked against lines of the form `<a> <b> <expected> <flags>`, a unary
operation against lines of the form `<a> <expected> <flags>`.
-/
def operations : List (String × Check) :=
  [("f64_add", f64Check (.binary '+' (modelBinop Float.Model.add))),
   ("f64_sub", f64Check (.binary '-' (modelBinop Float.Model.sub))),
   ("f64_mul", f64Check (.binary '*' (modelBinop Float.Model.mul))),
   ("f64_div", f64Check (.binary '/' (modelBinop Float.Model.div))),
   ("f64_sqrt", f64Check (.unary "sqrt" (modelUnop Float.Model.sqrt))),
   ("f64_eq", f64Check (.binary '=' (modelCompare Float.Model.beq))),
   ("f64_le", f64Check (.binary '≤' (modelCompare Float.Model.le))),
   ("f64_lt", f64Check (.binary '<' (modelCompare Float.Model.lt))),
   ("f32_add", f32Check (.binary '+' (modelBinop32 Float32.Model.add))),
   ("f32_sub", f32Check (.binary '-' (modelBinop32 Float32.Model.sub))),
   ("f32_mul", f32Check (.binary '*' (modelBinop32 Float32.Model.mul))),
   ("f32_div", f32Check (.binary '/' (modelBinop32 Float32.Model.div))),
   ("f32_sqrt", f32Check (.unary "sqrt" (modelUnop32 Float32.Model.sqrt))),
   ("f32_eq", f32Check (.binary '=' (modelCompare32 Float32.Model.beq))),
   ("f32_le", f32Check (.binary '≤' (modelCompare32 Float32.Model.le))),
   ("f32_lt", f32Check (.binary '<' (modelCompare32 Float32.Model.lt)))]

/-- All `*.txt.gz` vector files under `dir`, recursively. -/
partial def gzVectorFiles (dir : System.FilePath) : IO (Array System.FilePath) := do
  let mut out := #[]
  for entry in (← dir.readDir) do
    if ← entry.path.isDir then
      out := out ++ (← gzVectorFiles entry.path)
    else if entry.path.toString.endsWith ".txt.gz" then
      out := out.push entry.path
  return out

/-- The `<precision>_<operation>` key of a `*.txt.gz` vector file, e.g. `f64_add`. -/
def vectorKey (path : System.FilePath) : Option String := do
  let name ← path.fileName
  guard (name.endsWith ".txt.gz")
  return (name.dropEnd ".txt.gz".length).toString

/-- Whether `s` contains `sub` as a substring (used for the command-line filters). -/
def containsSubstr (s sub : String) : Bool := (s.splitOn sub).length > 1

/-- The number of failures shown before the rest are suppressed unless `--all` is given. -/
def defaultMaxShown : Nat := 20

public def main (args : List String) : IO UInt32 := do
  let (flags, filters) := args.partition (·.startsWith "--")
  let maxShown := if flags.contains "--all" then 1000000000 else defaultMaxShown

  unless (← vectorsDir.pathExists) do
    IO.eprintln s!"error: vector directory {vectorsDir} not found (run from the repository root)"
    return 2
  let files := (← gzVectorFiles vectorsDir).qsort (·.toString < ·.toString)
  if files.isEmpty then
    IO.eprintln s!"error: no *.txt.gz vector files found under {vectorsDir}"
    return 2

  let mut grandTotal := 0
  let mut grandFailures := 0
  let mut shown := 0
  let mut ran := 0
  for path in files do
    let some key := vectorKey path
      | IO.eprintln s!"skipping {path}: not a recognizable vector file"; continue
    let suite := (path.parent.bind (·.fileName)).getD "?"
    let label := s!"{suite}/{key}"
    if !filters.isEmpty && !filters.any (containsSubstr label ·) then
      continue
    let some (_, check) := operations.find? (·.1 == key)
      | IO.eprintln s!"error: {path} names unknown operation '{key}'"; return 2

    -- Decompress the vector file by streaming `gzip -dc <path>`.
    let child ← IO.Process.spawn
      { cmd := "gzip", args := #["-dc", path.toString], stdout := .piped }
    let stdout := child.stdout
    let mut total := 0
    let mut failures := 0
    let mut done := false
    while !done do
      let line ← stdout.getLine
      if line.isEmpty then
        done := true
      else
        let tokens := line.trimAscii.toString.split " " |>.filter (!·.isEmpty) |>.toStringList
        unless tokens.isEmpty do
          let parsed : Option (String × UInt64 × UInt64) :=
            match check.op, tokens.mapM hexToUInt64? with
            | .binary symbol op, some (a :: b :: expected :: _) =>
              some (s!"{check.toHex a} {symbol} {check.toHex b}", op a b, expected)
            | .unary name op, some (a :: expected :: _) =>
              some (s!"{name}({check.toHex a})", op a, expected)
            | _, _ => none
          match parsed with
          | none => IO.eprintln s!"malformed line in {label}: {line.trimAscii.toString}"
          | some (description, actual, expected) =>
            total := total + 1
            let ok := actual == expected || (check.isNaN actual && check.isNaN expected)
            unless ok do
              failures := failures + 1
              if shown < maxShown then
                shown := shown + 1
                IO.eprintln
                  s!"FAIL [{label}]: {description} = {check.toHex expected}, \
                     model returned {check.toHex actual}"
    let exit ← child.wait
    if exit != 0 then
      IO.eprintln s!"error: gzip -dc {path} exited with status {exit}"
      return 2
    IO.println s!"{label}: {total} tests, {failures} failures"
    grandTotal := grandTotal + total
    grandFailures := grandFailures + failures
    ran := ran + 1

  if grandFailures > shown then
    IO.eprintln s!"... ({grandFailures - shown} further failures suppressed; pass --all to show them)"
  IO.println s!"total: {grandTotal} tests, {grandFailures} failures across {ran} file(s)"
  return if grandFailures == 0 then 0 else 1
