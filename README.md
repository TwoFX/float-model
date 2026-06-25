# float-model

Minimal float model for the Lean standard library.

This has been upstreamed into Lean core to be included in Lean 4.33,
see [#14079](https://github.com/leanprover/lean4/pull/14079),
[#14091](https://github.com/leanprover/lean4/pull/14091) and
[#14110](https://github.com/leanprover/lean4/pull/14110) and
the [blog post](https://juliahimmel.de/blog/float-qanda/).

## Testing

All test vectors are committed (gzip-compressed, in Berkeley TestFloat format)
under `test-vectors/`, so the checks run with no external tooling:

```
lake exe testfloat-check
```

discovers every vector file, decompresses it, and checks the model's `binary32`
and `binary64` operations (`add sub mul div sqrt eq le lt`) against it, printing
failures and a per-file summary. Pass substring filters to restrict the run
(e.g. `lake exe testfloat-check f64_mul`) and `--all` to show every failure.

Three suites are checked, one per subdirectory of `test-vectors/`: the Berkeley
TestFloat level-1 vectors, the UCBTEST library vectors (converted to TestFloat
format), and the number-theoretically hard halfway/nearly-halfway cases
generated from the UCBTEST `mul`/`div`/`sqrt` programs. See
`test-vectors/README.md` for how each suite is produced and
`FloatModelTests/TestFloatCheck.lean` for the checker.
