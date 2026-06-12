# float-model

Minimal float model for the Lean standard library.

## Testing

`scripts/run-testfloat-level2.sh` checks all implemented operations against the
Berkeley TestFloat level 2 test vectors (~160 million in total), running in
parallel on all CPU cores. It requires `testfloat_gen` to be built in
`vendor/berkeley-testfloat-3`. See the module docstrings of
`FloatModelTests/TestFloatCheck.lean` and `FloatModelTests/UCBTestCheck.lean`
for running individual checks by hand.

`scripts/run-ucbtest-gen.sh` checks multiplication, division, and square root
against the number-theoretically hard halfway and nearly-halfway cases of the
UCBTEST suite. The generators in `scripts/ucbtest-gen/` are adaptations of
`ucbmultest`, `ucbdivtest`, and `ucbsqrtest` that emit the generated cases as
TestFloat-format vector files in `vendor/ucbtest-vectors/` (checked with
`testfloat-check`) instead of testing the host arithmetic; the expected results
are derived, as in the originals, without using the operation under test. Set
`NTESTS=<n>` to scale the number of generated cases (default 10000).
