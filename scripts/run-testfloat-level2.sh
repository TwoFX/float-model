#!/usr/bin/env bash
# Checks the model against the Berkeley TestFloat level 2 test vectors for all
# implemented operations, in parallel on all CPU cores.
#
# Usage: scripts/run-testfloat-level2.sh [<operation>...]
#
# With no arguments every operation known to `testfloat-check` is run. Each
# operation's vector stream is split round-robin over an equal share of the
# available cores; set JOBS=<n> to use a different number of cores in total.
# Failing test vectors are reported on stderr as they are found (capped per
# checker process), followed by a per-operation summary on stdout.

set -uo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
gen="$root/vendor/berkeley-testfloat-3/build/Linux-x86_64-GCC/testfloat_gen"
check="$root/.lake/build/bin/testfloat-check"
jobs="${JOBS:-$(nproc)}"

if [[ ! -x "$gen" ]]; then
  echo "error: $gen not found or not executable;" >&2
  echo "build it with: make -C vendor/berkeley-testfloat-3/build/Linux-x86_64-GCC" >&2
  exit 2
fi

(cd "$root" && lake build testfloat-check) || exit 2

# The checker lists the operations it implements in its usage message, so the
# script picks up newly added operations automatically.
known="$("$check" 2>&1 | sed -n 's/^known operations: //p' | tr -d ',')"
if [[ -z "$known" ]]; then
  echo "error: could not determine the operations implemented by testfloat-check" >&2
  exit 2
fi

if [[ $# -gt 0 ]]; then
  ops=("$@")
else
  read -ra ops <<<"$known"
fi

shards=$((jobs / ${#ops[@]}))
((shards < 1)) && shards=1

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# Runs one operation: the generated vectors are distributed round-robin over
# `shards` concurrent checker processes, whose per-shard `<n> tests, <m>
# failures` lines are summed into a single summary line.
run_op() {
  local op="$1"
  "$gen" -level 2 -rnear_even "$op" \
    | split -n "r/$shards" --filter="\"$check\" $op" - \
    | awk -v op="$op" '
        { tests += $1; failures += $3 }
        END { printf "%-10s %11d tests, %d failures\n", op, tests, failures
              exit failures > 0 }'
}

echo "running ${#ops[@]} operation(s) with $shards checker process(es) each"

pids=()
for op in "${ops[@]}"; do
  run_op "$op" >"$tmp/$op.summary" &
  pids+=($!)
done

status=0
for pid in "${pids[@]}"; do
  wait "$pid" || status=1
done

for op in "${ops[@]}"; do
  cat "$tmp/$op.summary" 2>/dev/null || status=1
done
exit "$status"
