#!/usr/bin/env bash

set -euo pipefail

# For example, `replicate 3 s` behaves as `{ echo s; echo s; echo s; }`
#
# Note: Any newlines in `s` will be converted to spaces.
function replicate {
    # The `true` prevents a pipefail from ending the script.
    { yes "$2" || true; } | head "-n$1" | tr '\n' ' '
}

# For example, `interleave '1 2 3' '4 5 6'` becomes `'1 4 2 5 3 6 '`.
#
# Note: White-space is not preserved, but words will remain separate.
function merge {
    paste -d' ' \
        <(echo "$1" | tr ' ' '\n') \
        <(echo "$2" | tr ' ' '\n') \
	| grep -vE '^ *$' \
	| tr '\n' ' '
}

# Where to accumulate log files
logdir="slow-tests-logs-${BUILDKITE_JOB_ID:-local}"
mkdir -p "$logdir"
find "$logdir" -name '*.log' -delete

# Where to accumulate Nix symlinks
fromNix="/tmp/fromNix-${BUILDKITE_JOB_ID:-local}"
mkdir -p "$fromNix"

# Always do these final steps
function finish {
    cd "$logdir"

    # Dump to stdout the last line of each log file
    true | tail -n1 $(find . -name '*.log' | sort)

    if [ "true" = "${BUILDKITE-}" ]; then
        # Collect related logs into one artifact file
        true | head -n999999 $(find . -name '*-Cardano.log' | sort) \
            1>"$logdir"/Cardano-artifact.log
        true | head -n999999 $(find . -name '*-RealTPraos.log' | sort) \
            1>"$logdir"/RealTPraos-artifact.log

        # Upload the artifact files as BuildKite artifacts
        #
        # TODO how to specify the charset: UTF-8? The browser renders the
        # artifact poorly when it's clicked in the UI. BuildKite v3 might allow
        # it via `--content-type`, but it seems we're still running v2, which
        # doesn't.
	buildkite-agent artifact upload "$logdir/**/*-artifact.log"
    fi
}
trap finish EXIT

# The directory containing the repository's default.nix
nixdir="$(dirname $0)/.."

# Ensure the invocations below of Nix-built exes see and use the desired locale
#
# See https://nixos.org/nixpkgs/manual/#locales
nix build -f "$nixdir" nightly-checks.glibcLocales -o "$fromNix/glibcLocales"
export LOCALE_ARCHIVE=$(readlink -m "$fromNix/glibcLocales")/lib/locale/locale-archive
export LC_ALL=en_US.utf8

# We use GNU parallel to manage multiple processes
#
# This BuildKite job runs on the benchmarking BuildKite queue, which means
# it'll be running alone on the machine. Thus, we want to keep the CPU
# saturated.
nix build -f "$nixdir" nightly-checks.gnuparallel -o "$fromNix/gnuparallel-exe"

# Build/fetch the exes that run the ThreadNet tests
nix build -f "$nixdir" nightly-checks.Cardano    -o "$fromNix/Cardano-exe"
nix build -f "$nixdir" nightly-checks.RealTPraos -o "$fromNix/RealTPraos-exe"

# GNU parallel will run multiple invocations of this command
function innerCommand {
    uniqueInvocationId="$(printf %03d $PARALLEL_SEQ)"
    logdir="$1"
    suite="$2"
    n="$3"

    # Run the specified tests with the nightly flag set
    "$fromNix/${suite}-exe/bin/test" \
        --pattern "$suite" \
        --quickcheck-tests="$n" \
        --iohk-enable-nightly-tests \
      1>"${logdir}/${uniqueInvocationId}-${suite}.log" 2>&1
}
export -f innerCommand   # now visible to processes spawned by GNU parallel

# The number of *physical* cores on this machine
#
# We avoid HyperThreads et al, at least for now.
ncores="$("$fromNix/gnuparallel-exe/bin/parallel" --number-of-cores)"

# How many tests to run per invocation, and how many invocations to run
#
# For example `7 10 5` would run three invocations, each with the given number
# of tests.
#
# At time of writing, 100 RealTPraos tests takes approximately 680s on overage
# on the BuildKite instance reserved for benchmarking. So, for example, a 1000
# should take a single core about two hours.
#
# See
# https://buildkite.com/input-output-hk/ouroboros-network-nightly/builds/144#0f2d1638-9751-4397-82e8-21ec5f457f1c
#
# We only test in multiples of 100 because otherwise it risks skewing the
# QuickCheck generator distribution (note that QuickCheck sets `maxSize stdArgs
# = 100`).
#
# We transition the ratios from few invocations-with-several-tests to several
# invocations-with-few-tests; this improves the batch's worst-case CPU
# utilization, since it makes it more likely that a small invocation will be
# the last to finish. Invocations-with-several-tests have slightly less
# overhead and also somewhat more reliable percentages in their QuickCheck
# labels.
qcSizes="$(replicate $ncores 1000) $(replicate $(expr 5 '*' $ncores) 100)"

# Run the invocations, but never more at once than the number of physical cores
"$fromNix/gnuparallel-exe/bin/parallel" "-j$ncores" \
    innerCommand "$logdir" RealTPraos \
    ::: Cardano RealTPraos \
    ::: $(merge "$qcSizes" "$qcSizes")
