#!/usr/bin/env fish
# Run the example programs of ScalarField with easifem run.
#
# Usage:
#   ./run_examples.fish                 run all *_test_*.F90 files
#   ./run_examples.fish Initiate GetNo  run the tests whose names start with
#                                       one of the given prefixes
#   ./run_examples.fish --no-rebuild    reuse the test build directory
#   ./run_examples.fish --help          show this help
#
# Each test is built with "easifem run --rebuild" (a clean build of the
# test, not of the library), so that a changed library is never used with
# an old executable. One line is printed per test:
#   PASS     all OK checks passed
#   FAIL     at least one OK check failed
#   CRASH    the program stopped (runtime error, signal, STOP)
#   COMPILE  the test did not compile
#   NOCHECK  the program ran but printed no OK check
# The output of each test is kept in $logdir/<test>.log.
# The exit status is 0 if no test fails, crashes, or does not compile.
#
# The tests read the meshes in ./meshdata/ (square_3x2.h5).

set -l script (status filename)
set -l script_dir (dirname $script)
cd $script_dir; or exit 1

set -l rebuild yes
set -l prefixes
for arg in $argv
    switch $arg
        case --no-rebuild
            set rebuild no
        case -h --help
            sed -n '2,21p' $script
            exit 0
        case '-*'
            echo "Unknown option: $arg" >&2
            exit 2
        case '*'
            set -a prefixes $arg
    end
end

if not command -q easifem
    echo "easifem is not in PATH" >&2
    exit 2
end

set -l meshdata ./meshdata
for m in square_3x2.h5
    if not test -f $meshdata/$m
        echo "$meshdata/$m not found; generate the mesh files first" >&2
        exit 2
    end
end

set -l logdir /tmp/easifemClasses/ScalarField-logs
mkdir -p $logdir

# Select the tests
set -l tests
for f in *_test_*.F90
    set -l name (string replace -r '\.F90$' '' -- $f)
    if test (count $prefixes) -eq 0
        set -a tests $name
        continue
    end
    for p in $prefixes
        if string match -q -- "$p*" $name
            set -a tests $name
            break
        end
    end
end

if test (count $tests) -eq 0
    echo "No test found" >&2
    exit 2
end

set -l npass 0
set -l nbad 0
set -l nnocheck 0
set -l bad

for t in $tests
    if test $rebuild = yes
        set out (easifem run --rebuild $t.F90 2>&1)
    else
        set out (easifem run $t.F90 2>&1)
    end
    set -l rc $status

    # remove the time stamps written by easifem
    set out (string replace -r '^\d{4}/\d\d/\d\d \d\d:\d\d:\d\d ' '' -- $out)
    printf '%s\n' $out >$logdir/$t.log

    set -l nok (count (string match -r '^ok ' -- $out))
    set -l nnot (count (string match -r '^not ok' -- $out))

    if string match -q -- '*ninja: build stopped*' $out
        set state COMPILE
    else if string match -q -r -- \
            'Fortran runtime error|signal: |^STOP |Error executing' $out
        set state CRASH
    else if test $rc -ne 0
        set state CRASH
    else if test $nnot -gt 0
        set state FAIL
    else if test $nok -eq 0
        set state NOCHECK
    else
        set state PASS
    end

    switch $state
        case PASS
            set npass (math $npass + 1)
            set_color green
        case NOCHECK
            set nnocheck (math $nnocheck + 1)
            set_color yellow
        case '*'
            set nbad (math $nbad + 1)
            set -a bad $t
            set_color red
    end
    printf '%-8s' $state
    set_color normal
    printf ' %-45s ok=%-3s not ok=%s\n' $t $nok $nnot
end

echo
echo "Tests: "(count $tests)"  pass: $npass  no check: $nnocheck  bad: $nbad"
echo "Logs:  $logdir"
if test $nbad -gt 0
    echo "Not passing:"
    printf '  %s\n' $bad
    exit 1
end
exit 0
