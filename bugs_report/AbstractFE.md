# Bug report: AbstractFE_

- Module: `src/modules/AbstractFE/src/AbstractFE_Class.F90`
- Submodules: `src/submodules/AbstractFE/src/`
- Tests: `src/modules/AbstractFE/examples/`
- Related implementation: `src/submodules/BasisOpt/src/`
- Related dependency: easifemBase, `src/modules/Display/src/disp/`
- Date: 2026-10-29

These failures were observed while running the AbstractFE examples against
the installed libraries. No library fixes have been made for these items.
The exact cause must be checked against the installed build and its source.

Status: **Open** means not fixed. **Fixed** requires a source correction,
a rebuilt library and a passing regression test.

## Test results

The full run compiled all 16 examples: 15 passed and `Display_test_1`
failed. After changing that example to use a positive output unit, a
targeted rerun still failed as described below.

Run from `src/modules/AbstractFE/examples/`:

```fish
./run_examples.fish Display
```

`runner.toml` selects Debug for the example and links easifemClasses.
The installed libraries were not rebuilt during these runs.

The latest output is in:
`/tmp/easifemClasses/AbstractFE-logs/Display_test_1.log`.
Rerunning the runner overwrites this log.

## Open

### 1. MdEncode crashes in the base formatting utility

After initializing a linear H1 Lagrange triangle, calling
`markdown = obj%MdEncode()` stops with:

```text
Fortran runtime error: Substring out of bounds: upper bound (5) of
'fmt1' exceeds string length (4)
```

The diagnostic points to line 653 of the base library file
`src/modules/Display/src/disp/dispmodule_util.F90`, in `sszipfmt`.
That routine starts writing format characters at index five and requires
its caller to provide a sufficiently long output buffer.

The observed call path enters `AbstractFE_`'s `obj_MdEncode`, then
`BasisOpt_`'s `obj_MdEncode`, which calls the base `ToString` and Markdown
encoding utilities. The exact caller supplying the short buffer has not
yet been isolated.

Expected: return a Markdown string without a bounds error.

`ReactEncode()` follows this call in the example and was not reached;
its runtime behaviour remains unverified by this test.

### 2. Display output is not readable from the requested scratch unit

The current example opens positive unit 20 as a scratch file with
`ACTION="readwrite"`, passes it to `obj%Display`, rewinds it and reads a
line. The read returns a nonzero `IOSTAT`, producing:

```text
not ok 1 - Display writes output
```

Expected: the requested unit contains the displayed object data and the
first read succeeds.

The precise IOSTAT and IOMSG were not printed by the example. Investigate
unit forwarding, output buffering and scratch-file contents before
assigning a root cause.

### 3. Display crashes with a NEWUNIT-assigned negative unit

The original version of `Display_test_1` opened the scratch file using
`OPEN(NEWUNIT=unitno, ...)` and passed that unit to `obj%Display`.
The display call stopped with:

```text
Fortran runtime error: Unit number is negative and unit was not already
opened with OPEN(NEWUNIT=...)
```

The diagnostic points to line 901 of the base library file
`src/modules/Display/src/disp/dispmodule_util.F90`, at the write to the
selected output unit. The test had opened its unit with NEWUNIT, so the
unit reaching this write needs investigation.

To reproduce this original variant, change the current example's
`OPEN(UNIT=unitno, ...)` to `OPEN(NEWUNIT=unitno, ...)` and declare
`unitno` as a variable rather than a parameter.

Expected: support an already-open NEWUNIT unit, or explicitly document
any restrictions imposed by the display utility.
