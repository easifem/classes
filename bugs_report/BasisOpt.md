# Bug report: BasisOpt_

- Module: `src/modules/BasisOpt/src/BasisOpt_Class.F90`
- Submodules: `src/submodules/BasisOpt/src/`
- Tests: `src/modules/BasisOpt/examples/`
- Related dependencies: QuadratureOpt_ and the easifemBase display utility
- Date: 2026-10-29

These findings come from source inspection and example runs against the
installed libraries. No implementation fixes or library rebuilds were
performed. The example runner selects Debug and links easifemClasses.

Status: **Open** means not fixed. **Fixed** requires an implementation
change, a rebuilt library and a passing regression test.

Run from `src/modules/BasisOpt/examples/`:

```fish
./run_examples.fish
```

Individual tests can be selected by prefix. Logs are written to
`/tmp/easifemClasses/BasisOpt-logs/<test>.log` and overwritten on reruns.

## Test results

The final full run compiled all 30 examples: **23 passed and 7 failed**.
The failing examples are listed under the findings below. There were no
examples without checks.

## Open

### 1. Initiate ignores dofType and transformType

- Implementation: `BasisOpt_Class@ConstructorMethods.F90`, `obj_Initiate`.
- Reproducer: `Initiate_test_2.F90`.

The constructor accepts both arguments but never assigns them. After
resetting the object, the corresponding fields retain the factory values:
`dofType = [1,1,1,1]` and `transformType = 0`.

The test supplies `[2,2,2,2]` and `2`, then reads the values through
GetParam. Both checks fail. SetParam can store these values, as verified
by `SetParam_test_1.F90`.

Expected: preserve the requested classification values, or revise the
interface if these inputs are intentionally unsupported.

### 2. SetParam uses total array size as the faceOrder column count

- Implementation: `BasisOpt_Class@SetMethods.F90`, `obj_SetParam`, line 78.
- Reproducer: `SetParam_test_2.F90`.

The assignment is:

```fortran
obj%faceOrder(1:3, 1:SIZE(faceOrder)) = faceOrder(1:3, :)
```

For a `(3,3)` input, SIZE returns nine, although there are only three
columns. The test stops with:

```text
Fortran runtime error: Index '9' of dimension 2 of array 'obj'
outside of expected range (6:1)
```

Expected: use `SIZE(faceOrder,2)` for the active column extent and copy
the three input columns. This is an incorrect assignment extent on valid
input, rather than a missing release-mode input check.

### 3. MdEncode and ReactEncode crash in the base formatting utility

- Implementation: `BasisOpt_Class@IOMethods.F90`.
- Reproducers: `MdEncode_test_1.F90`, `ReactEncode_test_1.F90`.
- Related report: `bugs_report/AbstractFE.md`, item 1.

Both encoders stop while formatting an initialized quadratic triangle:

```text
Fortran runtime error: Substring out of bounds: upper bound (5) of
'fmt1' exceeds string length (4)
```

The diagnostic points to line 653 of easifemBase's
`src/modules/Display/src/disp/dispmodule_util.F90`, in `sszipfmt`.
The encoder calls the base ToString and table-encoding utilities. The
exact caller supplying the short format buffer remains to be isolated.

Expected: return an encoded string without a bounds error. The separate
stdout Display example passes.

## Incomplete operations

These are existing WIP placeholders, recorded separately from the bugs
above. Their examples intentionally expose the unsupported calls.

### 4. Line facet quadrature is not implemented

- Public binding: `Line_GetFacetQuadraturePoints`.
- Reproducer: `Line_GetQuadraturePoints_test_1.F90`.

Cell quadrature and the point-count checks pass, but the facet call
delegates to QuadratureOpt_'s line-facet placeholder, which reports
`[WIP ERROR] :: This routine is under development` and stops with 666.

### 5. Line local facet shape evaluation is not implemented

- Bindings: `LineH1LagFE_GetLocalFacetElemShapeData` and
  `LineH1HieFE_GetLocalFacetElemShapeData`.
- Reproducers: the corresponding `_test_1.F90` examples.

Both methods report `[WIP ERROR] :: This routine is under development`
and stop with 666. The tests call the shape placeholders directly so
the separate quadrature placeholder cannot mask these failures.

## Source-inspection notes

- Both encoders explicitly leave the faceOrder row blank, even when face
  orders are active. Runtime encoding currently stops at item 3 before
  this output can be checked.
- The private `obj_CheckErrorHierarchical` interface has no matching
  implementation in the active submodules and is not type-bound.
  The legacy ParameterConstructorMethods source is excluded from CMake.
  No public example calls this private declaration.
