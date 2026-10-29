# Bug report: VectorField_

- Module: `src/modules/VectorField/src/VectorField_Class.F90`
- Submodules: `src/submodules/VectorField/src/`
- Tests: `src/modules/VectorField/examples/` (`run_examples.fish`;
  2026-10-28: 9 of 10 pass, Set_test_2 crashes, see item 1)
- Date: 2026-10-28

This report comes from reading the code while writing the docs and tests;
none of the items is changed yet. The docblocks describe the present
behaviour.

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Fixed

- 1. Set2 failed in debug mode: the check is now
  `obj%fieldType .NE. TypeFieldOpt%constant` (2026-10-29).
  Test: Set_test_2.
- 3. Get4: engine branches swapped; NATIVE_SERIAL now calls `GetValue_`
  and the other engines use `GetMultiple` (2026-10-29).

## Open

### 6. SetByFunction swaps edge DOF on triangles of order >= 3

- Where: `obj_SetByFunction` (VectorField and ScalarField SetMethods),
  through `feptr%GetDOFValue`.
- Effect: on small_tri3_mesh.h5 with order 3 Lagrange, the two interior
  DOF of some edges get each other's value (44 of 140 VTK points wrong
  for f = x**2 y + 1, which is exact in the space). The VTK path is
  right: with SetHigherOrderNodeCoord (u = x) all points match.
  Probably the edge orientation is not applied in GetDOFValue for
  triangles. Quadrangles (square_3x2, two elements) are not affected.
- Tests: GetVtkUnstructuredValue_test_2 uses SetHigherOrderNodeCoord
  for this reason; a SetByFunction test on triangles of order 3 is
  needed.

### 2. Set6 is under development

- Where: `VectorField_Class@SetMethods.F90:326`; it raises an error.
- Also `VectorField_Class@TomlMethods.F90:138` raises a WIP error.

### 4. scale is ignored without addContribution

- Set1, Set3, Set4, Set5, Set8, Set9 use SetMultiple, which ignores scale
  without addContribution (see AbstractNodeField.md, item 1); Set10 and
  Set12 use it.

### 5. islocal = .FALSE. is not supported

- Set1, Set9, Set10, Get3, Get4, Get5 raise an error in debug mode if
  islocal is false (`localNodeError.F90`).
