# Bug report: AbstractNodeField_

- Module: `src/modules/AbstractNodeField/src/AbstractNodeField_Class.F90`
- Submodules: `src/submodules/AbstractNodeField/src/`
- Tests: `src/modules/AbstractNodeField/examples/` (the fields are
  `ScalarField_`)
- Date: 2026-10-28

This report comes from reading the code while writing the docs and tests;
none of the items is changed yet. The docblocks describe the present
behaviour.

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Fixed

- 1. scale was ignored by SetMultiple without addContribution: now the
  values are set to scale * VALUE, as in SetSingle and SetAll (entries
  set to zero, then scale * VALUE added; no temporary array). The
  ScalarField_ and VectorField_ Set methods that call SetMultiple follow.
  Test: SetMultiple_test_2 (2026-10-29).
- 3. The tests in examples/ are run: all 16 pass (2026-10-29).

## Open

### 2. Routines that only raise an error

- `obj_Initiate3` (ConstructorMethods:331): under development.
- `obj_GetFEVariable` (GetMethods:66): must be implemented by the child.
- `obj_ExportToVTK` (VTKMethods:69): must be implemented by the child.
- Not bugs as such; listed so that they are not forgotten.
