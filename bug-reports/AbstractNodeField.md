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

None.

## Open

### 1. scale is ignored by SetMultiple without addContribution

- Where: `AbstractNodeField_Class@SetMethods.F90`, `obj_SetMultiple1`
  (line 142), `obj_SetMultiple2` (175), `obj_SetMultiple3` (210),
  `obj_SetMultiple4` (246). Without `addContribution` they call
  `RealVectorSet(..., VALUE=VALUE, ...)`; `scale` is not used.
- Inconsistent with `obj_SetSingle` (111) and `obj_SetAll` (279), which
  set `scale * VALUE`.
- Effect: `SetMultiple(..., scale=2)` sets VALUE, not 2 * VALUE. The
  children inherit it (ScalarField_ Set3/Set5/Set7/Set8, VectorField_
  Set1/Set3/Set4/Set5/Set8/Set9).
- Fix (proposal): multiply by scale (or decide that scale is only used
  with addContribution, and say so in all Set methods).
- Tests: none yet (the tests use scale only with addContribution).

### 2. Routines that only raise an error

- `obj_Initiate3` (ConstructorMethods:331): under development.
- `obj_GetFEVariable` (GetMethods:66): must be implemented by the child.
- `obj_ExportToVTK` (VTKMethods:69): must be implemented by the child.
- Not bugs as such; listed so that they are not forgotten.

### 3. AbstractNodeField tests not run

- The 15 tests in `src/modules/AbstractNodeField/examples/` are not run
  yet.
