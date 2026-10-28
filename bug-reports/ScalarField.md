# Bug report: ScalarField_

- Module: `src/modules/ScalarField/src/ScalarField_Class.F90`
- Submodules: `src/submodules/ScalarField/src/`
- Tests: `src/modules/ScalarField/examples/` (`run_examples.fish`,
  8 tests pass on 2026-10-28)
- Date: 2026-10-28

This report comes from reading the code while writing the docs and tests;
none of the items is changed yet. The docblocks describe the present
behaviour.

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Fixed

None.

## Open

### 1. ScalarFieldDeallocate does nothing

- Where: `ScalarField_Class@ConstructorMethods.F90:87`,
  `obj_Deallocate_ptr_vector`; the body is empty.
- Effect: `ScalarFieldSafeAllocate` (line 121) calls it and then
  `ALLOCATE (obj(newsize))` on an array which is still allocated: run
  time error when the vector of pointers grows.
- Fix: `#include "../../include/deallocate_vector_ptr.F90"`, as in
  `VectorField_Class@ConstructorMethods.F90`.
- Tests: none yet.

### 2. ApplySurfaceNeumannBC2 ignores its nbc argument

- Where: `ScalarField_Class@SurfaceNBCMethods.F90:161, 175, 197`.
  `tbc = SIZE(nbc)`, but both loops use `obj%GetNBCPointer(ibc)`, the
  boundary conditions of the field.
- Effect: the given list is not used; with more entries in nbc than in
  the field, the pointer is not associated (skipped) or wrong.
- Fix: use `nbc(ibc)%ptr` in both loops.
- Tests: none yet.

### 3. scale is ignored without addContribution

- Set3, Set5, Set7 and Set8 call SetMultiple, which ignores scale
  without addContribution (see AbstractNodeField.md, item 1); Set1 and
  Set2 use it.

### 4. Get4 sets the FEVariable tags with TypeFieldOpt constants

- Where: `ScalarField_Class@GetMethods.F90:157-159`
  (`VALUE%defineOn = TypeFieldOpt%nodal`, `varType = TypeFieldOpt%space`,
  `RANK = TypeFieldOpt%scalar`).
- `TypeFieldOpt%space` equals `TypeFEVariableOpt%space`; check that
  `nodal` and `scalar` also have the FEVariable values (or use
  `TypeFEVariableOpt`).
- Tests: Set_test_3 checks only `len` and `val`.

### 5. islocal = .FALSE. is not supported

- Set1, Set4, Set5, Get1, Get3, Get4 (and Set6, GetFEVariable through
  them) use globalNode as local node number; in debug mode an error is
  raised if islocal is false (`include/localNodeError.F90`).
- Not a bug as such; the argument is misleading.
