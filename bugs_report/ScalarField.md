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

- 1. ScalarFieldDeallocate did nothing: `obj_Deallocate_ptr_vector` now
  includes `deallocate_vector_ptr.F90` (found fixed on 2026-10-29).
- 2. ApplySurfaceNeumannBC2 ignored its nbc argument: both loops now use
  `nbc(ibc)%ptr` (2026-10-29; builds, no test yet).
- 4. Get4 FEVariable tags: not a bug. `TypeFieldOpt%nodal`, `%space`
  and `%scalar` are defined from `TypeFEVariableOpt` in
  `FieldOpt_Class`, so the values are the same.

- 3. scale without addContribution is applied now (see
  AbstractNodeField.md, item 1) (2026-10-29).

## Open

### 5. islocal = .FALSE. is not supported

- Set1, Set4, Set5, Get1, Get3, Get4 (and Set6, GetFEVariable through
  them) use globalNode as local node number; in debug mode an error is
  raised if islocal is false (`include/localNodeError.F90`).
- Not a bug as such; the argument is misleading.
