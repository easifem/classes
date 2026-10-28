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

None.

## Open

### 1. Set2 (all nodes to one vector) fails in debug mode

- Where: `VectorField_Class@SetMethods.F90:132`:
  `CALL AssertError1(obj%fieldType .EQ. TypeFieldOpt%constant, ...)`
  with the message "Not callable for constant VectorField_".
- Effect: in a debug build `Set(VALUE(:))` raises an error for every
  normal field:
  `[INTERNAL ERROR] :: Not callable for constant VectorField_`.
- Fix: `.NE.` instead of `.EQ.`.
- Tests: Set_test_2 (crash).

### 2. Set6 is under development

- Where: `VectorField_Class@SetMethods.F90:326`; it raises an error.
- Also `VectorField_Class@TomlMethods.F90:138` raises a WIP error.

### 3. Get4: engine branches swapped

- Where: `VectorField_Class@GetMethods.F90:268-282`. For an engine which
  is not NATIVE_SERIAL it calls `GetValue_` on realVec; for
  NATIVE_SERIAL it calls `me_if_not_native`.
- Effect: none for NATIVE_SERIAL (both give the same values), but the
  test and the name are inverted.
- Fix: `.EQ.` instead of `.NE.` (or swap the branches).

### 4. scale is ignored without addContribution

- Set1, Set3, Set4, Set5, Set8, Set9 use SetMultiple, which ignores scale
  without addContribution (see AbstractNodeField.md, item 1); Set10 and
  Set12 use it.

### 5. islocal = .FALSE. is not supported

- Set1, Set9, Set10, Get3, Get4, Get5 raise an error in debug mode if
  islocal is false (`localNodeError.F90`).
