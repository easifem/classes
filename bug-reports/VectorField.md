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

- 6. SetByFunction swapped the edge DOF on triangles of order >= 3:
  `TriangleH1LagFE_GetGlobalFacetElemShapeData` used `n2 = nns` of the
  geometry facet (2), so the facet shape functions of a reversed edge
  were never reversed; now `n2 = facetElemsd%nns`, as for quadrangles
  (2026-10-29). Tests: SetByFunction_test_2 of ScalarField and
  VectorField.

## Open

### 7. dNdXi of reversed edge DOF is reversed twice

- Where: `BasisOpt_Class@TriangleH1LagrangeFEMethods.F90` and
  `BasisOpt_Class@QuadrangleH1LagrangeFEMethods.F90`
  (`*_GetGlobalElemShapeData` and `*_GetGlobalFacetElemShapeData`).
  For an edge with orientation -1, `Reverse(ans=elemsd%dNdXi, ...)` is
  called twice, so dNdXi is not reversed (N and dNdXt are).
- Effect: dNdXi of the edge DOF does not match N for elements of order
  >= 3 with reversed edges. dNdXt is right, so the usual integrals are
  not affected; code which uses dNdXi directly is.
- Fix: remove the second call (or reverse dNdXt in its place if that
  was meant).
- Tests: none yet.

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
