# Bug report: FEDOF_

- Module: `src/modules/FEDOF/src/FEDOF_Class.F90`
- Submodules: `src/submodules/FEDOF/src/`
- Tests: `src/modules/FEDOF/examples/`
- Date: 2026-11-07

Findings come from a source review of the module against its submodules
and callees. Nothing was compiled or run: "Expected" test results are
predictions from reading the code.

Fixed in the source and removed from this report: H-01, H-03, H-04, H-05,
M-01 to M-07, L-01 to L-11. Their tests have not been run yet. These IDs
are not reused.

## High

### H-02 — SetFE loses per-face orders of variable-order hierarchical H1

- **Where:** `FEDOF_Class@SetMethods.F90:obj_SetFE`
- **Problem:** `SetFE` always passes `order=cellOrder(1)`. The hierarchical
  `SetOrder` treats a present `order` as uniform, and sets every face to
  the cell order. A high-order cell next to a low-order cell then gets a
  basis larger than its connectivity, which uses the minimum neighbouring
  face order.
- **Evidence:** `TriangleH1HierarchicalFE_Class@Methods.F90:obj_SetOrder`
  returns early when `order` is present. Commit `047ec588` removed the
  `H1`, `HIER`, 2D branch that omitted `order`. The docblock of `obj_SetFE`
  still says to pass cell and face orders without the scalar shortcut.
  Expected to fail: `H1VariableOrder_test_1`, "FE preserves minimum facet
  orders".
- **Suggested fix:** (a) restore the old `H1`, `HIER`, 2D branch, which
  calls `SetOrder` without `order`. No broken-space condition is needed:
  `GetOrders` now returns the cell order for broken cells. Check
  `SetFE_test_1` test 2 afterwards, because broken hierarchical cells then
  use the `cellOrder` and `faceOrder` path instead of the shortcut.

## Low

### L-12 — baseType key in tables outside FEDOF

- **Where:** `OneDimBasisOpt.toml:5`, `AbstractOneDimFE.toml:5`, and 17
  tables in the `examples` of `ScalarField`, `VectorField`, `VtkHDF5File`,
  `MeshField`, `AbstractNodeField`, `STVectorField`, `VTKMeshUtility` and
  `OneDimFEDOF`.
- **Problem:** the tables write `baseType`, but the code reads `basisType`,
  so the key is ignored. Every value is `Monomial`, which is also the
  default, so results do not change today. A user who copies a template and
  changes the value would get the default without an error.
- **Evidence:** `BasisOpt_Class@TomlMethods.F90` and
  `OneDimBasisOpt_Class@TomlMethods.F90` read `key="basisType"`.
  `rg '^baseType' src -g '*.toml'` finds 38 lines in 19 files, all
  `Monomial`.
- **Suggested fix:** (a) rename the key in all 19 files. This touches more
  than 10 files, so list them for approval first.
