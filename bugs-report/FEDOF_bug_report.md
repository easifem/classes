# Bug report: FEDOF_

- Module: `src/modules/FEDOF/src/FEDOF_Class.F90`
- Submodules: `src/submodules/FEDOF/src/`
- Tests: `src/modules/FEDOF/examples/`
- Date: 2026-11-07

Findings come from a source review of the module against its submodules
and callees.

Fixed in the source and removed from this report: H-01 to H-05, M-01 to
M-07, L-01 to L-11. Their tests pass. These IDs are not reused.

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
