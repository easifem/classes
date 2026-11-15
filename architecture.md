# easifemClasses architecture

A map of the repository structure. It does not document individual routines;
see the FORD docblocks and `docs/api/` for those, and the `easifem-dev`
plugin (skill `easifem-conventions`) for the coding rules.

## Directory layout

| Path | Contents |
|---|---|
| `CMakeLists.txt` | One library `easifemClasses`; needs `easifemBase` (`find_package`). Adds `DEBUG_VER` only for `CMAKE_BUILD_TYPE=Debug`. |
| `src/modules/<M>/` | Public interfaces: `CMakeLists.txt`, `src/`, and `examples/`. |
| `src/submodules/<M>/` | Implementations: `CMakeLists.txt` and `src/`. |
| `src/submodules/include/` | Shared include files (`errors.F90`, `deallocate_vector*.F90`, `display_vector*.F90`, `reallocate_vector*.F90`, ...). |
| `cmake/` | `Config.cmake.in` and `packaging.cmake`. |
| `doc/formal.md` | Project file of the doc generator; `src_dir` lists the documented modules. |
| `docs/` | Generated API pages (`docs/api/`) and notes. Ignored by git. |
| `python/` | Build/install scripts (pass `-D USE_LIS`, `-D USE_GMSH_SDK`). |
| `bugs_report/` | Per-module bug notes. |
| `AGENTS.md`, `architecture.md`, `fortitude.toml` | Repo facts for agents; this map; linter config (line length 78). |

## File naming pattern

- Interface: `src/modules/<M>/src/<M>_Class.F90` (the `TYPE` and the
  `INTERFACE` blocks of `MODULE SUBROUTINE`/`FUNCTION`).
- Implementation: `src/submodules/<M>/src/<M>_Class@<Category>Methods.F90`.
  Usual categories: `Constructor`, `Get`, `Set`, `IO`, `Toml`, `HDF`;
  plus domain ones (e.g. `FEDOF_Class@SetSparsityMethods.F90`).
- Examples/tests: `src/modules/<M>/examples/<Method>_test_<N>.F90`, run with
  `easifem run`; each examples folder has its own `runner.toml` and may have
  `meshdata/` and `toml/` input folders.
- Every source file is listed explicitly in the module's `CMakeLists.txt`
  (no globbing). A new submodule file must be added there.
- Other module names: `*Utility.F90` (free procedures), `*Factory.F90`
  (create objects from a name), `*_Method.F90` (wrappers), `NO_<M>_Class.F90`
  (stub used when an optional dependency is off).

## Module families

**Core and utilities**
- `ExceptionHandler`: global error/message object `e`; lowest layer.
- `CPUTime`, `HashTable`, `BinaryTree`, `LinkList`, `ftlMacros`: helpers and
  containers. `Toml`, `FPL`: configuration input.
- `EquationParser`, `UserFunction`: user functions from constants,
  expressions or procedures.
- `*Opt` classes (`BasisOpt`, `QuadratureOpt`, `FieldOpt`, `TimeOpt`,
  `LinSolverOpt`, ...): option sets read from TOML.
- `Engine`, `AbstractVector`, `Vector`: linear-algebra engine and vectors.

**File I/O**
- `AbstractFile` → `FortranFile` → `TxtFile` → `CSVFile`, `MarkdownFile`,
  `MSHFile`, ...; `AbstractFile` → `HDF5File` → `VtkHDF5File`;
  `AbstractFile` → `XMLFile` → `VTKFile`, `PVDFile`.
- `GmshAPI` and `Gmsh*`: bindings to the Gmsh SDK and mesh builders.

**Approximation**
- Polynomials: `AbstractFunction`, `AbstractBasis`, `Monomial*`,
  `Polynomial*`, `Lagrange*`, `AbstractOrthopol1D` (Jacobi, Chebyshev,
  Legendre, Ultraspherical), `PolynomialFactory`.
- `RefElement`: reference elements and topology of each shape.
- `AbstractFE` → `H1FE` (Lagrange, hierarchical and DG elements for each
  shape); `AbstractOneDimFE` (1D and time elements); `FEFactory`.

**Mesh, domain and DOF**
- `AbstractMesh` → `FEMesh`: mesh, node/element/face/edge data and their
  connectivity. `MeshSelection` picks element groups.
- `AbstractDomain` → `FEDomain`: the meshes of all dimensions read from
  HDF5. `DomainConnectivity`, `FEDomainConnectivity`: links between domains.
- `FEDOF`: spatial DOF numbering on a domain with an FE (continuous or
  broken), sparsity patterns. `TimeFEDOF`, `OneDimFEDOF`: time/1D DOFs.

**Physics data**
- `AbstractBC` → `DirichletBC` → `NeumannBC` → `NitscheBC`.
- Materials: `AbstractMaterialModel` → solid/fluid models (`LinearElastic`,
  `LinearPoroElastic`, `NewtonianFluid`), `CapillaryModel`,
  `AbstractMaterial` → `Solid`/`Fluid`/`PorousMaterial`, `MaterialFactory`.

**Fields and solvers**
- `AbstractField` → `AbstractNodeField` → `ScalarField`, `VectorField`,
  `STScalarField`, `STVectorField`, `BlockNodeField`.
- `AbstractField` → `AbstractMatrixField` → `MatrixField`,
  `BlockMatrixField`.
- Each node/matrix field has a `*Lis` subclass (LIS storage).
- `AbstractMeshField` → `MeshField`: element-wise data (e.g. material
  values at quadrature points). `FieldFactory` creates fields by name.
- `AbstractLinSolver` → `LinSolver` → `LinSolverLis`; `LinSolverFactory`.

**Top level**
- `Assemble{Mass,Diffusion,Stiffness}Matrix`: assembly utilities.
- `SDAlgorithms`, `CSDAlgorithms`, `TDGAlgorithms`: time integration
  options/coefficients.
- `VTKMeshUtility`, `VTKPlot`, `GnuPlot`, `PLPlot`: output and plotting.

## Dependency direction

Lower layers never use higher ones. Bottom to top:

```text
easifemBase (GlobalData, BaseType, *_Method, HDF5, toml-f, Sparsekit)
  └─ ExceptionHandler, CPUTime
      └─ AbstractFile → TxtFile / HDF5File → VTKFile / VtkHDF5File
          └─ Opt classes, EquationParser → UserFunction
              └─ Polynomials → RefElement → AbstractFE → H1FE
              └─ AbstractMesh → FEMesh
                  └─ AbstractDomain → FEDomain, MeshSelection
                      └─ FEDOF (domain + mesh + FE), TimeFEDOF
                          └─ AbstractBC → Dirichlet/Neumann/Nitsche
                          └─ AbstractMaterial → AbstractMeshField
                              └─ AbstractField → node / matrix fields
                                  └─ LinSolver, Assemble*, FieldFactory
                                      └─ kernels (elasticity, seepage,
                                         diffusion; separate repos)
```

The order in `src/modules/CMakeLists.txt` roughly follows these layers, but
it is one library target, so the build tool resolves the module order.

## Where to look for…

| Topic | Where |
|---|---|
| Type definition and docs of a method | `src/modules/<M>/src/<M>_Class.F90` |
| Code of a method | `src/submodules/<M>/src/<M>_Class@*Methods.F90` (grep `MODULE PROCEDURE obj_<Name>`) |
| How to call a method | `src/modules/<M>/examples/*_test_*.F90` |
| Assertion helpers | `src/submodules/include/errors.F90` |
| Build options, compile flags | `CMakeLists.txt`, `python/build.py` |
| Which source files are compiled | `src/{modules,submodules}/<M>/CMakeLists.txt` |
| Optional LIS/Gmsh code | `*Lis` modules, `NO_*_Class.F90`, `GmshAPI/` |
| TOML input format of a class | `<M>_Class@TomlMethods.F90`, `examples/toml/` |
| Kinds, base types, CSR matrices | easifemBase (`../base`) |
| Doc generation | `doc/formal.md` → `docs/api/` |

## Gotchas

- Error checks, `[START]`/`[END]` messages and the `errors.F90` asserts exist
  only when `DEBUG_VER` is defined (Debug build). Release does no checks.
- `USE_LIS`, `USE_GMSH_SDK`, `USE_PLPLOT` come from easifemBase's exported
  definitions or `-D` options; with an option off, CMake compiles the
  `NO_*`/`NoGmsh` stub instead of the real module.
- Several directories exist but are not built (e.g. `FiniteElement`,
  `MixedFiniteElement`, `TensorField`, `Fhash`, `EdgeList`, `easifemClasses`,
  `oldLibrary`, `XDMFFile`, empty `Mesh`, `MeshData`, `Kernel`). Check the
  `CMakeLists.txt` before editing a module.
- Legacy modules are still built (`Field`, `BoundaryCondition`, `Element`,
  `ElementFactory`, `Files`, `Plot`). Prefer the `Abstract*` hierarchies.
- Some module folders hold many types: `AbstractMesh` (node/elem/face/edge
  data, lists, trees), `RefElement` (all shapes), `H1FE` (one folder per
  shape), `GmshAPI`.
- Some full modules live under `src/submodules/` (`AbstractMeshUtility`,
  `BasisOptUtility`, `MatrixFieldUtility`, `GmshUtility`, `CapillaryFactory`).
- Surprising inheritance: `NeumannBC_` extends `DirichletBC_`;
  `LinearPoroElasticModel_` extends `LinearElasticModel_`.
- Very large files: `AbstractMesh_Class.F90`, `HDF5File_Class.F90`,
  `BasisOpt_Class.F90`, `AbstractFE_Class.F90`, `FEDOF_Class.F90`. Read them
  by section, not whole.
- Lowercase `.f90` files are legacy (`oldLibrary`); new code uses `.F90`.
- `examples/**/CMakeLists.txt` and `tests/` are ignored by git; there is no
  formal test framework, the example programs with `CALL OK(...)` are the
  tests.
- `doc/formal.md` documents only the modules in its `src_dir`.
- `docs/` is ignored by git.
