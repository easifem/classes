# easifemClasses

**The object-oriented core of [EASIFEM](https://www.easifem.com): meshes,
domains, degrees of freedom, finite elements, fields, materials and
solvers, ready to compose into a finite element application.**

[![License: GPL-3.0](https://img.shields.io/badge/license-GPL--3.0-blue.svg)](LICENSE.gpl3.md)
[![Fortran 2018](https://img.shields.io/badge/Fortran-2018-734f96.svg)](https://fortran-lang.org/)
[![Version](https://img.shields.io/badge/version-26.04.0-blue.svg)](CMakeLists.txt)
[![Part of EASIFEM](https://img.shields.io/badge/part%20of-EASIFEM-orange.svg)](https://www.easifem.com)

`easifemClasses` is the middle layer of the EASIFEM platform, written in
modern Fortran (2018) with an object-oriented design. It builds on
[easifemBase](https://github.com/easifem/base) and turns the pieces of a
finite element code into reusable, extensible objects. The physics kernels
([elasticity](https://github.com/easifem/elasticityKernel),
[seepage](https://github.com/easifem/seepageKernel),
[diffusion](https://github.com/easifem/diffusionKernel)) and your own
applications build on top of it.

> EASIFEM stands for **E**xpandable **A**nd **S**calable
> **I**nfrastructure for **F**inite **E**lement **M**ethods.

## Where it fits

![Where easifemClasses fits: applications and physics kernels build on
easifemClasses, which builds on easifemBase and the system numerical
libraries](figures/easifem-classes-layers.svg)

The physics kernels and applications use the objects in this repository;
this repository uses `easifemBase`, which wraps the numerical libraries.

## Why use it

- **One pipeline, from file to field.** Read a mesh, build a domain, number
  the DOFs, assemble a matrix, solve, and write VTK — with the same set of
  objects.
- **High-order and discontinuous spaces.** Lagrange, hierarchical and DG
  elements, continuous or broken DOF numbering, and variable order.
- **Multiphysics building blocks.** Boundary conditions, material models,
  node fields, matrix fields and element-wise mesh fields.
- **Designed to be extended.** Most families have an `Abstract*` base
  class, and the common ones have factories and TOML-driven options, so
  new elements, materials and solvers can be added without touching the
  callers.

## What's inside

| Area                     | Representative types                                                           | What it gives you                                          |
| ------------------------ | ------------------------------------------------------------------------------ | ---------------------------------------------------------- |
| **File I/O**             | `HDF5File_`, `VTKFile_`, `MSHFile_`, `CSVFile_`                                | HDF5, VTK/PVD, Gmsh `.msh`, CSV and Markdown files         |
| **Meshes**               | `AbstractMesh_`, `FEMesh_`                                                     | Nodes, elements, faces, edges, connectivity and groups     |
| **Domains**              | `FEDomain_`, `MeshSelection_`                                                  | All dimension meshes of a model and named region selection |
| **Approximation**        | `RefLine_`, `RefQuadrangle_`, `LineH1FE_`, `QuadrangleH1FE_`                   | Reference shapes, polynomials and H1 elements              |
| **DOF numbering**        | `FEDOF_`, `TimeFEDOF_`, `OneDimFEDOF_`                                         | Spatial, time and 1D numbering; sparsity patterns          |
| **Boundary conditions**  | `DirichletBC_`, `NeumannBC_`, `NitscheBC_`                                     | Strong, weak and Nitsche enforcement                       |
| **Materials**            | `LinearElasticModel_`, `NewtonianFluidModel_`, `GardnerModel_`                 | Solid, fluid, porous and capillary models                  |
| **Fields**               | `ScalarField_`, `VectorField_`, `STScalarField_`, `MatrixField_`, `MeshField_` | Node, matrix and element-wise data, with LIS variants      |
| **Solvers and assembly** | `LinSolver_`, `LinSolverLis_`, `AssembleMassMatrix`                            | Linear solvers and mass, stiffness and diffusion assembly  |
| **Utilities**            | `ExceptionHandler_`, `EquationParser_`, `UserFunction_`, `VTKPlot_`            | Error handling, expressions, user functions and plotting   |

The tree contains 1,800+ Fortran sources and 700+ example programs, many
of them self-checking.

## Quick start

### Requirements

| Requirement   | Notes                               |
| ------------- | ----------------------------------- |
| GFortran 9+   | GNU builds use `-std=f2018`         |
| CMake 3.20+   | Ninja is recommended                |
| HDF5          | Provided through `easifemBase`      |
| `easifemBase` | Required; must be findable by CMake |

Optional integrations are described in [Optional features](#optional-features).

### Install with the EASIFEM CLI

The easiest route is the `easifem` command-line tool, which knows how to
build and install the whole stack.

```bash
# 1. Install the base library once
easifem install base

# 2. Clone this repository and install classes
git clone https://github.com/easifem/classes.git
cd classes
easifem install classes --no-download
```

If the EASIFEM directories are not configured yet, run `easifem setenv`
once first; see `easifem setenv --help`.

### Build with CMake

```bash
cmake -S . -B build -G Ninja -D CMAKE_BUILD_TYPE=Release
cmake --build build --parallel
cmake --install build --prefix /path/to/easifem
```

If CMake cannot find `easifemBase`, add its install prefix to
`CMAKE_PREFIX_PATH`.

### Use it from another project

```cmake
find_package(easifemClasses REQUIRED)
target_link_libraries(my_app PRIVATE easifemClasses::easifemClasses)
```

### A first program

This reads an HDF5 mesh, builds a domain, creates a second-order
hierarchical H1 space, and prints the number of degrees of freedom.

```fortran
PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE HDF5File_Class, ONLY: HDF5File_
  IMPLICIT NONE

  TYPE(FEDOF_) :: fedof
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile

  CALL meshfile%Initiate("mesh.h5", mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, "")

  CALL fedof%Initiate(baseContinuity="H1", &
                      baseInterpolation="Hierarchical", &
                      order=2, dom=dom)

  PRINT *, "Degrees of freedom:", fedof%GetTotalDOF()

  CALL fedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
```

Adapted from `src/modules/FEDOF/examples/GetOrders_test_1.F90`.

## Developing the library

Build this checkout in debug mode (checks enabled, nothing installed):

```bash
easifem dev classes
```

Run the examples for a module. Each examples directory has its own
`runner.toml`; the programs assert their own results and double as tests.

```bash
cd src/modules/FEDOF/examples
easifem test ./
```

Lint changed sources:

```bash
fortitude check --no-fix <changed files>
```

Generate the API reference with [FORD](https://forddocs.readthedocs.io/):

```bash
formal generate --project ford.md --output docs/api
```

## Repository layout

```text
src/modules/<Module>/src/<Module>_Class.F90      public types and interfaces
src/modules/<Module>/examples/                   runnable examples and tests
src/submodules/<Module>/src/<Module>_Class@*.F90 implementations
src/submodules/include/                          shared error/display helpers
cmake/                                           CMake package configuration
python/                                          build and install helpers
docs/                                            generated API pages (git-ignored)
```

Every source file is listed explicitly in its module `CMakeLists.txt`.
See [architecture.md](architecture.md) for the dependency order and a map
of where to find things.

## Optional features

- **LIS** — iterative solvers and the `*Lis` field and solver classes.
  When LIS is off, stub `NO_*_Class.F90` implementations are compiled
  instead.
- **Gmsh SDK** — mesh generation through the `GmshAPI` bindings.
- **ParaView Catalyst** — in situ visualization; enable with
  `-D USE_CATALYST=ON`.
- **PLPlot** — plotting through the `PLPlot` and `EasyPlplot` modules.

`USE_LIS`, `USE_GMSH_SDK` and `USE_PLPLOT` are exported by
`easifemBase`, so configure them when building the base library.

## Documentation and community

- **Website and guides:** <https://www.easifem.com>
- **API reference:** generate locally with `formal` (see above)
- **Cookbook:** <https://github.com/easifem/cookbook>
- **Issues and questions:**
  <https://github.com/easifem/classes/issues>
- **Base library:** <https://github.com/easifem/base>

## Contributing

Contributions are welcome. Development happens on the `dev` branch, so
please base pull requests there.

- Read [architecture.md](architecture.md) before adding a type, and
  [AGENTS.md](AGENTS.md) for the repository conventions.
- Use Fortran 2018 free form, 78-column lines, and FORD docblocks.
- Add a new submodule file to the module `CMakeLists.txt`.
- Add an example `<Method>_test_<N>.F90`; the examples are the tests.
- Run `fortitude check --no-fix` on the files you changed.

## License

easifemClasses is free software under the GNU General Public License,
version 3 or later. See [LICENSE.gpl3.md](LICENSE.gpl3.md) and
[LICENSE](LICENSE).

---

**EASIFEM** — Expandable And Scalable Infrastructure for Finite Element
Methods. Written in modern Fortran; built to keep finite element
programming easy and elegant.
