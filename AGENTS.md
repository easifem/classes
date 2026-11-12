# AGENTS.md - easifemClasses

Object-oriented layer of EASIFEM (Fortran 2018, CMake + Ninja, GPL-3.0).
Depends on easifemBase; the kernels (elasticity, seepage, diffusion)
depend on it.

Conventions, build rules, docblock, test and version-control rules are
in the `easifem-dev` Claude plugin (skill `easifem-conventions`). Agents
without the plugin: read
`https://github.com/easifem/claude-plugins/tree/main/easifem-dev/skills`.

Before searching for code, read `architecture.md` (module map,
dependency direction, where to look, gotchas).

## Commands

- Build: `easifem dev classes`
- Install: `easifem install classes --no-download`
- Run tests: `easifem test ./` in an examples directory (needs its
  `runner.toml`)
- Lint: `fortitude check --no-fix <changed files>`
- Docs: `formal generate --project ford.md --output docs/api`
  (add the module to `src_dir` in `ford.md` first)

## Paths

- Interface: `src/modules/<Name>/src/<Name>_Class.F90`
- Implementation: `src/submodules/<Name>/src/<Name>_Class@<Category>.F90`
- Tests: `src/modules/<Name>/examples/<Name>_test_<N>.F90`, with
  `runner.toml` and the test meshes in `examples/meshdata/` of the same
  module
- Error helpers: `src/submodules/include/errors.F90`
- Docs output: `docs/api/` (git-ignored)
- Bug reports: `bugs-report/` (git-ignored)

## Dependencies

easifemBase, HDF5; optional GMSH SDK (`USE_GMSH_SDK`), LIS (`USE_LIS`).
Without `USE_LIS` the `*Lis` classes are built from their
`NO_<Name>_Class.F90` stubs.

## Version control

jj, colocated with git.
