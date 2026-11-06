# AGENTS.md - easifemClasses

Object-oriented layer of EASIFEM (Fortran 2018, CMake + Ninja, GPL-3.0).
Depends on easifemBase; the kernels (elasticity, seepage, diffusion)
depend on it.

Conventions, build rules, docblock and test rules are in the
`easifem-dev` Claude plugin (skill `easifem-conventions`). Agents without
the plugin: read
`~/Dropbox/easifem/easifem-claude-plugins/easifem-dev/skills/`.

Before searching for code, read `architecture.md` (module map,
dependency direction, where to look, gotchas).

## Commands

- Build: `easifem dev classes`
- Install: `easifem install classes --no-download`
- Lint: `fortitude check --no-fix <changed files>`
- Docs: `formal generate --project doc/formal.md`

## Paths

- Interface: `src/modules/<Name>/src/<Name>_Class.F90`
- Implementation: `src/submodules/<Name>/src/<Name>_Class@<Category>.F90`
- Tests: `src/modules/<Name>/examples/<Name>_test_<N>.F90`
- Error helpers: `src/submodules/include/errors.F90`
- Docs output: `docs/api/` (git-ignored)

## Dependencies

easifemBase, HDF5; optional GMSH SDK (`USE_GMSH_SDK`), LIS (`USE_LIS`).
