# Bug report: AbstractDomain_

- Module: `src/modules/AbstractDomain/src/AbstractDomain_Class.F90`
- Submodules: `src/submodules/AbstractDomain/src/`
- Tests: `src/modules/FEDomain/examples/` (FEDomain_ is the concrete domain)
- Date: 2026-10-22

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Fixed

### 1. GetNptrsInBox crashes if nptrs is allocated

- Where: `obj_GetNptrsInBox` in `AbstractDomain_Class@GetMethods.F90`
- Symptom: `Fortran runtime error: Attempting to allocate already
  allocated variable 'nptrs'`.
- Cause: `ALLOCATE (nptrs(tnodes))` on an `INTENT(INOUT)` allocatable
  argument without a check (same bug as in AbstractMesh_).
- Fix: deallocate `nptrs` first if allocated.
- Test: `GetNptrsInBox_test_1`

## Open

### 2. SetFacetElementType crashes (removed)

- Where: `obj_SetFacetElementType` in `AbstractDomain_Class@MeshDataMethods.F90`
- Cause: it called `SetFacetElementType` of the mesh, which writes to the
  never allocated mesh component `facetElementType` (see
  `AbstractMesh.md`, bug 10). It also marked every boundary facet of the
  nsd mesh as `BOUNDARY_ELEMENT`, including facets on the domain boundary.
- Resolution: removed (binding, interface, implementation), together with
  `InitiateBoundaryData`, its only caller.

### 3. Routines which are not implemented

| Routine | Behaviour |
|---|---|
| `obj_GetElemType` | raises "under development" |
| `obj_GetUniqueElemType` | calls `GetElemType`, so it fails too |
| `obj_GetTotalMeshFacetData` | raises "deprecated" |
| `obj_SetQuality` | raises "[WIP ERROR]" |
| `obj_SetDomainFacetElement` | empty body |
| `obj_SetMeshmap` | empty body |
| `obj_SetMeshFacetElement` | empty body |

`obj_GetOrder` was removed (binding, interface and implementation). It
was not implemented (it returned 0 and raised an error only in debug
builds), FEDomain_ did not override it, and nothing in the library called
it. The old packages (MovingMesh, StokesFlow, DBE) call
`dom%GetOrder(dim=...)`; they need the mesh `GetOrder()` instead.

### 4. entityNum is not used

- Where: `obj_GetNptrs`, `obj_GetNptrs_`, `obj_GetInternalNptrs` (they
  call `GetMeshPointer(dim=dim)` only).
- Symptom: the nodes of the whole mesh of dimension `dim` are returned
  for any `entityNum`.

### 5. GetDimEntityNum always returns entity 1 for FEDomain_

- Where: `obj_GetDimEntityNum` in `AbstractDomain_Class@GetMethods.F90`
- Symptom: element 20 of `very_small_quad4_mesh_two_region.h5` is in
  surface entity 2, but the result is `[2, 1]`.
- Cause: it loops over `entityNum` and calls
  `GetMeshPointer(dim, entityNum)`, but `FEDomain_%GetMeshPointer` ignores
  `entityNum` (one mesh per dimension holds all entities), so entity 1
  always matches.
- Proposal: in FEDomain_, find the mesh of dimension dim and return the
  mesh id of the element (`Elemdata_Meshid`).

### 6. AbstractDomainSetSparsity gives no entries for one domain

- Where: `obj_SetSparsity2` (`part1_obj_Set_sparsity2`) in
  `AbstractDomain_Class@SetMethods.F90`
- Symptom: with one domain (quad mesh, one scalar DOF) `GetNNZ(mat)` is 0
  after the call; `SetSparsity1` gives 49 for the same matrix.
- Status: not investigated; it uses `FEDomainConnectivity_` and
  `SetSparsity3` of the mesh. No test was written.

### 7. Minor

- `GetNodeCoord(dim=...)` returns three rows, but `GetNodeCoord()` (all
  nodes), `GetNodeCoord(globalNode(:))` and `GetNodeCoord(globalNode)`
  return `nsd` rows.
- `GetParam(nodeCoord=...)` uses an assumed-shape dummy; the caller must
  give the exact shape of the node coordinates.
- Component `isNodeNumberSparse` is declared `LOGICAL(I4B)`; the other
  flags are `LOGICAL(LGT)`.
- `SetTotalElements` changes `tElements(indx)` only; `GetTotalElements`
  reads the meshes, so the two can disagree.
