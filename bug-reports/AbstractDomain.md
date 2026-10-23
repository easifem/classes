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

### 4. entityNum is not used

- Where: `GetNptrs`, `GetNptrs_`, `GetInternalNptrs` and 15 other methods
  took `entityNum` and only passed it to `GetMeshPointer`, which ignored
  it; the whole mesh of dimension `dim` was used for any `entityNum`.
- Fix: `entityNum` is removed from these methods (`GetMeshPointer`,
  `IsElementPresent`, `GetConnectivity`, `GetConnectivity_`, `GetNNE`,
  `GetTotalNodes`, `GetNodeCoord`, `GetNptrs`, `GetNptrs_`,
  `GetInternalNptrs`, `GetBoundingBox`, `GetTotalMaterial`,
  `GetTotalVertexNodes(globalElement(:))`, `GetOrientation`,
  `GetElemTopologyIndx`, `IsElementActive`, `SetTotalMedium`,
  `SetQuality`) and the operator `.tNodes. [dim, entityNum]` is removed.
  `GetMeshPointer` also loses the unused `globalElement` and `islocal`.
  `entityNum` is kept where it selects elements by mesh id:
  `GetTotalElements`, `SetMaterial`, `GetTotalVertexNodes`. For the nodes
  of one entity use `GetMeshPointer(dim)` and the mesh method
  `GetNptrs(meshid=entityNum)`.

### 5. GetDimEntityNum always returns entity 1 for FEDomain_

- Where: `obj_GetDimEntityNum` in `AbstractDomain_Class@GetMethods.F90`
- Cause: it looped over entity numbers and took the first one whose mesh
  contained the element; the mesh of a dimension holds all entities.
- Fix: it returns `[dim, Elemdata_meshID(element)]`.
- Test: `GetDimEntityNum_test_1` (element 2 is on point 2, not 1),
  `GetDimEntityNum_test_2` (two regions: element 20 gives `[2, 2]`)

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
