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
- DomainConnectivity_ (follow-up): `InitiateFacetToCellData2/4` looped
  over the entities and called `GetMeshPointer` for each, which gave the
  same mesh every time. Data2 stored the last loop index as the entity of
  every facet; Data4 counted the cells of a facet again on every pass and
  raised "more than 2 cell element" for two or more entities. Both now do
  one pass, and `facet_to_cell_helper`, Data3 and Data4 store the mesh id
  of the cell element as its entity. `entityNum` (or `entityNum1/2`) is
  removed from `InitiateFacetToCellData1/3`, `InitiateNodeToNodeData1`
  and `InitiateCellToCellData1`, where it was not used or was wrong for
  meshes with more than one entity.

### 5. GetDimEntityNum always returns entity 1 for FEDomain_

- Where: `obj_GetDimEntityNum` in `AbstractDomain_Class@GetMethods.F90`
- Cause: it looped over entity numbers and took the first one whose mesh
  contained the element; the mesh of a dimension holds all entities.
- Fix: it returns `[dim, Elemdata_meshID(element)]`.
- Test: `GetDimEntityNum_test_1` (element 2 is on point 2, not 1),
  `GetDimEntityNum_test_2` (two regions: element 20 gives `[2, 2]`)

### 6. AbstractDomainSetSparsity gives no entries for one domain

- Where: `obj_SetSparsity2` (`part1_obj_Set_sparsity2`) in
  `AbstractDomain_Class@SetMethods.F90`, through
  `FEDomainConnectivity_%InitiateNodeToNodeData`.
- Symptom: with one domain (quad mesh, one scalar DOF) `GetNNZ(mat)` was 0;
  `SetSparsity1` gives 49.
- Cause 1: `InitiateNodeToNodeData1/2` (FEDomainConnectivity NodeMethods)
  compared all 3 components of the local arrays `x1(3)` and `x2(3)`, but
  `GetNodeCoord` and `GetNearestNode` set only the first nsd. For 2D the
  third components were not initialised, so no node matched and
  `nodeToNode` stayed zero; the mesh `SetSparsity3` then skipped every row.
- Cause 2: `nodeToNode` was indexed by the local node number of domain1
  (mesh1), but documented and used by the mesh `SetSparsity3` with the
  global node number.
- Fix: compare only the first nsd components (arrays set to zero first);
  index `nodeToNode` by the global node number (size: largest node number)
  in both routines, and in the `InitiateCellToCellData1/2` consumers.
- Test: `SetSparsity_test_2` (49 nonzeros with one domain)

### 3. Routines which are not implemented

- Removed (binding, interface and implementation): `GetElemType` and
  `GetUniqueElemType` (raised "under development"),
  `GetTotalMeshFacetData` (raised "deprecated"), `SetQuality` (raised
  "[WIP ERROR]"), `SetDomainFacetElement`, `SetMeshmap` and
  `SetMeshFacetElement` (empty bodies). Nothing in the library called
  them; FEDomain_ did not override them.
- `GetOrder` was removed earlier for the same reason.
- StokesFlow calls `GetTotalMeshFacetData` and `SetMeshFacetElement`; it
  needs a replacement when it is ported (it does not build against the
  current library anyway).

### 2. SetFacetElementType crashes (removed)

- Where: `obj_SetFacetElementType` in `AbstractDomain_Class@MeshDataMethods.F90`
- Cause: it called `SetFacetElementType` of the mesh, which writes to the
  never allocated mesh component `facetElementType` (see
  `AbstractMesh.md`, bug 10). It also marked every boundary facet of the
  nsd mesh as `BOUNDARY_ELEMENT`, including facets on the domain boundary.
- Resolution: removed (binding, interface, implementation), together with
  `InitiateBoundaryData`, its only caller.

### 7. Minor

- Fixed: `GetNodeCoord(dim=...)` returned three rows, but `GetNodeCoord()`
  (all nodes), `GetNodeCoord(globalNode(:))` and `GetNodeCoord(globalNode)`
  return `nsd` rows. Now all of them return `nrow = obj%nsd` rows.
- Fixed: `GetParam(nodeCoord=...)` assigned to an assumed-shape dummy, so
  the caller had to give the exact shape. It now calls the new
  `GetNodeCoord(nodeCoord, nrow, ncol, dim)` (`obj_GetNodeCoord4`), which
  writes the nodes of the domain (or of the mesh of dimension dim) without
  allocation; nodeCoord must be large enough. `GetNodeCoord(nodeCoord,
  dim)` allocates and calls it. Test:
  `GetNodeCoord_test_1`.
- Fixed: component `isNodeNumberSparse` was declared `LOGICAL(I4B)`; it is
  now `LOGICAL(LGT)` like the other flags.
- Fixed: `SetTotalElements` changed the stored `tElements(indx)` only, while
  `GetTotalElements` reads the meshes, so the two could disagree. The
  component `tElements` and `SetTotalElements` are removed;
  `GetParam(tElements=...)` and `Display` compute the counts from the meshes.
  Test: `GetParam_test_1`.

## Open

None.
