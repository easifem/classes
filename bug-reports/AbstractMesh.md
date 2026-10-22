# Bug report: AbstractMesh_

- Module: `src/modules/AbstractMesh/src/AbstractMesh_Class.F90`
- Submodules: `src/submodules/AbstractMesh/src/`
- Tests: `src/modules/FEMesh/examples/` (FEMesh_ is the concrete mesh)
- Date: 2026-10-22

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Fixed

### 2. GetBoundaryNptrs returns zeros

- Where: `obj_GetBoundaryNptrs` in `AbstractMesh_Class@GetMethods.F90`
- Symptom: the result has the right size (8 for the quad mesh) but all
  entries are 0.
- Cause: it selects nodes with `nodeType == BOUNDARY_NODE`, but
  `MarkInternalNodes` tags boundary nodes as `DOMAIN_BOUNDARY_NODE`.
  `IsBoundaryNode` and `GetTotalBoundaryNodes` use "not `INTERNAL_NODE`".
- Fix: use `nodeType .NE. INTERNAL_NODE`; removed the unused
  `BOUNDARY_NODE` import.
- Test: `IsBoundaryNode_test_1`

### 3. GetTotalBoundaryElements returns 0

- Where: `obj_GetTotalBoundaryElements` in `AbstractMesh_Class@GetMethods.F90`
- Symptom: 0 although `IsBoundaryElement` is true for every element.
- Cause: it counts `elementType == BOUNDARY_ELEMENT`, but elements are
  tagged `DOMAIN_BOUNDARY_ELEMENT`.
- Fix: count with `Elemdata_IsBoundaryElement` (same as
  `IsBoundaryElement`).
- Test: `IsBoundaryElement_test_1`, `InitiateBoundaryData_test_1`

### 4. GetElementToElements ignores onlyElements

- Where: `obj_GetElementToElements` in `AbstractMesh_Class@GetMethods.F90`
- Symptom: the full information (3 columns) is never returned; the
  result shape was `[neighbours, 1]`.
- Cause: `onlyElem = Input(default=.FALSE., option=onlyElem)` reads the
  uninitialized local variable instead of the argument `onlyElements`.
- Fix: `option=onlyElements`.
- Test: `GetElementToElements_test_1`

### 5. GetMaxElementToElements returns 3 times the neighbour count

- Where: `obj_GetMaxElementToElements` in `AbstractMesh_Class@GetMethods.F90`
- Symptom: 6 for the quad mesh, where each element has 2 neighbours.
- Cause: `globalElements` of the element data stores three entries per
  neighbour (element, local facet, neighbour facet); the routine used
  `SIZE(globalElements)`.
- Fix: divide by 3.
- Test: `InitiateElementToElements_test_1`

### 6. GetNodeCoord (all nodes) returns nrow = 0

- Where: `obj_GetNodeCoord2` in `AbstractMesh_Class@GetMethods.F90`
- Symptom: coordinates are written but `nrow` is 0 (undefined).
- Cause: `tsize` is `PRIVATE` in the OpenMP loop; `nrow = tsize` after the
  loop reads the undefined outer variable.
- Fix: read the number of rows from the first node after the loop.
- Test: `GetNodeCoord_test_1`

### 7. GetNptrsInBox crashes if nptrs is allocated

- Where: `obj_GetNptrsInBox` in `AbstractMesh_Class@GetMethods.F90`
- Symptom: `Fortran runtime error: Attempting to allocate already
  allocated variable 'nptrs'`.
- Cause: `ALLOCATE (nptrs(tnodes))` on an `INTENT(INOUT)` allocatable
  argument without a check.
- Fix: deallocate `nptrs` first if allocated.
- Test: `GetNptrsInBox_test_1`

### 8. Initiate(group=...) cannot find nodeCoord

- Where: `LocateNodeCoord` in `AbstractMesh_Class@HDFMethods.F90`
- Symptom: `Initiate(hdf5, group="/surfaceEntities_1")` stops with
  "Failed to open dataset", then (after the first fix) "cannot locate
  nodeCoord in the path".
- Cause (two problems):
  - the first candidate (the group path itself) is accepted when the
    path exists, even when it is a group and not a dataset;
  - the third candidate uses `PathBase(group)//"/nodeCoord"`, but
    `PathBase` returns the last element of the path, not the parent.
- Fix: accept the first candidate only if it is not a group
  (`hdf5%IsGroup`); use `PathJoin(PathDir(group), "nodeCoord")`.
- Test: `Initiate_test_4`

### 9. GetMaxElementToElements and GetMaxNodeToNodes are private

- Where: type bindings of `AbstractMesh_` in `AbstractMesh_Class.F90`
- Symptom: `Error: 'getmaxnodetonodes' of 'abstractmesh_' is PRIVATE`
  when called from a program.
- Cause: the bindings had no `PUBLIC` attribute (the type has `PRIVATE`
  after `CONTAINS`), unlike `GetMaxNodeToElements`.
- Fix: added `PUBLIC`.
- Test: `InitiateElementToElements_test_1`, `InitiateNodeToNodes_test_1`

### 10. facetElementType is never allocated

- Where: component `facetElementType(:, :)`; used by
  `obj_GetFacetElementType` (GetMethods) and `obj_SetFacetElementType`
  (SetMethods).
- Symptom: `GetFacetElementType` stopped with SIGBUS;
  `SetFacetElementType` wrote to an unallocated array.
- Cause: no routine allocated or filled `facetElementType`; it was only
  deallocated (ConstructorMethods) and displayed (IOMethods). Nothing in
  the library called `GetFacetElementType`.
- Fix: removed the component, `GetFacetElementType` (binding, interface,
  implementation), and its use in `Deallocate` and `Display`.
  `SetFacetElementType` now sets only the element type of the element
  (`elementData(iel)%ptr%elementType`); `iface` is not used. The domain
  callers (`AbstractDomain_%SetFacetElementType`,
  `InitiateBoundaryData`) were removed earlier.
- Test: `SetFacetElementType_test_1`

### 11. InitiateFacetElements fails for curve meshes

- Where: `obj_InitiateFacetElements` in `AbstractMesh_Class@FacetDataMethods.F90`
- Symptom: "[ERROR] :: Facet elements not found in the mesh" (stopped the
  program) for a mesh with `xidim = 1`.
- Cause: it uses `obj%tFaces`, which is zero for 1D meshes; only point
  meshes returned early.
- Fix: return early for `xidim <= 1`; point and curve meshes have no
  facet elements. This also makes `FEDomain_%InitiateFacetElements` work
  for 2D and 3D domains.
- Test: `InitiateFacetElements_test_4` (FEMesh),
  `InitiateFacetElements_test_1` (FEDomain)

## Open

### 12. Routines which are not implemented

| Routine | Behaviour | Decision |
|---|---|---|
| `obj_Export` | raises "[WIP]: This routine has not been implemented yet" and stops | kept; docblock says it needs to be implemented |
| `obj_SetQuality` | debug build: raises "[WIP ERROR] :: This routine is under development"; release build: does nothing | kept; docblock says it needs to be implemented |

`obj_DisplayFacetElements` (raised "[DEPRECATED]") was removed: binding,
interface and implementation. Nothing called it.

`obj_GetOrder1` (`GetOrder()` without element) is implemented. It returns
the largest element order, stored in the new components `maxElemOrder` and
`isMaxElemOrderSet` (reset by Deallocate, shown by Display); `obj` is now
INTENT(INOUT). Tests: `GetOrder_test_2` (Quadrangle4, order 1, computed and
stored), `GetOrder_test_3` (Triangle6, order 2, again after Deallocate).

### 13. SetTotalMedium adds media instead of setting the number

- Where: `obj_SetTotalMedium1`, `obj_SetTotalMedium2` (SetMethods) via
  `Elemdata_SetTotalMedium`.
- Symptom: `SetTotalMedium(n=2)` then `SetTotalMedium(n=3,
  globalElement=14)` gives `GetTotalMaterial(14) = 5`.
- Cause: `Elemdata_SetTotalMedium` extends the existing array by `n`.
- Question: is this intended? The argument comment says "Total number of
  medium assigned to a given element". The docblocks describe the
  present behaviour.

### 14. GetNodeToNodes does not initiate its data

- Where: `obj_GetNodeToNodes1/2`, `obj_GetNodeToNodes1_/2_`
- Symptom: empty result if `InitiateNodeToNodes` was not called.
- Note: `GetNodeToElements` initiates its data on demand. Either make
  `GetNodeToNodes` do the same, or keep the documented requirement.

### 15. AbstractMeshPointerDeallocate does not free the meshes

- Where: `AbstractMeshPointerDeallocate` (ConstructorMethods)
- Symptom: memory leak.
- Cause: it calls `Deallocate` on each mesh and deallocates the array,
  but never `DEALLOCATE (obj(ii)%ptr)`.

### 16. Minor

- `GetNearestNode` with `nn` returns nodes in no fixed order: the kdtree
  is created with `sort=.FALSE.` (documented, not a bug).
- `GetFacetConnectivity`, `GetFacetConnectivity_` and
  `GetTotalFacetConnectivity` declare `islocal` as `LOGICAL(I4B)`;
  the rest of the class uses `LOGICAL(LGT)`.
- `GetMasterCellNumber`, `GetSlaveCellNumber`, `GetCellNumber`,
  `GetLocalFacetID` and `AbstractMeshGetFacetConnectivity` ignore
  `elementType`.
- The edge orientation is zero-sized until `InitiateEdgeConnectivity` is
  called; no error is raised.
