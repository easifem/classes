# Bug report: FEDomainConnectivity_

- Module: `src/modules/FEDomainConnectivity/src/FEDomainConnectivity_Class.F90`
- Submodules: `src/submodules/FEDomainConnectivity/src/`
- Tests: none yet (only indirectly, `SetSparsity_test_2` in
  `src/modules/FEDomain/examples/`)
- Date: 2026-10-24
- Users in the library: `AbstractDomainSetSparsity`
  (`InitiateNodeToNodeData` with two domains) and FEDOF `SetSparsity`
  (`InitiateCellToCellData` with two meshes). Nothing in the library uses
  the facet-to-cell part; NitscheBC uses `DomainConnectivity_`.

This report comes from reading the code; the open items are not run yet.

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Fixed

### 1. InitiateNodeToNodeData gives an empty map in 2D

- Where: `obj_InitiateNodeToNodeData1/2` (NodeMethods)
- Cause: all 3 components of `x1` and `x2` were compared, but only the
  first nsd are set; `nodeToNode` was also indexed by the local node
  number although it is documented (and used) with the global number.
- Fix and test: see `AbstractDomain.md`, bug 6 (`SetSparsity_test_2`).

## Open

### 2. Facet-to-cell data is not implemented

- Where: `obj_InitiateFacetToCellData1`, `2`, `3`, `4` (FacetMethods)
- Behaviour: each body is a debug-only "[WIP ERROR] :: This routine is
  under development"; the old code is commented out. In a release build
  the routines do nothing, so `facetToCell` is never allocated.
- Effect: every facet getter (`MasterCellNumber`, `SlaveCellNumber`,
  `MasterFacetLocalID`, `SlaveFacetLocalID`, `MasterDimTag`,
  `SlaveDimTag`, `GlobalFacetID` and the `Get...` versions) returns an
  empty array, or, for the versions with `localElement`, indexes an
  unallocated `facetToCell` (crash).
- Note: `DomainConnectivity_` has a working version (fixed on 2026-10-24:
  one pass per mesh, entity from the mesh id of the cell) that could be
  ported.

### 3. SlaveFacetLocalID() returns slave cell numbers

- Where: `obj_SlaveFacetLocalID3` (FacetMethods)
- Cause: it calls `obj%GetSlaveCellNumber(ans)` instead of
  `obj%GetSlaveFacetLocalID(ans)`.

### 4. SlaveDimTag() allocates an array with zero rows or columns

- Where: `obj_SlaveDimTag3` (FacetMethods)
- Cause: `ii = 0` is never changed, so `ans` is allocated as `(tsize, 0)`
  or `(0, tsize)`; `GetSlaveDimTag` then writes `VALUE(ii, 1:2)` or
  `VALUE(1:2, ii)` out of bounds. `obj_MasterDimTag3` sets `ii = 2` when
  `facetToCell` is allocated.

### 5. GetDimEntityNum reads an array that is never set

- Where: `obj_GetDimEntityNum` (CellMethods)
- Cause: it returns `obj%cellToCellExtraData(1:2, globalElement)`, but no
  routine allocates or fills `cellToCellExtraData`, so the call reads an
  unallocated array.
- Options: fill it in `InitiateCellToCellData` (dim and mesh id of the
  cell in domain2/mesh2), or remove the component and the method.

### 6. cellToCell is indexed by local element number

- Where: `obj_InitiateCellToCellData1/2` (CellMethods); doc of the
  component `cellToCell` in the module.
- Behaviour: `cellToCell(iel)` uses the local element number `iel` of
  domain1/mesh1 and stores the global element number of domain2/mesh2.
- Doc: "CellToCell(ielem) => global elem number in domain-2, corresponding
  to global elem number `ielem` in domain-1".
- The only user, FEDOF `SetSparsity`, reads it with the local number
  (`GetConnectivity_(globalElement=iel, islocal=.TRUE.)`), so code and
  user agree; the doc is wrong. Decide which numbering is intended.
- Also: a cell with no match keeps `cellToCell = 0`; FEDOF passes this 0
  to `GetConnectivity_(globalElement=0, islocal=.FALSE.)`.

### 7. Deallocate does not free cellToCellExtraData

- Where: `obj_Deallocate1` (ConstructorMethods)
- Cause: all other allocatable components are freed, not
  `cellToCellExtraData`.

### 8. Minor

- `isInitiated` is never set to true (only reset in `Deallocate`).
- `elemToElem` (`ElementConnectivity_`) is never filled; only freed.
- `InitiateCellToCellData1` (domains) does not skip inactive elements;
  `InitiateCellToCellData2` (meshes) does.
- In `InitiateCellToCellData1/2` the `EXIT` leaves only the loop over the
  elements around one node; the search continues with the other nodes of
  the cell and may overwrite `cellToCell(iel)` with the same element.
- Docblocks are in the old style (`authors:`, `!# Introduction`, no
  `Examples`).
