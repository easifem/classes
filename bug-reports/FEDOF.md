# Bug report: FEDOF_

- Module: `src/modules/FEDOF/src/FEDOF_Class.F90`
- Submodules: `src/submodules/FEDOF/src/`
- Tests: `src/modules/FEDOF/examples/` (`run_examples.fish`)
- Date: 2026-10-24

This report comes from reading the code while writing the docs and tests;
none of the items is changed yet. The docblocks describe the present
behaviour and mention the items that affect users.

Status: **Fixed** means the source is changed and the library is rebuilt;
the listed test passes. **Open** means not changed.

## Fixed

All 57 tests pass (2026-10-29).

- 0a. Labelled CYCLE in inner loops: the last wrong label, `CYCLE loop1`
  after `colMesh%isEmpty()` in `FEDOFSetSparsity`, is now `CYCLE loop2`.
  Tests: GetTotalDOF_1-4, GetConnectivity_1-4, SetSparsity_1-2, ...
- 0b. Edges of a 3D mesh: three causes.
  - `AllocateSizes` now calls `mesh%InitiateEdgeConnectivity()` when
    `xidim == 3` (nothing else numbered the edges, `tEdges` was 0).
  - `Elemdata_GetEdgeConnectivity` (ElemData_Class) returned the local
    node numbers of the edge (1 to 8) instead of the global ones, so
    `SetEdgeOrder` found the wrong elements around an edge. It now maps
    them with `obj%globalNodes`.
  - `Elemdata_GetTotalEdgeDOF` returned `order - 2`; it now uses
    `GetTotalInDOF` of a line (`order - 1` for H1), as the face version
    does.
  - Also in `SetEdgeOrder`: `n2e2` had `maxFaces` (6) entries but holds
    the elements around a node; now `maxNodeToElem`.
  - Tests: GetEdgeDOF_test_1, GetFaceOrder_test_1.
- 3. `GetMaxTotalQuadraturePoints` starts from 0 and takes the maximum
  over the elements. Test: GetMaxTotalQuadraturePoints_test_1.
- Stale caches: `SetOrdersFromCellOrder` resets `isMaxConSet`,
  `isMaxQuadPointSet` and `isSumElemTotalDOFSet`. The order setters are
  private and only called after `DEALLOCATE`, so this was not reachable
  yet; the reset keeps the caches right if that changes.

## Open

### 1. GetConnectivity_ ignores long opt values

- Where: `obj_GetConnectivity_` (GetMethods)
- Cause: it sets `opt0 = opt(1:1)` but selects `SELECT CASE (opt)` on the
  whole string, so `opt="Vertex"`, `"Edge"`, `"Face"` or `"Cell"` fall to
  the default case (all degrees of freedom). `GetTotalDOF` uses `opt0`, so
  `GetConnectivity(opt="Vertex")` allocates fewer entries than it writes.
- Workaround: use one letter (V, E, F, C, A); the tests do.

### 2. GetOrders reads the orientation of the wrong element

- Where: `obj_GetOrders` (GetMethods)
- Cause: the local element number `jj` is used for the orders, but
  `GetOrientation` is called with `globalElement=globalElement,
  islocal=.TRUE.`. With a global element number (`islocal` false) the
  orientations of another element are returned. `SetFE` passes a local
  number, so it is not affected.
- Test: `GetOrders_test_1` uses local numbers.

### 4. Deallocate does not free the finite elements

- Where: `obj_Deallocate` (ConstructorMethods)
- Cause: the finite elements are created by `FEFactory` in `Initiate`;
  `Deallocate` calls their `DEALLOCATE` and nullifies the pointers, but does
  not free them. Note that `Copy` shares the finite elements, so freeing
  them needs an ownership rule.

### 5. InitiateGeoFEDOF cannot be called

- Where: `obj_InitiateGeoFEDOF` (interface and ConstructorMethods)
- Cause: it has no type-bound binding and the module is PRIVATE.
  Add it to the `Initiate` generic (or a binding), or remove it.

### 6. SetNodeCoord is not implemented

- Where: `obj_SetNodeCoord` (SetMethods): empty body.

### 7. GetFacetConnectivity_ mixes node numbers and DOF numbers

- Where: `obj_GetFacetConnectivity_` (GetMethods)
- The vertex part comes from the mesh (global node numbers); the vertex
  degree of freedom is the local node number (`GetVertexDOF`). They agree
  only when the nodes are numbered 1 to N.

### 8. Minor

- `scaleForQuadOrder` is 2 by default in the type and in `Deallocate`, but
  1 by default in `ImportFromToml`.
- `ImportFromToml2`: `dom` is optional but passed to `ImportFromToml1`,
  which needs it; `printToml` is not used.
- `GetEdgeDOF1`, `GetTotalEdgeDOF1`, `GetFaceDOF1`, `GetTotalFaceDOF1`
  ignore `islocal`.
- `SetOrdersFromCellOrder` skips inactive cells; their edges, faces and
  cells keep zero entries in `edgeIA`, `faceIA`, `cellIA`.
- The shape-data tests of the old docs (`GetLocalElemshapeData_test_*`)
  test AbstractFE_ and were not ported; they belong to AbstractFE_.
