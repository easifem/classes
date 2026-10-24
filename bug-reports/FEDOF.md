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

None.

## Open

### 0a. Labelled CYCLE in inner loops (regression, uncommitted)

- Where: the uncommitted change that labels the outer loops as `loop1`.
  An inner `CYCLE` became `CYCLE loop1`, so it now leaves the inner loop
  and moves to the next element:
  - `FEDOF_Class@SetMethods.F90:134` (`SetFaceOrder`):
    `IF (foundFaces(kk)) CYCLE loop1`
  - `FEDOF_Class@SetMethods.F90:194` (`SetEdgeOrder`):
    `IF (foundEdges(jj)) CYCLE loop1`
  - `FEDOF_Class@SetSparsityMethods.F90:249, 252, 253` (inner `jvar`
    loop of `FEDOFSetSparsity`): `CYCLE loop1` skips the other column
    variables.
- Effect: as soon as an element meets a face (edge) that is already
  numbered, its other faces (edges) are skipped and keep order 0. On
  small_tri3, 6 of 25 faces get no DOFs: order 2 gives 31 DOFs instead of
  37, and order 3 gives 64 instead of 76.
- Fix: use a plain `CYCLE` (or label the inner loop and cycle that one).
- Tests: GetTotalDOF_1-4, GetConnectivity_1-4, GetCellDOF_1-4, Copy_1-2,
  Initiate_1,2,3,5,6, ImportFromToml_1-3, Lagrange_2-4, SetSparsity_1,
  GetOrders_1.

### 0b. Edges of a 3D mesh are not initiated

- Where: `obj_Initiate` (ConstructorMethods) reads
  `ent = obj%mesh%GetTotalEntities()`, but nothing calls
  `InitiateEdgeConnectivity` for a 3D mesh. Only face connectivity is
  built, in `InitiateElementToElements`. So `tEdges = 0` and
  `ElemData%globalEdges` has size 0.
- Effect: on a hexahedral mesh, edge DOFs are missing (order 2 gives a
  wrong `GetTotalDOF`), `GetEdgeOrder` returns 0 edges, and
  `GetEdgeDOF(localEdgeNumber=...)` crashes in
  `Elemdata_GetGlobalEdgeNumber` ("Index '1' ... above upper bound of 0").
- Fix (proposal): in `Initiate`, when `xidim == 3`, call
  `obj%mesh%InitiateEdgeConnectivity()` if it is not initiated yet. Or do
  it in `FEDomain_%Initiate`.
- Tests: GetEdgeDOF_test_1 (crash), GetFaceOrder_test_1 (one check).

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

### 3. GetMaxTotalQuadraturePoints returns the value of the last element

- Where: `obj_GetMaxTotalQuadraturePoints` (GetMethods)
- Cause: `ans = feptr%GetTotalQuadraturePoints()` in the element loop
  instead of `ans = MAX(ans, ...)`; `ans` is also not set to 0 first.
- Effect: wrong for meshes with several topologies or orders.

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
