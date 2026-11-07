# Bug report: FEDOF_

- Module: `src/modules/FEDOF/src/FEDOF_Class.F90`
- Submodules: `src/submodules/FEDOF/src/`
- Tests: `src/modules/FEDOF/examples/` (`run_examples.fish`)
- Date: 2026-10-24

This report records findings from source review and executable examples.
The sections below distinguish fixed findings from open issues.

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

### 1. GetConnectivity_ ignores long opt values

- Where: `obj_GetConnectivity_` (GetMethods)
- Cause: it sets `opt0 = opt(1:1)` but selects `SELECT CASE (opt)` on the
  whole string, so `opt="Vertex"`, `"Edge"`, `"Face"` or `"Cell"` fall to
  the default case (all degrees of freedom). `GetTotalDOF` uses `opt0`, so
  `GetConnectivity(opt="Vertex")` allocates fewer entries than it writes.
- Fixed 2026-10-30: normalize the option before both selection and mesh
  lookup. Test: `H1Connectivity_test_1` (267 checks).

### 2. GetOrders reads the orientation of the wrong element

- Where: `obj_GetOrders` (GetMethods)
- Cause: the local element number `jj` is used for the orders, but
  `GetOrientation` is called with `globalElement=globalElement,
  islocal=.TRUE.`. With a global element number (`islocal` false) the
  orientations of another element are returned. `SetFE` passes a local
  number, so it is not affected.
- Fixed 2026-10-30: preserve the converted local element for all lookups.
  Test: `H1Orders_test_1` (87 checks).

### 7. GetFacetConnectivity_ mixes node numbers and DOF numbers

- Where: `obj_GetFacetConnectivity_` (GetMethods)
- The vertex part comes from the mesh (global node numbers); the vertex
  degree of freedom is the local node number (`GetVertexDOF`). They agree
  only when the nodes are numbered 1 to N.
- Fixed 2026-10-30 for linear 2D facets: map the prefix to mesh-local
  vertex DOF IDs. Test: `H1FacetConnectivity_test_1` (153 checks).

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

## Open

### 6. SetNodeCoord is not implemented

- Where: `obj_SetNodeCoord` (SetMethods): empty body.

### 8. Minor

- `scaleForQuadOrder` is 2 by default in the type and in `Deallocate`, but
  1 by default in `ImportFromToml`.
- `ImportFromToml2`: `dom` is optional but passed to `ImportFromToml1`,
  which needs it; `printToml` is not used.
- `GetEdgeDOF1`, `GetTotalEdgeDOF1`, `GetFaceDOF1`, `GetTotalFaceDOF1`
  ignore `islocal`.
- The old numbering skipped inactive entities without propagating offsets.
  The count-then-accumulate implementation now propagates empty ranges;
  dedicated inactive-mesh integration coverage remains outstanding.
- The shape-data tests of the old docs (`GetLocalElemshapeData_test_*`)
  test AbstractFE_ and were not ported; they belong to AbstractFE_.

## H1 regression batch — 2026-10-30

Items 1, 2 and 7 above were rebuilt, installed and verified:

- `GetConnectivity_` normalizes long options before mesh lookup and
  entity selection. `H1Connectivity_test_1` checks allocating and
  caller-buffer forms, DOF counts and shared traces.
- `GetOrders` uses the converted local element consistently for order
  and orientation lookup. `H1Orders_test_1` compares global and local IDs.
- `GetFacetConnectivity_` maps mesh-global node IDs to vertex DOF IDs.
  `H1FacetConnectivity_test_1` checks IDs and shared 2D facet traces.

Baseline: both connectivity-option and orientation tests crash, while
facet connectivity has 42 failed checks. All use orders 1–3 and both
Lagrange and hierarchical H1 spaces on two linear quadrangles.
All three new examples pass: 507 checks, zero failures. The complete
FEDOF runner also passes all 60 examples (57 existing plus three new).

Scope limitation: complete 3D facet closure (including edge DOFs) and
facets on higher-order geometry are not implemented by this batch.

## Uniform H1 numbering batch — 2026-10-30

`SetOrdersFromCellOrder` previously formed each entity offset while
visiting cells, using `IA(j)` immediately to assign `IA(j+1)`. This assumes
entities are first encountered in increasing ID order. The routine now
collects counts by entity ID first, then accumulates offsets in ID order.
Unused entities receive empty ranges. The total is derived from the final
cell offset, so it agrees with the actual numbered ranges.

`H1Numbering_test_1` checks adjacent triangles and quadrangles, Lagrange
and hierarchical bases, and uniform orders 1, 2, 3, 4, then 1 again. It
verifies exact vertex, facet-interior and cell-interior IDs, local counts,
global coverage without holes, unique local IDs, and shared trace IDs.
All 1,006 checks pass before and after the refactor on these standard
fixtures, confirming compatibility for this supported uniform H1 case.
The prior traversal-order assumption is removed structurally; this test
does not simulate arbitrary entity renumbering or inactive meshes.

Rebuilt and installed successfully. All 61 FEDOF examples pass. Targeted
Fortitude checks and documentation generation also pass.
Orientation-dependent permutations/signs, variable-order compatibility,
and complete 3D facet closure are outside this batch.

## Uniform H1 orientation batch — 2026-10-30

See [H1Orientation.md](H1Orientation.md) for the two dependency defects
found and fixed in easifemBase. No additional FEDOF connectivity
permutation is needed: existing Lagrange physical mapping and hierarchical
local evaluation already apply orientation in their respective paths.
Reversed-edge basis traces and tangential derivatives now pass for both
families, triangles and quadrangles, orders one through four, in both
cell visitation orders. All 64 FEDOF examples pass.

## Variable-order hierarchical H1 batch — 2026-10-30

`SetFE` now passes entity-order arrays without the scalar-order shortcut
for hierarchical H1 in 2D. This preserves minimum shared-face orders in
triangles while retaining each cell's interior order. Quadrangles use
the same explicit entity-order path. See
[H1VariableOrder.md](H1VariableOrder.md) for the reproduced defect.

`H1VariableOrder_test_1` passes all 2,036 checks: nine order pairs on
triangles and quadrangles, local/global order indexing, both cell visit
orders, shared DOF IDs, basis traces and tangential derivatives. All 65
FEDOF examples pass. Variable-order Lagrange constraints remain
outside this implementation.
