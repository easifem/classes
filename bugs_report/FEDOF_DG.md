# DG FEDOF generation — 2026-10-30

## Scope of this batch

Nodal Lagrange DG on linear 2D triangles and quadrangles, nonnegative
isotropic cell degree. Tests cover degrees zero through three, uniform
and mixed orders, and local/global element identifiers.

The deferred 3D hierarchical H1 work and variable-order Lagrange H1
constraints remain deferred; neither is implemented by this batch.

## Fixed: DG construction still followed conforming H1 paths

The interface accepted DG, but AllocateSizes reserved shared mesh vertex
DOFs; connectivity and per-element counts also included shared vertices.
FEFactory has no DGLAGR case. The regression stopped in FEFactoryTriangle
with `No case found for acase=DGLAGR` before any checks completed.

DG now has no shared vertex, edge or face DOFs. Each active cell owns a
complete polynomial basis, with cumulative cell offsets starting at one.
Inactive cells get empty ranges; inactive-mesh integration is untested.
Cell count is (p+1)(p+2)/2 for triangles and (p+1)^2 for quadrangles.

FEDOF keeps DG as its continuity mode and reuses the existing local H1
Lagrange evaluator to evaluate that full basis. The FE factory itself is
unchanged. This is a broken polynomial space: the DOF mapping, rather than
the local polynomial family, determines interelement continuity.

SetFE gives the evaluator the owning-cell order without minimum-neighbor
trace reduction and disables shared-edge orientation corrections. The
minimum quadrature accuracy is one, including constant cells.

GetConnectivity with All or Cell returns the full owning-cell block;
Vertex, Edge and Face return empty blocks. GetVertexDOF returns an empty
result. GetFacetConnectivity_ returns the full owning-cell block for any
valid local facet: a DG trace is evaluated from all cell coefficients,
not from shared facet unknowns or just the nodes lying on that facet.

## Fixed: constant local bases entered a vertex-lattice routine

The existing triangle interpolation generator writes the three vertex
coordinates even at degree zero, overflowing the one-point local buffer.
After DG numbering worked, the regression stopped at that bounds error
while evaluating the first constant cell.

Triangle and quadrangle local Lagrange shape evaluation now handles degree
zero directly: one shape of value one and zero reference gradients. Their
interpolation-point helpers likewise return one point at the mean of the
supplied vertex coordinates. The underlying base interpolation generator
is unchanged; direct callers of that generator still need its own fix.

## Verification

- `DGNumbering_test_1`: 1,784 checks pass. Eight order pairs on both cell
  shapes, local/global order indexing, contiguous independent cell blocks,
  global/local counts, every facet's owning-cell block, local basis sizes,
  nodal point counts, constant centroids and zero gradients.
- `DGTrace_test_1`: 148 checks pass. Evaluate both sides at seven matching
  physical edge points, with values two and five on opposite sides. The
  jump remains three. Check zero tangential derivatives of constant fields
  and both cell visitation orders, degrees zero through three.
- Classes rebuilt and installed with the easifem CLI.
- All 67 FEDOF examples pass, including the existing H1 regressions.
- Targeted lint passes except for two pre-existing findings in the
  BasisOpt interface: pointer default initialization and a missing INTENT
  on an unrelated procedure. No unsafe lint fixes were applied.

## Remaining DG work

- Numerical flux assembly and neighbor-cell sparsity. Existing SetSparsity
  creates only volume couplings inside each DG cell; it is not a complete
  flux-coupled DG graph.
- A dedicated facet trace/operator API; current regressions evaluate the
  full cell basis at facet points. Existing H1 facet basis helpers are not
  a DG flux implementation.
- Additional interpolation families, 1D/3D, anisotropic order and curved
  geometry validation.
