# H1 orientation regression findings — 2026-10-30

Scope: uniform-order conforming H1 triangles and quadrangles, linear
geometry, Lagrange and hierarchical bases, orders one through four.

## Fixed in easifemBase: vector Ultraspherical evaluation overwrites rows

- File: `src/submodules/Polynomial/src/UltrasphericalPolynomialUtility@Methods.F90`
  in the sibling `easifem/base` repository.
- Routine: `UltrasphericalEvalAll2_`.
- Inside the concurrent sample loop, the degree recurrence assigned
  `ans(1:jj, ii+1)`, overwriting earlier sample rows using coefficients
  computed from sample `jj`. This also creates overlapping concurrent
  writes. The corrected recurrence assigns only `ans(jj, ii+1)`.
- This corrupted the hierarchical triangle's degree-two edge kernel,
  making order-four traces disagree across a shared edge.
- Regression: `H1OrientationKernel_test_1.F90` compares vector evaluation
  with scalar evaluation and reverses the sample order, degrees zero
  through four. Baseline: 14 failed checks; corrected: all 30 pass.
- Identical base example: `UltrasphericalRows_test_1.F90` under
  `src/modules/Polynomial/examples/`.

## Fixed in easifemBase: inconsistent edge-gradient orientation factor

- File: `src/submodules/Triangle/src/TriangleInterpolationUtility@HeirarchicalBasisMethods.F90`
  in the sibling `easifem/base` repository.
- Routine: `BarycentricEdgeBasisGradient_Triangle2`.
- An edge mode uses orientation factor `o**(ii+1)` and an un-oriented
  kernel argument. Both terms of its derivative must carry that same
  factor. Previously the kernel-derivative term used `o**ii`, giving
  the wrong sign when `o=-1`.
- All three edges now use the same factor in both product-rule terms.
- Regression: `H1OrientationGradient_test_1.F90` compares vertex/edge
  gradients with centered finite differences, orders one through four,
  both coordinate directions and all eight edge-orientation combinations.
  Baseline: 30 failed checks; corrected: all 64 pass.
- Identical base example: `HierarchicalEdgeGradient_test_1.F90` under
  `src/modules/Triangle/examples/`.

## FEDOF integration verification

`H1Orientation_test_1.F90` evaluates every global basis trace at seven
matching physical points on the shared edge and compares physical
tangential derivatives. It checks opposite local edge directions and
visits cells in both orders to exercise FEDOF's shared FE object.

Geometry and solution use the same reference domain. Their order-one
geometry bases come from the same family; mixing UNIT and BIUNIT reference
coordinates would invalidate the test rather than expose an orientation
bug. This was corrected while preparing the test.

With that setup, baseline integration had six failed checks, all for
hierarchical triangles at orders three/four. The corrected dependency
passes all 196 checks. Lagrange triangles and both quadrangle families
passed before the dependency changes. FEDOF connectivity is kept in
canonical entity order; no second permutation or sign correction is added.

Base was rebuilt and installed with `easifem install base --no-download`.
All 64 FEDOF examples pass, including 290 checks in the three new
orientation examples. Targeted lint and documentation generation pass.
These checks establish trace
continuity; they do not validate normal derivative continuity, cell-mode
gradients, variable-order interfaces, curved geometry or 3D face closures.
