# Variable-order hierarchical H1 — 2026-10-30

## FEDOF SetFE discarded reduced triangle face orders

- Location: `obj_SetFE` in
  `src/submodules/FEDOF/src/FEDOF_Class@SetMethods.F90`.
- FEDOF already assigned minimum neighboring orders to shared faces and
  used those orders when numbering DOFs. However, SetFE passed both a
  scalar `order` and the entity-order arrays to the FE's SetOrder.
- TriangleH1HierarchicalFE_ gives the scalar argument precedence: it
  assigned that order to every face. A higher-order triangle therefore
  evaluated more edge modes than FEDOF numbered, losing trace conformity.
- Fix: for hierarchical H1 in 2D, SetFE passes the cell, face and edge
  arrays without the scalar shortcut. Other paths retain their original
  scalar argument. The quadrangle already gives entity arrays precedence;
  the explicit array-only call preserves that behavior.

## Regression

`src/modules/FEDOF/examples/H1VariableOrder_test_1.F90` uses two adjacent
triangles and two adjacent quadrangles, with opposite local directions
on their shared edge. Order pairs are (1,4), (2,4), (4,2), (4,1), (3,4),
(4,3), (2,3), (3,2), and finally (1,1) to exercise reinitialization.
Each pair runs with both locally and globally indexed order arrays and
visits the FE objects in both element orders.

Checks include global/local counts, unique local DOF IDs, numbering
coverage, shared facet DOF IDs and counts, configured cell/face orders,
basis-row/connectivity agreement, every global basis trace, and physical
tangential derivatives at seven matching points. Geometry is linear and
uses the same hierarchical reference domain as the solution basis.

Baseline: 128 checks fail, including configured face orders, basis sizes,
trace values and tangential derivatives. After rebuilding and installing,
all 2,036 checks pass. All 65 FEDOF examples pass. Targeted Fortitude
checks, documentation generation and whitespace checks also pass.

## Scope

This is a hierarchical conforming space with reduced edge traces. It is
not the full high-order polynomial space on the higher-order cell: that
cell retains its interior modes and its other edge modes, while sharing
only the lower-order trace. Additional variable-order Lagrange constraints,
anisotropic cell orders, inactive meshes, curved geometry and 3D interfaces
are outside this change. Interior gradient accuracy is not established by
these trace checks.
