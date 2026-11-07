# Bug report: H1FE modules

- Modules: `src/modules/H1FE/src/`
- Implementations: `src/submodules/H1FE/src/`
- Examples: `src/modules/H1FE/examples/`
- Date: 2026-10-30

This report covers all 24 H1FE modules, including the OneDim classes.
Examples are compiled in Debug against the installed classes library.
No implementation fixes, library rebuilds or library installs were made.

Status: **Open** means not fixed. **Fixed** requires an implementation
correction, a rebuilt library and a passing regression example.

Run from `src/modules/H1FE/examples/`:

```fish
./run_examples.fish
```

Use a class or method prefix to select examples. Logs are written to
`/tmp/easifemClasses/H1FE-logs/<example>.log` and overwritten on reruns.
WIP examples deliberately expose unsupported public operations; they are
reported separately from defects in implemented methods.

## Test results

The final run contains 131 examples: **58 pass and 73 fail**, with no
examples lacking checks. Failure categories are:

| Category | Examples |
| --- | ---: |
| Explicit 3D WIP placeholders | 56 |
| Missing vector cleanup implementations (link failures) | 14 |
| Constructor/order behavior defects | 2 |
| Existing line-facet quadrature placeholder | 1 |

The working OneDim value and pointer cleanup, all six implemented cell
shape families, and the tested triangle/quadrangle facet projections pass.
The working-library cases use consistent geometry and solution reference
domains and prepare OneDim interpolation storage before nodal sampling.

## Open

### 1. Modern FiniteElementDeallocate overloads have no implementation

The Lagrange and hierarchical classes for each of the following topologies
declare Deallocate_Vector and Deallocate_Ptr_Vector but provide no matching
module procedure implementations in their active submodules:

- LineH1
- TriangleH1
- QuadrangleH1
- TetrahedronH1
- HexahedronH1
- PrismH1
- PyramidH1

Each class exports both through FiniteElementDeallocate. All 14 examples
named `<Class>_Deallocate_test_1.F90` compile their source but fail to link.
For example:

```text
Undefined symbols for architecture arm64:
  ___hexahedronh1lagrangefe_class_MOD_deallocate_ptr_vector
  ___hexahedronh1lagrangefe_class_MOD_deallocate_vector
```

Expected: both advertised overloads link and release their wrapper/value
arrays. Pointer-target ownership must remain explicit: resetting a target
does not itself free its allocation.

The equivalent cleanup examples for all three OneDim classes pass.

### 2. Triangle hierarchical SetOrder stores four facet columns

- Class: TriangleH1HierarchicalFE_.
- Implementation: `obj_SetOrder` in the corresponding Methods submodule.
- Reproducer: `TriangleH1HierarchicalFE_SetOrder_test_1.F90`.

The scalar-order branch creates faceOrder0(3,4) and calls SetFaceOrder
with tFace=4, although a triangle has three facets. The DOF count for the
cubic example is ten, but opt%GetFaceOrder returns ncol=4 and the
three-column check fails.

Expected: store three active facet-order columns, consistently with the
constructor and the topology. The explicit entity-order branch uses the
supplied sizes and does not contain this hardcoded four-column setting.

### 3. Line hierarchical pointer constructor does not set cellOrder

- Constructor: LineH1HierarchicalFEPointer with arguments.
- Reproducer: `LineH1HierarchicalFE_Constructor_test_1.F90`.

The constructor passes order to inherited Initiate with the Hierarchical
family. BasisOpt_'s hierarchical path uses cellOrder instead of that
isotropic argument. The resulting target is allocated and initialized,
but its requested quadratic DOF count is not three, so the regression
check fails.

Expected: initialize the hierarchical cell degree from the requested
order and establish its DOF count. Calling the child SetOrder separately
works, as shown by the separate SetOrder example.

## Incomplete operations

### 4. Three-dimensional topology and basis methods are WIP placeholders

The tetrahedron, hexahedron, prism and pyramid topology classes each have
six placeholders:

- GetQuadraturePoints
- GetFacetQuadraturePoints
- SetQuadratureOrder
- SetQuadratureType
- GetTotalInterpolationPoints
- GetInterpolationPoints

Their Lagrange and hierarchical children each have four more:

- configured pointer constructor
- SetOrder
- GetLocalElemShapeData
- GetGlobalElemShapeData

All 56 corresponding examples stop with the declared under-development
diagnostic in DEBUG mode. They test the placeholders directly so one
unfinished operation does not hide another. Empty pointer construction
is implemented and passes for all these classes.

These placeholders do not perform the operation suggested by their
interface names; their docblocks state this explicitly.

### 5. Line facet quadrature remains unsupported

- Reproducer: `LineH1LagrangeFE_Quadrature_test_1.F90`.
- Related report: `bugs_report/BasisOpt.md`, item 4.

The cell quadrature checks pass, then GetFacetQuadraturePoints delegates
through BasisOpt_ to the QuadratureOpt_ line-facet WIP placeholder and
stops with 666.

## Source-inspection findings not separately reproduced

### 6. Optional onlyFaceBubble is read without a presence/default step

OneDimLagrangeFE_'s obj_GetDOFValueFromConstant and
obj_GetTimeDOFValueFromConstant declare onlyFaceBubble OPTIONAL but
evaluate `IF (onlyFaceBubble)` directly. The examples pass it explicitly;
the omitted-argument path was not executed.

Expected: either provide a defined default for omission or change the
interface contract. This concerns a valid omitted optional argument,
rather than additional checking of invalid input.

### 7. Triangle facet endpoint sampling precedes time initialization

In TriangleH1FE_'s obj_GetFacetDOFValueFromSTFunc, the onlyFaceBubble
branches evaluate endpoint values using args before `args = times` is
executed. Only the spatial entries have been assigned at that point.
For a time-dependent user function, the endpoint time is therefore not
established before evaluation. The vector-return branch follows the
same pattern.

The current facet-function example uses a constant and does not expose
this time-dependent case. A focused regression should sample a function
depending on time with onlyFaceBubble true.

### 8. Stale private triangle user-function declaration

TriangleH1FE_Class declares obj_GetFacetDOFValueFromUserFunction, but
there is no matching implementation or type binding. The working public
binding is obj_GetFacetDOFValueFromSTFunc. No example calls the private
legacy declaration.
