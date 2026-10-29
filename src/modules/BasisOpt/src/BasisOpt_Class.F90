! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Basis configuration, quadrature and H1 shape-function operations
!
! Defines BasisOpt_ and the immutable TypeBasisOpt default value. Public
! bindings configure and evaluate line, triangle and quadrangle bases.
!
MODULE BasisOpt_Class
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
USE BaseType, ONLY: ipopt => TypeInterpolationOpt
USE BaseType, ONLY: polyopt => TypePolynomialOpt
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: elemnameopt => TypeElemNameOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: eleminfo => TypeRefelemOpt
USE QuadratureOpt_Class, ONLY: QuadratureOpt_
USE ExceptionHandler_Class, ONLY: e
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE
CHARACTER(*), PARAMETER :: modName = "BasisOpt_Class"
INTEGER(I4B), PARAMETER :: FE_DOF_POINT_EVAL = 1_I4B
INTEGER(I4B), PARAMETER :: DEFAULT_DOF_TYPE(4) = [1, 1, 1, 1]
INTEGER(I4B), PARAMETER :: FE_TRANSFORM_IDENTITY = 1_I4B
INTEGER(I4B), PARAMETER :: DEFAULT_TRANSFORM_TYPE = 1_I4B

PUBLIC :: BasisOpt_
PUBLIC :: TypeBasisOpt

!----------------------------------------------------------------------------
!                                                             BasisOpt_Class
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Configuration and workspaces for multidimensional basis functions
!
! Stores private flags, geometry/topology metadata, approximation degrees,
! orientations, polynomial parameters, reference coordinates, quadrature
! options and reusable allocatable work arrays. TypeBasisOpt supplies default
! values: uninitialized, zero dimensions and DOFs, scalar H1 Lagrange,
! equidistant points, Monomial basis, alpha=beta=0 and lambda=0.5. Directional
! data use three entries. Face arrays use three rows and maxFaces columns;
! edge arrays use maxEdges entries. Reference vertices use a (3,8) array.
! Entity order and orientation methods share the same active size counters.
! Use Initiate to establish consistent topology and quadrature metadata;
! low-level setters do not recompute all derived values. Shape methods may
! resize internal caches and modify firstCall. Call DEALLOCATE to restore
! defaults and release work arrays.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: order, basis(3), dofs(4), transform
! REAL(DFP) :: alpha(3), beta(3), lambda(3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(order=order, basisType=basis, dofType=dofs, &
!   transformType=transform, alpha=alpha, beta=beta, lambda=lambda)
! CALL OK(order == 2_I4B, "Initial order")
! CALL OK(obj%GetNSD() == 2_I4B, "Spatial dimension")
! CALL OK(obj%GetTotalDOF() == 6_I4B, "Quadratic triangle DOF count")
! CALL OK(ALL(dofs == 1_I4B), "Default DOF types")
! CALL OK(ALL(ABS(alpha) < 1.0E-12_DFP), "Default Jacobi alpha")
! CALL OK(ALL(ABS(beta) < 1.0E-12_DFP), "Default Jacobi beta")
! CALL OK(ALL(ABS(lambda - 0.5_DFP) < 1.0E-12_DFP), "Default lambda")
! CALL obj%DEALLOCATE()
! CALL OK(obj%GetNSD() == 0_I4B, "Deallocate restores default dimension")
! CALL OK(obj%GetTotalDOF() == 0_I4B, "Deallocate resets DOF count")
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_SetOrder]], [[obj_Deallocate]]

TYPE :: BasisOpt_
  PRIVATE
  LOGICAL(LGT) :: firstCall = .TRUE.
  !! Whether the next Lagrange evaluation must construct its coefficients.

  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is set to true at the time of constructor

  LOGICAL(LGT) :: isIsotropicOrder = .FALSE.
  !! flag to check if the order is isotropic or not
  !! see order

  LOGICAL(LGT) :: isAnisotropicOrder = .FALSE.
  !! flag to check if the order is anisotropic or not
  !! see anisoOrder

  LOGICAL(LGT) :: isIpType = .FALSE.
  !! Whether an interpolation point type was explicitly supplied.

  LOGICAL(LGT) :: isEdgeOrder = .FALSE.
  !! True if we set the edge order

  LOGICAL(LGT) :: isFaceOrder = .FALSE.
  !! True if we set the face order

  LOGICAL(LGT) :: isCellOrder = .FALSE.
  !! True if we set the cell order

  LOGICAL(LGT) :: isCellOrient = .FALSE.
  !! True if we set the cell orientation

  LOGICAL(LGT) :: isFaceOrient = .FALSE.
  !! True if we set the face orientation

  LOGICAL(LGT) :: isEdgeOrient = .FALSE.
  !! True if we set the edge orientation

  INTEGER(I4B) :: tdof = 0
  !! total number of degrees of freedom

  INTEGER(I4B) :: nsd = 0
  !! number of spatial dimensions

  INTEGER(I4B) :: xidim = 0
  !! Reference coordinate dimension (not the codimension).

  INTEGER(I4B) :: topoType = 0
  !! Topology type of element, also see topoType_char
  !! point, line, triangle, quadrangle, tetrahedron, hexahedron,
  !! prism, pyramid

  INTEGER(I4B) :: elemType = 0
  !! Topology type of reference element
  !! line, triangle, quadrangle, tetrahedron, hexahedron,
  !! prism, pyramid

  INTEGER(I4B) :: elemIndx = 0
  !! Element index of topoType

  INTEGER(I4B) :: feType = fevaropt%Scalar
  !! Type of finite element, Scalar, Vector, Matrix
  !! Numeric value type; feType_char is maintained separately.

  INTEGER(I4B) :: tEdgeOrder = 0
  !! Active size shared by edgeOrder and edgeOrient.
  INTEGER(I4B) :: tFaceOrder = 0
  !! Active column count shared by faceOrder and faceOrient.
  INTEGER(I4B) :: tCellOrder = 0
  !! Active size shared by cellOrder and cellOrient.

  INTEGER(I4B) :: transformType = 0
  !! Currently it is not used
  !! Type of Tranformation usef for polynomial space
  !! Initiate currently leaves this at its default zero.

  INTEGER(I4B) :: ipType = ipopt%Equidistance
  !! Type of lattice point for interpol (i.e., interpolation point type)
  !! Used for constructing Lagrange interpolation points.
  !! ipType_char is separate display metadata.

  INTEGER(I4B) :: order = 0
  !! isotropic order of the basis function

  INTEGER(I4B) :: anisoOrder(3) = 0
  !! anisotropic order of the basis function in x, y, z direction

  INTEGER(I4B) :: edgeOrder(eleminfo%maxEdges) = 0
  !! Order on each edge of the element
  INTEGER(I4B) :: edgeOrient(eleminfo%maxEdges) = 0
  !! Orientation on each edge of the element

  INTEGER(I4B) :: faceOrder(3, eleminfo%maxFaces) = 0
  !! Order of approximation on each face of the element
  INTEGER(I4B) :: faceOrient(3, eleminfo%maxFaces) = 0
  !! orientation on each face

  INTEGER(I4B) :: cellOrder(3) = 0
  !! Order of approximation inside the element
  INTEGER(I4B) :: cellOrient(3) = 0
  !! Orientation of each cell

  INTEGER(I4B) :: dofType(4) = DEFAULT_DOF_TYPE
  !! Currently it is not used
  !! dofType(1): Type of dof for shape function defined on vertex
  !! dofType(2): Type of dof for shape functions on edge
  !! dofType(3): Type of dof for shape functions on face
  !! dofType(4): Type of dof for shape functions in cell
  !! These shape functions can take following values:
  !! - FE_DOF_POINT_EVAL

  INTEGER(I4B) :: basisType(3) = polyopt%Monomial
  !! Integer code for basis type in x, y, and z direction
  !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto
  !! Ultraspherical
  !! basisType_char is separate display metadata.

  REAL(DFP) :: alpha(3) = 0.0_DFP
  !! Jacobi parameters
  REAL(DFP) :: beta(3) = 0.0_DFP
  !! Jacobi parameters

  REAL(DFP) :: lambda(3) = 0.5_DFP
  !! Ultraspherical parameters

  CHARACTER(1) :: refelemDomain = "B"
  !! Reference domain code used for coordinates, basis and quadrature.
  !! It can take following values:
  !! - UNIT "U"
  !! - BIUNIT "B"

  CHARACTER(2) :: baseContinuity = "H1"
  !! Two-character continuity code; initialized from the uppercased prefix.
  !! H1, HC (HCurl), HD (HDiv), or DG.

  CHARACTER(4) :: baseInterpolation = "LAGR"
  !! Four-character family code used for interpolation on the reference
  !! element, Following values are allowed
  !! LagrangeInterpolation, Lagrange
  !! HierarchyInterpolation, Hierarchy
  !! OrthogonalInterpolation, Orthogonal
  !! HermitInterpolation, Hermit
  !! SerendipityInterpolation, Serendipity

  REAL(DFP) :: refelemCoord(3, 8) = 0.0_DFP
  !! Reference vertex coordinates; active columns depend on topology.

  CHARACTER(128) :: basisType_char(3) = "MONOMIAL"
  !! basis type in string format

  CHARACTER(128) :: ipType_char = "EQUIDISTANCE"
  !! interpolation type in string format

  CHARACTER(6) :: feType_char = "SCALAR"
  !! finite element type in string format
  !! scalar, vector, matrix

  TYPE(QuadratureOpt_) :: quadOpt
  !! Quadrature options

  REAL(DFP), ALLOCATABLE :: coeff(:, :), xx(:, :)
  !! coefficient necessary for lagrange Interpolation
  !! coefficient matrix needed for Lagrange interpolation
  !! Coeff helps us in reducing the computation time for Lagrange polynomials
  !! xx is evaluation of basis points

  REAL(DFP), ALLOCATABLE :: xij(:, :)
  !! Interpolation points used for Lagrange interpolation
  !! Private scratch storage used during local shape evaluation.

  REAL(DFP), ALLOCATABLE :: temp(:, :, :)
  !! temporary array used in case of Lagrange Interpolation
  !! Private scratch storage used during local shape evaluation.

CONTAINS

  ! CONSTRUCTOR:
  !@ConstructorMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Restore default settings and release working arrays.
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate => obj_Initiate
  !! Initialize basis and quadrature options.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy basis options and allocated working arrays.
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Initiate by copy

  !IO:
  !@IOMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display configuration and working-array allocation state.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: MdEncode => obj_MdEncode
  !! Encode selected basis options as Markdown.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ReactEncode => &
    obj_ReactEncode
  !! Encode selected basis options as React markup.
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml1 => &
    obj_ImportFromToml1
  !! Import basis and quadrature configuration from a TOML table.
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import a named basis table from a TOML document.
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Import from toml file

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Assign selected stored configuration values.
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Select approximation orders and compute the DOF count.
  PROCEDURE, PUBLIC, PASS(obj) :: SetCellOrder => obj_SetCellOrder
  !! Set cell order data and activity state.
  PROCEDURE, PUBLIC, PASS(obj) :: SetFaceOrder => obj_SetFaceOrder
  !! Set face order data and activity state.
  PROCEDURE, PUBLIC, PASS(obj) :: SetEdgeOrder => obj_SetEdgeOrder
  !! Set edge order data and activity state.
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsotropicOrder => obj_SetIsotropicOrder
  !! Set isotropic degree and recompute Lagrange DOFs.
  PROCEDURE, PUBLIC, PASS(obj) :: SetAnisotropicOrder => &
    obj_SetAnisotropicOrder
  !! Set directional degrees and recompute Lagrange DOFs.
  PROCEDURE, PASS(obj) :: ResetAnisotropicOrder => &
    obj_ResetAnisotropicOrder
  !! Restore default directional degrees and activity flag.
  PROCEDURE, PASS(obj) :: ResetIsotropicOrder => &
    obj_ResetIsotropicOrder
  !! Restore default isotropic degree and activity flag.
  PROCEDURE, PASS(obj) :: SetTotalDOF => obj_SetTotalDOF
  !! Assign the stored DOF count.
  PROCEDURE, PUBLIC, PASS(obj) :: Line_SetQuadratureOrder
  !! Set Quadrature Order for a line.
  PROCEDURE, PUBLIC, PASS(obj) :: Triangle_SetQuadratureOrder
  !! Set Quadrature Order for a triangle.
  PROCEDURE, PUBLIC, PASS(obj) :: Quadrangle_SetQuadratureOrder
  !! Set Quadrature Order for a quadrangle.
  PROCEDURE, PUBLIC, PASS(obj) :: Line_SetQuadratureType
  !! Set Quadrature Type for a line.
  PROCEDURE, PUBLIC, PASS(obj) :: Triangle_SetQuadratureType
  !! Set Quadrature Type for a triangle.
  PROCEDURE, PUBLIC, PASS(obj) :: Quadrangle_SetQuadratureType
  !! Set Quadrature Type for a quadrangle.
  PROCEDURE, PUBLIC, PASS(obj) :: SetCellOrientation => obj_SetCellOrientation
  !! Set cell orientation data and activity state.
  PROCEDURE, PUBLIC, PASS(obj) :: SetFaceOrientation => &
    obj_SetFaceOrientation
  !! Set face orientation data and activity state.
  PROCEDURE, PUBLIC, PASS(obj) :: SetEdgeOrientation => &
    obj_SetEdgeOrientation
  !! Set edge orientation data and activity state.
  PROCEDURE, PUBLIC, PASS(obj) :: Line_FixOrientOfNptrs
  !! Orient line connectivity.
  PROCEDURE, PUBLIC, PASS(obj) :: Quadrangle_FixOrientOfNptrs
  !! Orient quadrangle connectivity.
  PROCEDURE, PUBLIC, PASS(obj) :: Triangle_FixOrientOfNptrs
  !! Orient triangle connectivity.

  !GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFace => obj_GetTotalFace
  !! Return the number of reference facets.
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalVertex => obj_GetTotalVertex
  !! Return the number of reference vertices.
  PROCEDURE, PUBLIC, PASS(obj) :: GetNSD => obj_GetNSD
  !! Return the physical spatial dimension.
  PROCEDURE, PUBLIC, PASS(obj) :: GetRefElemCoord => obj_GetRefElemCoord
  !! Copy reference vertex coordinates.
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalDOF => obj_GetTotalDOF
  !! Return the stored basis DOF count.
  PROCEDURE, PUBLIC, PASS(obj) :: GetTopologyType => &
    obj_GetTopologyType
  !! Return the reference topology code.
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Read selected stored configuration values.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetTotalQuadraturePoints => obj_GetTotalQuadraturePoints
  !! Return the configured cell quadrature point count.
  PROCEDURE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Return the four-character basis family code.
  PROCEDURE, PUBLIC, PASS(obj) :: GetBaseContinuity => &
    obj_GetBaseContinuity
  !! Return the two-character continuity code.
  PROCEDURE, PUBLIC, PASS(obj) :: GetIpType => obj_GetIpType
  !! Return the interpolation point type code.
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellOrder => obj_GetCellOrder
  !! Copy active cell polynomial orders.
  PROCEDURE, PUBLIC, PASS(obj) :: GetFaceOrder => obj_GetFaceOrder
  !! Copy active face polynomial orders.
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeOrder => obj_GetEdgeOrder
  !! Copy active edge polynomial orders.
  PROCEDURE, PUBLIC, PASS(obj) :: Line_GetQuadraturePoints
  !! Get Quadrature Points for a line.
  PROCEDURE, PUBLIC, PASS(obj) :: Triangle_GetQuadraturePoints
  !! Get Quadrature Points for a triangle.
  PROCEDURE, PUBLIC, PASS(obj) :: Quadrangle_GetQuadraturePoints
  !! Get Quadrature Points for a quadrangle.
  PROCEDURE, PUBLIC, PASS(obj) :: Line_GetFacetQuadraturePoints
  !! Get Facet Quadrature Points for a line.
  PROCEDURE, PUBLIC, PASS(obj) :: Triangle_GetFacetQuadraturePoints
  !! Get Facet Quadrature Points for a triangle.
  PROCEDURE, PUBLIC, PASS(obj) :: Quadrangle_GetFacetQuadraturePoints
  !! Get Facet Quadrature Points for a quadrangle.

  !@ LineH1LagrangeFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1LagFE_GetLocalElemShapeData
  !! Evaluate local cell shapes for Line H1 Lagrange.
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1LagFE_GetLocalFacetElemShapeData
  !! Evaluate local facet shapes for Line H1 Lagrange.
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1LagFE_GetGlobalElemShapeData
  !! Map cell shapes to physical coordinates for Line H1 Lagrange.

  !@ LineH1HierarchicalFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1HieFE_GetLocalElemShapeData
  !! Evaluate local cell shapes for Line H1 hierarchical.
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1HieFE_GetLocalFacetElemShapeData
  !! Evaluate local facet shapes for Line H1 hierarchical.
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1HieFE_GetGlobalElemShapeData
  !! Map cell shapes to physical coordinates for Line H1 hierarchical.

  !@ TriangleH1LagrangeFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1LagFE_GetLocalElemShapeData
  !! Evaluate local cell shapes for Triangle H1 Lagrange.
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1LagFE_GetLocalFacetElemShapeData
  !! Evaluate local facet shapes for Triangle H1 Lagrange.
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1LagFE_GetGlobalElemShapeData
  !! Map cell shapes to physical coordinates for Triangle H1 Lagrange.
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1LagFE_GetGlobalFacetElemShapeData
  !! Map facet shapes and compute normals for Triangle H1 Lagrange.

  !@ TriangleH1HierarchicalFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1HieFE_GetLocalElemShapeData
  !! Evaluate local cell shapes for Triangle H1 hierarchical.
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1HieFE_GetLocalFacetElemShapeData
  !! Evaluate local facet shapes for Triangle H1 hierarchical.
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1HieFE_GetGlobalElemShapeData
  !! Map cell shapes to physical coordinates for Triangle H1 hierarchical.
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1HieFE_GetGlobalFacetElemShapeData
  !! Map facet shapes and compute normals for Triangle H1 hierarchical.

  !@ QuadrangleH1LagrangeFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1LagFE_GetLocalElemShapeData
  !! Evaluate local cell shapes for Quadrangle H1 Lagrange.
  PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1LagFE_GetLocalFacetElemShapeData
  !! Evaluate local facet shapes for Quadrangle H1 Lagrange.
  PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1LagFE_GetGlobalElemShapeData
  !! Map cell shapes to physical coordinates for Quadrangle H1 Lagrange.
 PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1LagFE_GetGlobalFacetElemShapeData
  !! Map facet shapes and compute normals for Quadrangle H1 Lagrange.

  !@ QuadrangleH1HierarchicalFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1HieFE_GetLocalElemShapeData
  !! Evaluate local cell shapes for Quadrangle H1 hierarchical.
  PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1HieFE_GetLocalFacetElemShapeData
  !! Evaluate local facet shapes for Quadrangle H1 hierarchical.
  PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1HieFE_GetGlobalElemShapeData
  !! Map cell shapes to physical coordinates for Quadrangle H1 hierarchical.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    QuadrangleH1HieFE_GetGlobalFacetElemShapeData
  !! Map facet shapes and compute normals for Quadrangle H1 hierarchical.

END TYPE BasisOpt_

!----------------------------------------------------------------------------
!                                                              TypeBasisOpt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Immutable factory defaults for BasisOpt_
!
! Assign to an object to copy factory defaults, or call DEALLOCATE to also
! release existing working arrays. Copying an unallocated source workspace
! does not itself release an allocated destination workspace.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BasisOpt_Class, ONLY: TypeBasisOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
!
! obj = TypeBasisOpt
! CALL OK(obj%GetNSD() == 0_I4B, "Default spatial dimension")
! CALL OK(obj%GetTotalDOF() == 0_I4B, "Default DOF count")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Default family")
! CALL OK(obj%GetBaseContinuity() == "H1", "Default continuity")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Copy]], [[obj_Deallocate]]
TYPE(BasisOpt_), PARAMETER :: TypeBasisOpt = BasisOpt_( &
                              xij=NULL(), temp=NULL(), coeff=NULL())

!----------------------------------------------------------------------------
!                                                           BasisOptPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Private pointer wrapper for BasisOpt_
!
! This type is not exported. Its pointer has no default initialization;
! establish its association status before using the private cleanup helper.

TYPE :: BasisOptPointer_
  CLASS(BasisOpt_), POINTER :: ptr
  !! Pointer target; association status is not initialized by this type.
END TYPE BasisOptPointer_

!----------------------------------------------------------------------------
!                                  CheckErrorHierarchical@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Reserved hierarchical validation interface
!
! Private declaration with no matching module procedure implementation in the
! active BasisOpt submodules. It is not bound to BasisOpt_ and cannot be
! called through the public API. The legacy ParameterConstructorMethods file
! is excluded from the submodule CMake source list.
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_CheckErrorHierarchical(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
  END SUBROUTINE obj_CheckErrorHierarchical
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Initialize basis and quadrature options
!
! Resets the object to TypeBasisOpt, sets initialization state, computes
! topology, reference dimension and reference vertex coordinates, and stores
! the basis family and continuity codes. Initializes orders and orientations
! through SetOrder, then initializes quadOpt. One-element basisType, alpha,
! beta and lambda arrays broadcast over xidim. dofType and transformType are
! accepted but currently not assigned. Approximation order handling depends on
! the selected basis family.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: order, basis(3), dofs(4), transform
! REAL(DFP) :: alpha(3), beta(3), lambda(3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(order=order, basisType=basis, dofType=dofs, &
!   transformType=transform, alpha=alpha, beta=beta, lambda=lambda)
! CALL OK(order == 2_I4B, "Initial order")
! CALL OK(obj%GetNSD() == 2_I4B, "Spatial dimension")
! CALL OK(obj%GetTotalDOF() == 6_I4B, "Quadratic triangle DOF count")
! CALL OK(ALL(dofs == 1_I4B), "Default DOF types")
! CALL OK(ALL(ABS(alpha) < 1.0E-12_DFP), "Default Jacobi alpha")
! CALL OK(ALL(ABS(beta) < 1.0E-12_DFP), "Default Jacobi beta")
! CALL OK(ALL(ABS(lambda - 0.5_DFP) < 1.0E-12_DFP), "Default lambda")
! CALL obj%DEALLOCATE()
! CALL OK(obj%GetNSD() == 0_I4B, "Deallocate restores default dimension")
! CALL OK(obj%GetTotalDOF() == 0_I4B, "Deallocate resets DOF count")
! END PROGRAM main
!```
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%TriangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, elemType, nsd, baseContinuity, baseInterpolation, ipType, &
    basisType, alpha, beta, lambda, feType, dofType, transformType, order, &
    anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, faceOrient, &
    edgeOrient, tcell, tface, tedge, errCheck, quadratureIsHomogeneous, &
    quadratureType, quadratureOrder, quadratureIsOrder, quadratureNips, &
    quadratureIsNips, quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of physical spatial dimensions.
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity name; Initiate uppercases its first two characters.
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Family name; Initiate uppercases its first four characters.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type; defaults to Equidistance on initialization.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Polynomial basis codes by reference direction; default Monomial.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi alpha values by reference direction; default zero.
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi beta values by reference direction; default zero.
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters by reference direction; default 0.5.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
    !! Finite element value type code; default scalar.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! Four DOF classification codes; default [1,1,1,1]. Currently accepted
    !! but ignored by Initiate.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! Transformation classification code; default zero. Currently accepted
    !! but ignored by Initiate.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic polynomial degree, or quadrature accuracy in quadrature
    !! setters.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Directional polynomial degrees; one entry broadcasts over xidim.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Cell polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Face polynomial orders, three rows and one column per active face.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Edge polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! Cell orientation data; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! Face orientations; shape (3, tFace), or one entry for broadcast.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! Edge orientation data; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! Active size of cell order or orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! Active number of face columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! Whether the same quadrature settings apply in all directions.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureOrder(:)
    !! Quadrature accuracy orders by reference direction.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Whether quadrature is specified by accuracy order.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! Quadrature point counts by reference direction.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Whether quadrature is specified by point count.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Quadrature Jacobi alpha parameters by direction.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    !! Quadrature Jacobi beta parameters by direction.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    !! Quadrature ultraspherical parameters by direction.
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Copy basis options and allocated working arrays
!
! Copies scalar fields, fixed-size arrays and quadOpt. Allocated source work
! arrays are copied by intrinsic allocatable assignment. If a source work
! array is unallocated, the corresponding destination array is left as it was;
! this procedure does not first deallocate the destination. Defined assignment
! uses this same procedure.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: other
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
! INTEGER(I4B) :: order
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1LagFE_GetLocalElemShapeData(shapes, quad)
! CALL other%Copy(obj)
! CALL obj%DEALLOCATE()
! CALL other%GetParam(order=order)
! CALL OK(order == 2_I4B, "Copied order survives source cleanup")
! CALL other%TriangleH1LagFE_GetLocalElemShapeData(shapes, quad)
! CALL OK(shapes%nns == 6_I4B, "Copied shape workspace remains usable")
! obj = other
! CALL other%SetIsotropicOrder(order=1_I4B)
! CALL OK(obj%GetTotalDOF() == 6_I4B, "Assignment is independent")
! CALL obj%DEALLOCATE()
! CALL other%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    CLASS(BasisOpt_), INTENT(IN) :: obj2
    !! Source options; allocated working arrays are copied by value.
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Restore default settings and release working arrays
!
! Copies TypeBasisOpt and explicitly deallocates coeff, xx, xij and temp when
! allocated. Resets dimensions and DOF count to zero, clears activity flags
! and restores basis and quadrature defaults. The object remains available for
! reuse.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: order, basis(3), dofs(4), transform
! REAL(DFP) :: alpha(3), beta(3), lambda(3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(order=order, basisType=basis, dofType=dofs, &
!   transformType=transform, alpha=alpha, beta=beta, lambda=lambda)
! CALL OK(order == 2_I4B, "Initial order")
! CALL OK(obj%GetNSD() == 2_I4B, "Spatial dimension")
! CALL OK(obj%GetTotalDOF() == 6_I4B, "Quadratic triangle DOF count")
! CALL OK(ALL(dofs == 1_I4B), "Default DOF types")
! CALL OK(ALL(ABS(alpha) < 1.0E-12_DFP), "Default Jacobi alpha")
! CALL OK(ALL(ABS(beta) < 1.0E-12_DFP), "Default Jacobi beta")
! CALL OK(ALL(ABS(lambda - 0.5_DFP) < 1.0E-12_DFP), "Default lambda")
! CALL obj%DEALLOCATE()
! CALL OK(obj%GetNSD() == 0_I4B, "Deallocate restores default dimension")
! CALL OK(obj%GetTotalDOF() == 0_I4B, "Deallocate resets DOF count")
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Reset and detach a vector of pointer wrappers
!
! Private helper behind the private DEALLOCATE generic. Resets each associated
! target, nullifies its wrapper and deallocates the wrapper array. Target
! allocations are not freed. BasisOptPointer_ and this generic are not
! exported from the module.
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE DEALLOCATE
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(BasisOptPointer_), ALLOCATABLE :: obj(:)
    !! Basis options object.
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Display configuration and working-array allocation state
!
! Writes the message, initialization state, basis options and allocated-array
! shapes through Display_Method, then displays quadOpt. Returns after the
! initialization flag when the object is not initialized. notFull is currently
! unused. Output-unit behavior depends on the base display utility.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
!
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Display(msg="Basis options")
! CALL OK(obj%GetTotalDOF() == 6_I4B, "Display preserves basis options")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno, notFull)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    CHARACTER(*), INTENT(IN) :: msg
    !! Message printed before the object state.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! Optional output unit passed to Display.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull
    !! Accepted for compatibility; not used by the implementation.
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         MdEncode@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Encode selected basis options as Markdown
!
! Builds a 21-row table containing dimension, family, polynomial parameters
! and order flags. The face-order row is currently emitted as a blank value
! even when face orders are active. Uses the base ToString and Markdown
! encoding utilities.
!
! Returns:
! ans : TYPE(String) - Markdown table encoding of the selected basis settings.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE String_Class, ONLY: String
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(String) :: text
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! text = obj%MdEncode()
! CALL OK(LEN(text%chars()) > 0, "MdEncode produces output")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_MdEncode(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    TYPE(String) :: ans
    !! Markdown table encoding of the selected basis settings.
  END FUNCTION obj_MdEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ReactEncode@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Encode selected basis options as React markup
!
! Builds a 21-row table through the base React encoding utilities. Includes
! scalar settings and selected order arrays. The face-order row is currently
! emitted as a blank value even when face orders are active.
!
! Returns:
! ans : TYPE(String) - React table encoding of the selected basis settings.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE String_Class, ONLY: String
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(String) :: text
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! text = obj%ReactEncode()
! CALL OK(LEN(text%chars()) > 0, "ReactEncode produces output")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_ReactEncode(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    TYPE(String) :: ans
    !! React table encoding of the selected basis settings.
  END FUNCTION obj_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-27
! summary: Assign selected stored configuration values
!
! Assigns supplied fields directly and forwards quadrature settings to
! quadOpt%SetParam. It does not recompute topology from elemType, reference
! coordinates from topology, activity flags from orders, DOF count or basis
! workspaces. Supply mutually consistent values. Directional fixed-size arrays
! must have three entries and dofType four. The current faceOrder assignment
! uses SIZE(faceOrder) as its column bound; see the bug report before using
! this argument.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: basis(3), dofs(4), transform
! REAL(DFP) :: alpha(3), beta(3), lambda(3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetParam(alpha=[0.1_DFP, 0.2_DFP, 0.3_DFP], &
!   beta=[0.4_DFP, 0.5_DFP, 0.6_DFP], &
!   lambda=[0.7_DFP, 0.8_DFP, 0.9_DFP], &
!   dofType=[2_I4B, 2_I4B, 2_I4B, 2_I4B], transformType=2_I4B)
! CALL obj%GetParam(alpha=alpha, beta=beta, lambda=lambda, &
!   dofType=dofs, transformType=transform)
! CALL OK(ABS(alpha(2) - 0.2_DFP) < 1.0E-12_DFP, "Alpha updated")
! CALL OK(ABS(beta(2) - 0.5_DFP) < 1.0E-12_DFP, "Beta updated")
! CALL OK(ABS(lambda(2) - 0.8_DFP) < 1.0E-12_DFP, "Lambda updated")
! CALL OK(ALL(dofs == 2_I4B), "DOF types updated")
! CALL OK(transform == 2_I4B, "Transform type updated")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, nsd, xidim, order, anisoOrder, edgeOrder, faceOrder, cellOrder, &
    fetype, elemType, topoType, elemIndx, ipType, basisType, alpha, beta, &
    lambda, dofType, transformType, refElemDomain, refelemCoord, &
    baseContinuity, baseInterpolation, isIsotropicOrder, isAnisotropicOrder, &
  isEdgeOrder, isFaceOrder, isCellOrder, tEdgeOrder, tFaceOrder, tCellOrder, &
    quadratureIsHomogeneous, quadratureType, quadratureType1, &
    quadratureType2, quadratureType3, quadratureOrder, quadratureOrder1, &
    quadratureOrder2, quadratureOrder3, quadratureIsOrder, quadratureNips, &
    quadratureNips1, quadratureNips2, quadratureNips3, quadratureIsNips, &
    quadratureAlpha, quadratureAlpha1, quadratureAlpha2, quadratureAlpha3, &
    quadratureBeta, quadratureBeta1, quadratureBeta2, quadratureBeta3, &
    quadratureLambda, quadratureLambda1, quadratureLambda2, quadratureLambda3)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of physical spatial dimensions.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidim
    !! Reference coordinate dimension; distinct from physical nsd.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic polynomial degree, or quadrature accuracy in quadrature
    !! setters.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Directional approximation degrees; SIZE(anisoOrder) = 3.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Edge polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Face-order input; current column-bound handling is defective (see
    !! report).
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Cell polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element value type code; default scalar.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
    !! Reference element topology code.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemIndx
    !! Reference element index.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type; defaults to Equidistance on initialization.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Polynomial basis codes; SIZE(basisType) = 3.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi alpha values; SIZE(alpha) = 3.
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi beta values; SIZE(beta) = 3.
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters; SIZE(lambda) = 3.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! Four DOF classification codes; default [1,1,1,1].
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! Transformation classification code; default zero.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    !! Continuity name; Initiate uppercases its first two characters.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
    !! Family name; Initiate uppercases its first four characters.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refElemDomain
    !! Reference domain code; only the first character is stored.
    REAL(DFP), INTENT(IN), OPTIONAL :: refelemCoord(:, :)
    !! Reference vertex coordinate array; stored shape is (3, 8).
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isIsotropicOrder
    !! Whether isotropic approximation order is active.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isAnisotropicOrder
    !! Whether anisotropic approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isEdgeOrder
    !! Whether edge-specific approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFaceOrder
    !! Whether face-specific approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isCellOrder
    !! Whether cell-specific approximation orders are active.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdgeOrder
    !! Number of active edge orders.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFaceOrder
    !! Number of active face-order columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCellOrder
    !! Number of active cell orders.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! Whether the same quadrature settings apply in all directions.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    !! Quadrature rule type codes by reference direction in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    !! Quadrature rule type codes by reference direction in direction 2.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
    !! Quadrature rule type codes by reference direction in direction 3.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! Quadrature accuracy orders by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder1
    !! Quadrature accuracy orders by reference direction in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder2
    !! Quadrature accuracy orders by reference direction in direction 2.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder3
    !! Quadrature accuracy orders by reference direction in direction 3.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Whether quadrature is specified by accuracy order.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! Quadrature point counts by reference direction.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips1
    !! Quadrature point counts by reference direction in direction 1.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips2
    !! Quadrature point counts by reference direction in direction 2.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips3
    !! Quadrature point counts by reference direction in direction 3.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Whether quadrature is specified by point count.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Quadrature Jacobi alpha parameters by direction.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha1
    !! Quadrature Jacobi alpha parameters by direction in direction 1.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha2
    !! Quadrature Jacobi alpha parameters by direction in direction 2.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha3
    !! Quadrature Jacobi alpha parameters by direction in direction 3.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    !! Quadrature Jacobi beta parameters by direction.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta1
    !! Quadrature Jacobi beta parameters by direction in direction 1.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta2
    !! Quadrature Jacobi beta parameters by direction in direction 2.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta3
    !! Quadrature Jacobi beta parameters by direction in direction 3.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    !! Quadrature ultraspherical parameters by direction.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda1
    !! Quadrature ultraspherical parameters by direction in direction 1.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda2
    !! Quadrature ultraspherical parameters by direction in direction 2.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda3
    !! Quadrature ultraspherical parameters by direction in direction 3.
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                              SetCellOrientation@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set cell orientation data and activity state
!
! Sets the corresponding activity flag from PRESENT(cellOrient). If tCell is
! supplied, updates the shared active order/orientation size for this entity.
! Copies supplied values, broadcasting a one-entry input. With no data, clears
! the activity flag without clearing stored values or active size. Optional
! errCheck enables DEBUG shape and presence checks where applicable. Does not
! recompute tdof or reset shape caches.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nptrs(10), orient(3, 3), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,10)]
! orient = 1_I4B
! orient(1, 1) = -1_I4B
! CALL obj%SetCellOrientation(cellOrient=[-1_I4B], tCell=1_I4B)
! CALL obj%SetEdgeOrientation(edgeOrient=[1_I4B], tEdge=1_I4B)
! CALL obj%SetFaceOrientation(faceOrient=orient, tFace=3_I4B)
! CALL obj%Triangle_FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
! CALL OK(ALL(nptrs(1:3) == [(ii, ii=1,3)]), "Vertices retained")
! CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), &
!   "Reversed first edge")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetCellOrientation(obj, cellOrient, tCell, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! Cell orientation data; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! Active size of cell order or orientation data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetCellOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetFaceOrientation@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set face orientation data and activity state
!
! Sets the corresponding activity flag from PRESENT(faceOrient). If tFace is
! supplied, updates the shared active order/orientation size for this entity.
! Copies supplied values, broadcasting a one-entry input. With no data, clears
! the activity flag without clearing stored values or active size. Optional
! errCheck enables DEBUG shape and presence checks where applicable. Does not
! recompute tdof or reset shape caches.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nptrs(10), orient(3, 3), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,10)]
! orient = 1_I4B
! orient(1, 1) = -1_I4B
! CALL obj%SetCellOrientation(cellOrient=[-1_I4B], tCell=1_I4B)
! CALL obj%SetEdgeOrientation(edgeOrient=[1_I4B], tEdge=1_I4B)
! CALL obj%SetFaceOrientation(faceOrient=orient, tFace=3_I4B)
! CALL obj%Triangle_FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
! CALL OK(ALL(nptrs(1:3) == [(ii, ii=1,3)]), "Vertices retained")
! CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), &
!   "Reversed first edge")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetFaceOrientation(obj, faceOrient, tFace, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! Face orientations; shape (3, tFace), or one entry for broadcast.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! Active number of face columns.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetFaceOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                              SetEdgeOrientation@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set edge orientation data and activity state
!
! Sets the corresponding activity flag from PRESENT(edgeOrient). If tEdge is
! supplied, updates the shared active order/orientation size for this entity.
! Copies supplied values, broadcasting a one-entry input. With no data, clears
! the activity flag without clearing stored values or active size. Optional
! errCheck enables DEBUG shape and presence checks where applicable. Does not
! recompute tdof or reset shape caches.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nptrs(10), orient(3, 3), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,10)]
! orient = 1_I4B
! orient(1, 1) = -1_I4B
! CALL obj%SetCellOrientation(cellOrient=[-1_I4B], tCell=1_I4B)
! CALL obj%SetEdgeOrientation(edgeOrient=[1_I4B], tEdge=1_I4B)
! CALL obj%SetFaceOrientation(faceOrient=orient, tFace=3_I4B)
! CALL obj%Triangle_FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
! CALL OK(ALL(nptrs(1:3) == [(ii, ii=1,3)]), "Vertices retained")
! CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), &
!   "Reversed first edge")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetEdgeOrientation(obj, edgeOrient, tEdge, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! Edge orientation data; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetEdgeOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                                    FixOrientOfNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Orient line connectivity
!
! Currently leaves nptrs unchanged; the line implementation only emits DEBUG
! messages.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nptrs(4), orient(3, 2), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,4)]
! orient = 1_I4B
! orient(1, 1) = -1_I4B
! CALL obj%SetCellOrientation(cellOrient=[-1_I4B], tCell=1_I4B)
! CALL obj%SetEdgeOrientation(edgeOrient=[1_I4B], tEdge=1_I4B)
! CALL obj%SetFaceOrientation(faceOrient=orient, tFace=2_I4B)
! CALL obj%Line_FixOrientOfNptrs(nptrs=nptrs, tsize=4_I4B)
! CALL OK(ALL(nptrs(1:2) == [(ii, ii=1,2)]), "Vertices retained")
! CALL OK(ALL(nptrs == [1_I4B, 2_I4B, 3_I4B, 4_I4B]), &
!   "Line connectivity fixer currently leaves entries unchanged")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE Line_FixOrientOfNptrs(obj, nptrs, tsize)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! Node connectivity array; first tsize entries are active.
    INTEGER(I4B), INTENT(IN) :: tsize
    !! Number of active connectivity entries; currently unused.
  END SUBROUTINE Line_FixOrientOfNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                    FixOrientOfNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Orient quadrangle connectivity
!
! For isotropic degree greater than two and active face orientations, reverses
! each edge-interior block whose faceOrient(1,iface) is -1. The first 4 vertex
! entries and cell-interior entries stay unchanged. The active connectivity
! layout must match the polynomial degree. tsize is accepted but not used by
! this implementation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nptrs(16), orient(3, 4), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,16)]
! orient = 1_I4B
! orient(1, 1) = -1_I4B
! CALL obj%SetCellOrientation(cellOrient=[-1_I4B], tCell=1_I4B)
! CALL obj%SetEdgeOrientation(edgeOrient=[1_I4B], tEdge=1_I4B)
! CALL obj%SetFaceOrientation(faceOrient=orient, tFace=4_I4B)
! CALL obj%Quadrangle_FixOrientOfNptrs(nptrs=nptrs, tsize=16_I4B)
! CALL OK(ALL(nptrs(1:4) == [(ii, ii=1,4)]), "Vertices retained")
! CALL OK(ALL(nptrs(5:6) == [6_I4B, 5_I4B]), &
!   "Reversed first edge")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE Quadrangle_FixOrientOfNptrs(obj, nptrs, tsize)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! Node connectivity array; first tsize entries are active.
    INTEGER(I4B), INTENT(IN) :: tsize
    !! Number of active connectivity entries; currently unused.
  END SUBROUTINE Quadrangle_FixOrientOfNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                    FixOrientOfNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Orient triangle connectivity
!
! For isotropic degree greater than two and active face orientations, reverses
! each edge-interior block whose faceOrient(1,iface) is -1. The first 3 vertex
! entries and cell-interior entries stay unchanged. The active connectivity
! layout must match the polynomial degree. tsize is accepted but not used by
! this implementation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nptrs(10), orient(3, 3), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,10)]
! orient = 1_I4B
! orient(1, 1) = -1_I4B
! CALL obj%SetCellOrientation(cellOrient=[-1_I4B], tCell=1_I4B)
! CALL obj%SetEdgeOrientation(edgeOrient=[1_I4B], tEdge=1_I4B)
! CALL obj%SetFaceOrientation(faceOrient=orient, tFace=3_I4B)
! CALL obj%Triangle_FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
! CALL OK(ALL(nptrs(1:3) == [(ii, ii=1,3)]), "Vertices retained")
! CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), &
!   "Reversed first edge")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE Triangle_FixOrientOfNptrs(obj, nptrs, tsize)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! Node connectivity array; first tsize entries are active.
    INTEGER(I4B), INTENT(IN) :: tsize
    !! Number of active connectivity entries; currently unused.
  END SUBROUTINE Triangle_FixOrientOfNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Select approximation orders and compute the DOF count
!
! Dispatches using the first character of baseInterpolation. For Lagrange,
! updates supplied orientations, applies isotropic order, then anisotropic
! order; if both orders are present the anisotropic one takes precedence. For
! Hierarchy, updates orientations and entity orders and computes
! HierarchicalDOF. Other family codes are rejected in DEBUG mode. This
! routine does not release or explicitly reset cached shape workspaces.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: order, aniso(3)
! LOGICAL(LGT) :: isotropic, anisotropic
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetIsotropicOrder(order=3_I4B)
! CALL OK(obj%GetTotalDOF() == 10_I4B, "Isotropic cubic DOF count")
! CALL obj%ResetIsotropicOrder()
! CALL obj%GetParam(order=order, isIsotropicOrder=isotropic)
! CALL OK(order == 0_I4B .AND. .NOT. isotropic, "Reset isotropic state")
! CALL obj%SetOrder(order=1_I4B)
! CALL OK(obj%GetTotalDOF() == 3_I4B, "SetOrder recomputes DOFs")
! CALL obj%SetTotalDOF(tdof=7_I4B)
! CALL OK(obj%GetTotalDOF() == 7_I4B, "Explicit DOF override")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: cell(3), faces(3, 3), tsize, nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(cellOrder=[3_I4B], tcell=2_I4B, &
!   faceOrder=RESHAPE([3_I4B], [1, 1]), tface=3_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!   faceOrient=RESHAPE([1_I4B], [1, 1]))
! CALL obj%GetCellOrder(ans=cell, tsize=tsize)
! CALL obj%GetFaceOrder(ans=faces, nrow=nrow, ncol=ncol)
! CALL OK(tsize == 2_I4B .AND. ALL(cell(1:tsize) == 3_I4B), "Cell order")
! CALL OK(ncol == 3_I4B .AND. ALL(faces == 3_I4B), "Facet orders")
! CALL OK(obj%GetTotalDOF() == 10_I4B, "Cubic hierarchical triangle DOFs")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetOrder( &
    obj, order, anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, &
    faceOrient, edgeOrient, tcell, tface, tedge, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic polynomial degree, or quadrature accuracy in quadrature
    !! setters.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Directional polynomial degrees; one entry broadcasts over xidim.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Cell polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Face polynomial orders, three rows and one column per active face.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Edge polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! Cell orientation data; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! Face orientations; shape (3, tFace), or one entry for broadcast.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! Edge orientation data; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! Active size of cell order or orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! Active number of face columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetCellOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set cell order data and activity state
!
! Sets the corresponding activity flag from PRESENT(cellOrder). If tCell is
! supplied, updates the shared active order/orientation size for this entity.
! Copies supplied values, broadcasting a one-entry input. With no data, clears
! the activity flag without clearing stored values or active size. Optional
! errCheck enables DEBUG shape and presence checks where applicable. Does not
! recompute tdof or reset shape caches.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: cell(3), face(3, 4), edge(12), nrow, ncol, tsize
! LOGICAL(LGT) :: active
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetCellOrder(cellOrder=[3_I4B, 2_I4B], tCell=2_I4B)
! CALL obj%GetCellOrder(ans=cell, tsize=tsize)
! CALL OK(tsize == 2_I4B, "Active cell orders")
! CALL OK(ALL(cell(1:tsize) == [3_I4B, 2_I4B]), "Cell-order values")
! face = 2_I4B
! CALL obj%SetFaceOrder(faceOrder=face, tFace=4_I4B)
! CALL obj%GetFaceOrder(ans=face, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 4_I4B, "Face-order dimensions")
! CALL OK(ALL(face == 2_I4B), "Face-order values")
! edge = 3_I4B
! CALL obj%SetEdgeOrder(edgeOrder=edge, tEdge=4_I4B)
! CALL obj%GetEdgeOrder(ans=edge, tsize=tsize)
! CALL OK(tsize == 4_I4B .AND. ALL(edge(1:tsize) == 3_I4B), "Edge orders")
! CALL obj%SetEdgeOrder()
! CALL obj%GetParam(isEdgeOrder=active)
! CALL OK(.NOT. active, "Omitted edgeOrder clears the active flag")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetCellOrder(obj, cellOrder, tCell, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Cell polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! Active size of cell order or orientation data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetFaceOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set face order data and activity state
!
! Sets the corresponding activity flag from PRESENT(faceOrder). If tFace is
! supplied, updates the shared active order/orientation size for this entity.
! Copies supplied values, broadcasting a one-entry input. With no data, clears
! the activity flag without clearing stored values or active size. Optional
! errCheck enables DEBUG shape and presence checks where applicable. Does not
! recompute tdof or reset shape caches.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: cell(3), face(3, 4), edge(12), nrow, ncol, tsize
! LOGICAL(LGT) :: active
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetCellOrder(cellOrder=[3_I4B, 2_I4B], tCell=2_I4B)
! CALL obj%GetCellOrder(ans=cell, tsize=tsize)
! CALL OK(tsize == 2_I4B, "Active cell orders")
! CALL OK(ALL(cell(1:tsize) == [3_I4B, 2_I4B]), "Cell-order values")
! face = 2_I4B
! CALL obj%SetFaceOrder(faceOrder=face, tFace=4_I4B)
! CALL obj%GetFaceOrder(ans=face, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 4_I4B, "Face-order dimensions")
! CALL OK(ALL(face == 2_I4B), "Face-order values")
! edge = 3_I4B
! CALL obj%SetEdgeOrder(edgeOrder=edge, tEdge=4_I4B)
! CALL obj%GetEdgeOrder(ans=edge, tsize=tsize)
! CALL OK(tsize == 4_I4B .AND. ALL(edge(1:tsize) == 3_I4B), "Edge orders")
! CALL obj%SetEdgeOrder()
! CALL obj%GetParam(isEdgeOrder=active)
! CALL OK(.NOT. active, "Omitted edgeOrder clears the active flag")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetFaceOrder(obj, faceOrder, tFace, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Face polynomial orders, three rows and one column per active face.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! Active number of face columns.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetFaceOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetEdgeOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set edge order data and activity state
!
! Sets the corresponding activity flag from PRESENT(edgeOrder). If tEdge is
! supplied, updates the shared active order/orientation size for this entity.
! Copies supplied values, broadcasting a one-entry input. With no data, clears
! the activity flag without clearing stored values or active size. Optional
! errCheck enables DEBUG shape and presence checks where applicable. Does not
! recompute tdof or reset shape caches.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: cell(3), face(3, 4), edge(12), nrow, ncol, tsize
! LOGICAL(LGT) :: active
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetCellOrder(cellOrder=[3_I4B, 2_I4B], tCell=2_I4B)
! CALL obj%GetCellOrder(ans=cell, tsize=tsize)
! CALL OK(tsize == 2_I4B, "Active cell orders")
! CALL OK(ALL(cell(1:tsize) == [3_I4B, 2_I4B]), "Cell-order values")
! face = 2_I4B
! CALL obj%SetFaceOrder(faceOrder=face, tFace=4_I4B)
! CALL obj%GetFaceOrder(ans=face, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 4_I4B, "Face-order dimensions")
! CALL OK(ALL(face == 2_I4B), "Face-order values")
! edge = 3_I4B
! CALL obj%SetEdgeOrder(edgeOrder=edge, tEdge=4_I4B)
! CALL obj%GetEdgeOrder(ans=edge, tsize=tsize)
! CALL OK(tsize == 4_I4B .AND. ALL(edge(1:tsize) == 3_I4B), "Edge orders")
! CALL obj%SetEdgeOrder()
! CALL obj%GetParam(isEdgeOrder=active)
! CALL OK(.NOT. active, "Omitted edgeOrder clears the active flag")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetEdgeOrder(obj, edgeOrder, tEdge, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Edge polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetEdgeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetIsotropicOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Set isotropic degree and recompute Lagrange DOFs
!
! Sets the isotropic activity flag from PRESENT(order). When supplied, resets
! anisotropic state, stores order and computes LagrangeDOF for topoType. With
! no order, only clears the isotropic activity flag; the stored order and DOF
! count remain unchanged. errCheck is currently unused.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: order, aniso(3)
! LOGICAL(LGT) :: isotropic, anisotropic
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetIsotropicOrder(order=3_I4B)
! CALL OK(obj%GetTotalDOF() == 10_I4B, "Isotropic cubic DOF count")
! CALL obj%ResetIsotropicOrder()
! CALL obj%GetParam(order=order, isIsotropicOrder=isotropic)
! CALL OK(order == 0_I4B .AND. .NOT. isotropic, "Reset isotropic state")
! CALL obj%SetOrder(order=1_I4B)
! CALL OK(obj%GetTotalDOF() == 3_I4B, "SetOrder recomputes DOFs")
! CALL obj%SetTotalDOF(tdof=7_I4B)
! CALL OK(obj%GetTotalDOF() == 7_I4B, "Explicit DOF override")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetIsotropicOrder(obj, order, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic polynomial degree, or quadrature accuracy in quadrature
    !! setters.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetIsotropicOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                              SetAnisotropicOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Set directional degrees and recompute Lagrange DOFs
!
! Sets the anisotropic activity flag from PRESENT(anisoOrder). When supplied,
! resets isotropic state, copies or broadcasts the first xidim degrees and
! computes LagrangeDOF using the stored directional degrees. With no array,
! only clears the activity flag. errCheck is currently unused.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: orders(3)
! LOGICAL(LGT) :: isAniso, isIso
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetAnisotropicOrder(anisoOrder=[2_I4B, 3_I4B])
! CALL obj%GetParam(anisoOrder=orders, isAnisotropicOrder=isAniso, &
!   isIsotropicOrder=isIso)
! CALL OK(isAniso .AND. .NOT. isIso, "Anisotropic mode")
! CALL OK(ALL(orders(1:2) == [2_I4B, 3_I4B]), "Directional orders")
! CALL OK(obj%GetTotalDOF() == 12_I4B, "Tensor-product DOF count")
! CALL obj%ResetAnisotropicOrder()
! CALL obj%GetParam(anisoOrder=orders, isAnisotropicOrder=isAniso)
! CALL OK(.NOT. isAniso .AND. ALL(orders == 0_I4B), "Reset aniso state")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetAnisotropicOrder(obj, anisoOrder, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Directional polynomial degrees; one entry broadcasts over xidim.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional request for DEBUG validation where implemented; default false.
  END SUBROUTINE obj_SetAnisotropicOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                          Line_SetQuadratureOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set Quadrature Order for a line
!
! Delegates to quadOpt%Line_SetOrder. Changes quadrature accuracy without
! changing approximation orders. Accuracy and point-count interpretation
! follow QuadratureOpt_.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Line_SetQuadratureOrder(order=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Line_SetQuadratureType(quadratureType=types(1))
! CALL obj%Line_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Line_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Line_SetQuadratureOrder(obj, order)
    CLASS(BasisOpt_), INTENT(inout) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(IN) :: order
    !! Quadrature accuracy, or directional accuracy array for a quadrangle.
  END SUBROUTINE Line_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                      Triangle_SetQuadratureOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set Quadrature Order for a triangle
!
! Delegates to quadOpt%Triangle_SetOrder. Changes quadrature accuracy without
! changing approximation orders. Accuracy and point-count interpretation
! follow QuadratureOpt_.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Triangle_SetQuadratureOrder(order=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Triangle_SetQuadratureType(quadratureType=types(1))
! CALL obj%Triangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Triangle_SetQuadratureOrder(obj, order)
    CLASS(BasisOpt_), INTENT(inout) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(IN) :: order
    !! Quadrature accuracy, or directional accuracy array for a quadrangle.
  END SUBROUTINE Triangle_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                    Quadrangle_SetQuadratureOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set Quadrature Order for a quadrangle
!
! Delegates to quadOpt%Quadrangle_SetOrder. Changes quadrature accuracy
! without changing approximation orders. Accuracy and point-count
! interpretation follow QuadratureOpt_. Supports an array or separate
! directional scalar settings.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Quadrangle_SetQuadratureOrder(order=[8_I4B, 8_I4B])
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Quadrangle_SetQuadratureType(quadratureType=types(1:2))
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Quadrangle_SetQuadratureOrder(obj, order, order1, order2)
    CLASS(BasisOpt_), INTENT(inout) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    !! Quadrature accuracy, or directional accuracy array for a quadrangle.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1
    !! Isotropic polynomial degree, or quadrature accuracy in quadrature
    !! setters in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2
    !! Isotropic polynomial degree, or quadrature accuracy in quadrature
    !! setters in direction 2.
  END SUBROUTINE Quadrangle_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                           ResetAnisotropicOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary: Restore default directional degrees and activity flag
!
! Restores anisoOrder and isAnisotropicOrder from TypeBasisOpt. Does not
! recompute tdof or alter isotropic state.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: orders(3)
! LOGICAL(LGT) :: isAniso, isIso
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetAnisotropicOrder(anisoOrder=[2_I4B, 3_I4B])
! CALL obj%GetParam(anisoOrder=orders, isAnisotropicOrder=isAniso, &
!   isIsotropicOrder=isIso)
! CALL OK(isAniso .AND. .NOT. isIso, "Anisotropic mode")
! CALL OK(ALL(orders(1:2) == [2_I4B, 3_I4B]), "Directional orders")
! CALL OK(obj%GetTotalDOF() == 12_I4B, "Tensor-product DOF count")
! CALL obj%ResetAnisotropicOrder()
! CALL obj%GetParam(anisoOrder=orders, isAnisotropicOrder=isAniso)
! CALL OK(.NOT. isAniso .AND. ALL(orders == 0_I4B), "Reset aniso state")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_ResetAnisotropicOrder(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
  END SUBROUTINE obj_ResetAnisotropicOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                           ResetIsotropicOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary: Restore default isotropic degree and activity flag
!
! Restores order and isIsotropicOrder from TypeBasisOpt. Does not recompute
! tdof or alter anisotropic state.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: order, aniso(3)
! LOGICAL(LGT) :: isotropic, anisotropic
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetIsotropicOrder(order=3_I4B)
! CALL OK(obj%GetTotalDOF() == 10_I4B, "Isotropic cubic DOF count")
! CALL obj%ResetIsotropicOrder()
! CALL obj%GetParam(order=order, isIsotropicOrder=isotropic)
! CALL OK(order == 0_I4B .AND. .NOT. isotropic, "Reset isotropic state")
! CALL obj%SetOrder(order=1_I4B)
! CALL OK(obj%GetTotalDOF() == 3_I4B, "SetOrder recomputes DOFs")
! CALL obj%SetTotalDOF(tdof=7_I4B)
! CALL OK(obj%GetTotalDOF() == 7_I4B, "Explicit DOF override")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_ResetIsotropicOrder(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
  END SUBROUTINE obj_ResetIsotropicOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetTotalDOF@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary: Assign the stored DOF count
!
! Stores tdof directly without changing approximation orders, activity flags
! or allocated workspaces.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: order, aniso(3)
! LOGICAL(LGT) :: isotropic, anisotropic
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetIsotropicOrder(order=3_I4B)
! CALL OK(obj%GetTotalDOF() == 10_I4B, "Isotropic cubic DOF count")
! CALL obj%ResetIsotropicOrder()
! CALL obj%GetParam(order=order, isIsotropicOrder=isotropic)
! CALL OK(order == 0_I4B .AND. .NOT. isotropic, "Reset isotropic state")
! CALL obj%SetOrder(order=1_I4B)
! CALL OK(obj%GetTotalDOF() == 3_I4B, "SetOrder recomputes DOFs")
! CALL obj%SetTotalDOF(tdof=7_I4B)
! CALL OK(obj%GetTotalDOF() == 7_I4B, "Explicit DOF override")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetTotalDOF(obj, tdof)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(IN) :: tdof
    !! Total DOF count to store; no other settings are updated.
  END SUBROUTINE obj_SetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                           Line_SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set Quadrature Type for a line
!
! Delegates to quadOpt%Line_SetQuadratureType. Changes the quadrature rule
! type without changing the approximation family.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Line_SetQuadratureOrder(order=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Line_SetQuadratureType(quadratureType=types(1))
! CALL obj%Line_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Line_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Line_SetQuadratureType(obj, quadratureType)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Quadrature rule type codes by reference direction.
  END SUBROUTINE Line_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                       Triangle_SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set Quadrature Type for a triangle
!
! Delegates to quadOpt%Triangle_SetQuadratureType. Changes the quadrature rule
! type without changing the approximation family.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Triangle_SetQuadratureOrder(order=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Triangle_SetQuadratureType(quadratureType=types(1))
! CALL obj%Triangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Triangle_SetQuadratureType(obj, quadratureType)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Quadrature rule type codes by reference direction.
  END SUBROUTINE Triangle_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                     Quadrangle_SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set Quadrature Type for a quadrangle
!
! Delegates to quadOpt%Quadrangle_SetQuadratureType. Changes the quadrature
! rule type without changing the approximation family. Supports an array or
! separate directional scalar settings.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Quadrangle_SetQuadratureOrder(order=[8_I4B, 8_I4B])
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Quadrangle_SetQuadratureType(quadratureType=types(1:2))
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Quadrangle_SetQuadratureType( &
    obj, quadratureType, quadratureType1, quadratureType2)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    !! Quadrature rule type codes by reference direction in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    !! Quadrature rule type codes by reference direction in direction 2.
  END SUBROUTINE Quadrangle_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetTotalFace@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-30
! summary: Return the number of reference facets
!
! Calls GetTotalFaces for the stored topology code.
!
! Returns:
! ans : INTEGER(I4B) - Number of facets for the stored reference topology.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetTotalFace(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B) :: ans
    !! Number of facets for the stored reference topology.
  END FUNCTION obj_GetTotalFace
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTotalVertex@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-30
! summary: Return the number of reference vertices
!
! Calls GetTotalNodes for the stored topology code; this is the reference
! vertex count, not the polynomial interpolation point count.
!
! Returns:
! ans : INTEGER(I4B) - Number of reference vertices.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertex(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B) :: ans
    !! Number of reference vertices.
  END FUNCTION obj_GetTotalVertex
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetNSD@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-30
! summary: Return the physical spatial dimension
!
! Returns the stored nsd. Reference coordinate dimension is a separate field.
!
! Returns:
! ans : INTEGER(I4B) - Physical spatial dimension.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetNSD(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B) :: ans
    !! Physical spatial dimension.
  END FUNCTION obj_GetNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetRefElemCoord@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Copy reference vertex coordinates
!
! Writes three coordinate rows and GetTotalNodes(topoType) vertex columns to
! ans. Inactive output entries are not written. Coordinates are reference
! vertices rather than all polynomial interpolation points.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetRefElemCoord(obj, ans, nrow, ncol)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Coordinate output; SIZE(ans,1) >= 3 and SIZE(ans,2) >= vertex count.
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! Number of active rows written to ans.
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! Number of active columns written to ans.
  END SUBROUTINE obj_GetRefElemCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Read selected stored configuration values
!
! Copies requested scalar fields and fixed-size vectors; copies only the
! active edge, face and cell order ranges. Requests quadrature settings from
! quadOpt%GetParam. Inactive parts of entity-order output arrays are not
! written. Full-vector outputs require three entries, except dofType, which
! requires four.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, nsd, order, anisoOrder, edgeOrder, faceOrder, cellOrder, fetype, &
    elemType, topoType, elemIndx, ipType, basisType, alpha, beta, lambda, &
    dofType, transformType, refElemDomain, baseContinuity, &
    baseInterpolation, isIsotropicOrder, isAnisotropicOrder, isEdgeOrder, &
    isFaceOrder, isCellOrder, tEdgeOrder, tFaceOrder, tCellOrder, &
    quadratureIsHomogeneous, quadratureType, quadratureOrder, &
    quadratureNips, quadratureIsOrder, quadratureIsNips, quadratureAlpha, &
    quadratureBeta, quadratureLambda)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
    !! Number of physical spatial dimensions.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order
    !! Isotropic polynomial degree, or quadrature accuracy in quadrature
    !! setters.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: anisoOrder(3)
    !! Directional approximation degrees; SIZE(anisoOrder) = 3.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: edgeOrder(:)
    !! Edge polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: faceOrder(:, :)
    !! Face polynomial orders, three rows and one column per active face.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: cellOrder(:)
    !! Cell polynomial orders; one entry broadcasts over the active size.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: fetype
    !! Finite element value type code; default scalar.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: topoType
    !! Reference element topology code.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemIndx
    !! Reference element index.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ipType
    !! Interpolation point type; defaults to Equidistance on initialization.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: basisType(:)
    !! Polynomial basis codes; SIZE(basisType) = 3.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha(:)
    !! Jacobi alpha values; SIZE(alpha) = 3.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta(:)
    !! Jacobi beta values; SIZE(beta) = 3.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda(:)
    !! Ultraspherical parameters; SIZE(lambda) = 3.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: dofType(:)
    !! Four DOF classification codes; default [1,1,1,1].
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: transformType
    !! Transformation classification code; default zero.
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseContinuity
    !! Continuity name; Initiate uppercases its first two characters.
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseInterpolation
    !! Family name; Initiate uppercases its first four characters.
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: refElemDomain
    !! Reference domain code; only the first character is stored.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isIsotropicOrder
    !! Whether isotropic approximation order is active.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isAnisotropicOrder
    !! Whether anisotropic approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isEdgeOrder
    !! Whether edge-specific approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isFaceOrder
    !! Whether face-specific approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isCellOrder
    !! Whether cell-specific approximation orders are active.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tEdgeOrder
    !! Number of active edge orders.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tFaceOrder
    !! Number of active face-order columns.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tCellOrder
    !! Number of active cell orders.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsHomogeneous
    !! Whether the same quadrature settings apply in all directions.
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureType(:)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureOrder(:)
    !! Quadrature accuracy orders by reference direction.
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureNips(:)
    !! Quadrature point counts by reference direction.
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureAlpha(:)
    !! Quadrature Jacobi alpha parameters by direction.
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureBeta(:)
    !! Quadrature Jacobi beta parameters by direction.
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureLambda(:)
    !! Quadrature ultraspherical parameters by direction.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsOrder
    !! Whether quadrature is specified by accuracy order.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsNips
    !! Whether quadrature is specified by point count.
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                             Line_GetFacetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Get Facet Quadrature Points for a line
!
! Delegates to quadOpt%Line_GetFacetQuadraturePoints. Populates cell reference
! coordinates restricted to localFaceNumber and the corresponding facet
! reference points. Face numbering follows the reference topology.
!
! The delegated line-facet quadrature routine is currently a WIP placeholder
! and reports an error in DEBUG mode; the example exposes this unsupported
! operation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Line_SetQuadratureOrder(order=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Line_SetQuadratureType(quadratureType=types(1))
! CALL obj%Line_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Line_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Line_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                  localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE Line_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                         Triangle_GetFacetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Get Facet Quadrature Points for a triangle
!
! Delegates to quadOpt%Triangle_GetFacetQuadraturePoints. Populates cell
! reference coordinates restricted to localFaceNumber and the corresponding
! facet reference points. Face numbering follows the reference topology.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Triangle_SetQuadratureOrder(order=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Triangle_SetQuadratureType(quadratureType=types(1))
! CALL obj%Triangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Triangle_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                      localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE Triangle_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                     Quadrangle_GetFacetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Get Facet Quadrature Points for a quadrangle
!
! Delegates to quadOpt%Quadrangle_GetFacetQuadraturePoints. Populates cell
! reference coordinates restricted to localFaceNumber and the corresponding
! facet reference points. Face numbering follows the reference topology.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Quadrangle_SetQuadratureOrder(order=[8_I4B, 8_I4B])
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Quadrangle_SetQuadratureType(quadratureType=types(1:2))
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
 MODULE SUBROUTINE Quadrangle_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                        localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE Quadrangle_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetTotalQuadraturePoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Return the configured cell quadrature point count
!
! Delegates to quadOpt%GetTotalQuadraturePoints without populating a
! QuadraturePoint_ container.
!
! Returns:
! ans : INTEGER(I4B) - Configured cell quadrature point count.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Quadrangle_SetQuadratureOrder(order=[8_I4B, 8_I4B])
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Quadrangle_SetQuadratureType(quadratureType=types(1:2))
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    INTEGER(I4B) :: ans
    !! Configured cell quadrature point count.
  END FUNCTION obj_GetTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetTopologyType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-14
! summary: Return the reference topology code
!
! Returns the stored topoType.
!
! Returns:
! ans : INTEGER(I4B) - Reference topology code.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetTopologyType(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B) :: ans
    !! Reference topology code.
  END FUNCTION obj_GetTopologyType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-14
! summary: Return the stored basis DOF count
!
! Returns tdof. Individual entity setters and low-level SetParam do not
! recompute this value.
!
! Returns:
! ans : INTEGER(I4B) - Stored total basis DOF count.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B) :: ans
    !! Stored total basis DOF count.
  END FUNCTION obj_GetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary: Import basis and quadrature configuration from a TOML table
!
! Resets the object and reads baseInterpolation, baseContinuity, ipType,
! basisType, alpha, beta, lambda, feType and optional elemType. Scalar
! polynomial parameters broadcast to three directions; vector inputs update up
! to three entries. Optional nsd and elemType arguments override the table.
! Reads quadrature through quadOptName (default quadOpt). This path does not
! import approximation orders, orientations or DOF count; set them afterward.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE Test_Method, ONLY: OK
! USE tomlf, ONLY: toml_table
! USE tomlf, ONLY: set_value
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(toml_table) :: table
! REAL(DFP) :: alpha(3)
!
! CALL set_value(table, "baseContinuity", "H1")
! CALL set_value(table, "baseInterpolation", "Lagrange")
! CALL set_value(table, "alpha", 0.25_DFP)
! CALL obj%ImportFromToml(table=table, elemType=TypeElemNameOpt%Triangle, &
!   nsd=2_I4B)
! CALL obj%GetParam(alpha=alpha)
! CALL OK(obj%GetNSD() == 2_I4B, "TOML dimension override")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "TOML basis family")
! CALL OK(ALL(ABS(alpha - 0.25_DFP) < 1.0E-12_DFP), "Scalar expansion")
! CALL obj%SetOrder(order=1_I4B)
! CALL OK(obj%GetTotalDOF() == 3_I4B, "Set approximation order after import")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, elemType, nsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(toml_table), INTENT(INOUT) :: table
    !! TOML table containing basis options.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of physical spatial dimensions.
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-08
! summary: Import a named basis table from a TOML document
!
! Reads the document from filename or afile, selects tomlName and calls the
! table overload. A missing table is diagnosed in DEBUG mode. If printToml is
! present, the selected table is printed in DEBUG mode regardless of its
! logical value.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER :: unitno
!
! OPEN(NEWUNIT=unitno, FILE="basisopt_example.toml", &
!   STATUS="replace", ACTION="write")
! WRITE(unitno, '(A)') '[basis]'
! WRITE(unitno, '(A)') 'baseContinuity = "H1"'
! WRITE(unitno, '(A)') 'baseInterpolation = "Lagrange"'
! CLOSE(unitno)
! CALL obj%ImportFromToml(tomlName="basis", &
!   filename="basisopt_example.toml", &
!   elemType=TypeElemNameOpt%Triangle, nsd=2_I4B)
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "TOML topology")
! CALL OK(obj%GetBaseContinuity() == "H1", "TOML continuity")
! CALL obj%DEALLOCATE()
! OPEN(NEWUNIT=unitno, FILE="basisopt_example.toml", &
!   STATUS="old", ACTION="read")
! CLOSE(unitno, STATUS="delete")
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml, elemType, nsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    CHARACTER(*), INTENT(IN) :: tomlName
    !! Name of the finite element table in the TOML document.
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! Optional text-file handle passed to the TOML reader.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! Optional TOML input filename.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! If present, prints the selected table in DEBUG, even when false.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of physical spatial dimensions.
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetBaseInterpolation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-16
! summary: Return the four-character basis family code
!
! Returns the stored baseInterpolation code, for example LAGR or HIER.
!
! Returns:
! ans : CHARACTER(4) - Four-character interpolation family code.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    CHARACTER(4) :: ans
    !! Four-character interpolation family code.
  END FUNCTION obj_GetBaseInterpolation
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetBaseContinuity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Return the two-character continuity code
!
! Returns the stored baseContinuity code, for example H1.
!
! Returns:
! ans : CHARACTER(2) - Two-character continuity code.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetBaseContinuity(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    CHARACTER(2) :: ans
    !! Two-character continuity code.
  END FUNCTION obj_GetBaseContinuity
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetBaseContinuity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Return the interpolation point type code
!
! Returns the stored ipType.
!
! Returns:
! ans : INTEGER(I4B) - Interpolation point type code.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
! REAL(DFP) :: coords(3, 8), alpha(3)
! CHARACTER(4) :: interpolation
! CHARACTER(2) :: continuity
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
!   ipType=ipType, baseInterpolation=interpolation, baseContinuity=continuity)
! CALL OK(obj%GetTotalVertex() == 3_I4B, "Triangle vertices")
! CALL OK(obj%GetTotalFace() == 3_I4B, "Triangle facets")
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseInterpolation() == "LAGR", "Interpolation code")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference coordinate shape")
! CALL OK(ALL(ABS(coords(3, 1:3)) < 1.0E-12_DFP), "Reference plane")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_GetIpType(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B) :: ans
    !! Interpolation point type code.
  END FUNCTION obj_GetIpType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetCellOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-17
! summary: Copy active cell polynomial orders
!
! Copies the first tCellOrder entries of stored cell orders and returns their
! active size. Caller supplies a large enough output buffer; inactive entries
! remain unchanged.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: cell(3), face(3, 4), edge(12), nrow, ncol, tsize
! LOGICAL(LGT) :: active
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetCellOrder(cellOrder=[3_I4B, 2_I4B], tCell=2_I4B)
! CALL obj%GetCellOrder(ans=cell, tsize=tsize)
! CALL OK(tsize == 2_I4B, "Active cell orders")
! CALL OK(ALL(cell(1:tsize) == [3_I4B, 2_I4B]), "Cell-order values")
! face = 2_I4B
! CALL obj%SetFaceOrder(faceOrder=face, tFace=4_I4B)
! CALL obj%GetFaceOrder(ans=face, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 4_I4B, "Face-order dimensions")
! CALL OK(ALL(face == 2_I4B), "Face-order values")
! edge = 3_I4B
! CALL obj%SetEdgeOrder(edgeOrder=edge, tEdge=4_I4B)
! CALL obj%GetEdgeOrder(ans=edge, tsize=tsize)
! CALL OK(tsize == 4_I4B .AND. ALL(edge(1:tsize) == 3_I4B), "Edge orders")
! CALL obj%SetEdgeOrder()
! CALL obj%GetParam(isEdgeOrder=active)
! CALL OK(.NOT. active, "Omitted edgeOrder clears the active flag")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetCellOrder(obj, ans, tsize)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! Order output; SIZE(ans) must fit the active entity-order count.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of active entries copied into ans.
  END SUBROUTINE obj_GetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetFaceOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-17
! summary: Copy active face polynomial orders
!
! Returns nrow=3 and ncol=tFaceOrder, and copies the active face-order
! columns. Caller supplies a large enough output buffer; inactive entries
! remain unchanged.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: cell(3), face(3, 4), edge(12), nrow, ncol, tsize
! LOGICAL(LGT) :: active
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetCellOrder(cellOrder=[3_I4B, 2_I4B], tCell=2_I4B)
! CALL obj%GetCellOrder(ans=cell, tsize=tsize)
! CALL OK(tsize == 2_I4B, "Active cell orders")
! CALL OK(ALL(cell(1:tsize) == [3_I4B, 2_I4B]), "Cell-order values")
! face = 2_I4B
! CALL obj%SetFaceOrder(faceOrder=face, tFace=4_I4B)
! CALL obj%GetFaceOrder(ans=face, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 4_I4B, "Face-order dimensions")
! CALL OK(ALL(face == 2_I4B), "Face-order values")
! edge = 3_I4B
! CALL obj%SetEdgeOrder(edgeOrder=edge, tEdge=4_I4B)
! CALL obj%GetEdgeOrder(ans=edge, tsize=tsize)
! CALL OK(tsize == 4_I4B .AND. ALL(edge(1:tsize) == 3_I4B), "Edge orders")
! CALL obj%SetEdgeOrder()
! CALL obj%GetParam(isEdgeOrder=active)
! CALL OK(.NOT. active, "Omitted edgeOrder clears the active flag")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetFaceOrder(obj, ans, nrow, ncol)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! Order output; SIZE(ans,1) >= 3, SIZE(ans,2) >= tFaceOrder.
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! Number of active rows written to ans.
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! Number of active columns written to ans.
  END SUBROUTINE obj_GetFaceOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEdgeOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-17
! summary: Copy active edge polynomial orders
!
! Copies the first tEdgeOrder entries of stored edge orders and returns their
! active size. Caller supplies a large enough output buffer; inactive entries
! remain unchanged.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! INTEGER(I4B) :: cell(3), face(3, 4), edge(12), nrow, ncol, tsize
! LOGICAL(LGT) :: active
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetCellOrder(cellOrder=[3_I4B, 2_I4B], tCell=2_I4B)
! CALL obj%GetCellOrder(ans=cell, tsize=tsize)
! CALL OK(tsize == 2_I4B, "Active cell orders")
! CALL OK(ALL(cell(1:tsize) == [3_I4B, 2_I4B]), "Cell-order values")
! face = 2_I4B
! CALL obj%SetFaceOrder(faceOrder=face, tFace=4_I4B)
! CALL obj%GetFaceOrder(ans=face, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 4_I4B, "Face-order dimensions")
! CALL OK(ALL(face == 2_I4B), "Face-order values")
! edge = 3_I4B
! CALL obj%SetEdgeOrder(edgeOrder=edge, tEdge=4_I4B)
! CALL obj%GetEdgeOrder(ans=edge, tsize=tsize)
! CALL OK(tsize == 4_I4B .AND. ALL(edge(1:tsize) == 3_I4B), "Edge orders")
! CALL obj%SetEdgeOrder()
! CALL obj%GetParam(isEdgeOrder=active)
! CALL OK(.NOT. active, "Omitted edgeOrder clears the active flag")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetEdgeOrder(obj, ans, tsize)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    !! Basis options object.
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! Order output; SIZE(ans) must fit the active entity-order count.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of active entries copied into ans.
  END SUBROUTINE obj_GetEdgeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                          Line_GetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get Quadrature Points for a line
!
! Delegates to quadOpt%Line_GetQuadraturePoints. Populates the cell quadrature
! container using the configured rule and accuracy.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Line_SetQuadratureOrder(order=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Line_SetQuadratureType(quadratureType=types(1))
! CALL obj%Line_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Line_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Line_GetQuadraturePoints(obj, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE Line_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                       TriangleGetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get Quadrature Points for a triangle
!
! Delegates to quadOpt%Triangle_GetQuadraturePoints. Populates the cell
! quadrature container using the configured rule and accuracy.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Triangle_SetQuadratureOrder(order=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Triangle_SetQuadratureType(quadratureType=types(1))
! CALL obj%Triangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Triangle_GetQuadraturePoints(obj, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE Triangle_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                     QuadrangleGetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get Quadrature Points for a quadrangle
!
! Delegates to quadOpt%Quadrangle_GetQuadraturePoints. Populates the cell
! quadrature container using the configured rule and accuracy.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%Quadrangle_SetQuadratureOrder(order=[8_I4B, 8_I4B])
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 8_I4B, "Quadrature accuracy")
! CALL obj%Quadrangle_SetQuadratureType(quadratureType=types(1:2))
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count == obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count > 0_I4B, "Nonempty quadrature")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetTotalQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE Quadrangle_GetQuadraturePoints(obj, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE Quadrangle_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                     LineH1LagFE_GetLocalElemShapeData@LineH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-15
! summary: Evaluate local cell shapes for Line H1 Lagrange
!
! Allocates cell shape storage, copies quadrature weights and evaluates
! Lagrange basis values N and reference derivatives dNdXi for the configured
! line. N is stored as (DOF, quadrature point); dNdXi as (DOF, reference
! direction, quadrature point). Physical coordinates and derivatives require a
! subsequent global mapping. Reuses and may resize private working arrays.
! Requires isotropic order and matching interpolation-point and polynomial
! basis settings.
!
! Formula:
! $$ N_{iq} = N_i(\xi_q), \qquad
! (dNdXi)_{ikq} = \frac{\partial N_i}{\partial \xi_k}(\xi_q). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(1, 2)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(1, :) = [0.0_DFP, 1.0_DFP]
! CALL obj%Line_GetQuadraturePoints(quad)
! CALL obj%LineH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%LineH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%LineH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:1, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE LineH1LagFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE LineH1LagFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                       Line_GetLocalFacetElemShapeData@LineH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Evaluate local facet shapes for Line H1 Lagrange
!
! Unimplemented point-facet operation. Reports a WIP error in DEBUG mode and
! does not populate outputs. The example intentionally exposes this
! unsupported operation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(ElemShapeData_) :: shapes, facets
! TYPE(QuadraturePoint_) :: quad, facetQuad
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%LineH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Point-facet shape data populated")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[LineH1LagFE_GetLocalElemShapeData]]

INTERFACE
  MODULE SUBROUTINE LineH1LagFE_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE LineH1LagFE_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                    LineH1LagFE_GetGlobalElemShapeData@LineH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Map cell shapes to physical coordinates for Line H1 Lagrange
!
! Maps prepared solution shape data using xij and required geometry basis
! values and derivatives from geoelemsd. Populates physical coordinates,
! Jacobian data and physical derivatives through ElemshapeData_Method%Set.
! Solution and geometry datasets must use matching quadrature points. When
! geometry and solution have different DOF counts, eligible higher-order
! interior blocks are reversed according to the configured orientations.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(1, 2)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(1, :) = [0.0_DFP, 1.0_DFP]
! CALL obj%Line_GetQuadraturePoints(quad)
! CALL obj%LineH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%LineH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%LineH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:1, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE LineH1LagFE_GetGlobalElemShapeData(obj, xij, elemsd, &
                                                       geoelemsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required geometry cell shapes at the same points and reference domain.
  END SUBROUTINE LineH1LagFE_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!            TriangleH1LagFE_GetLocalElemShapeData@TriangleH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-15
! summary: Evaluate local cell shapes for Triangle H1 Lagrange
!
! Allocates cell shape storage, copies quadrature weights and evaluates
! Lagrange basis values N and reference derivatives dNdXi for the configured
! triangle. N is stored as (DOF, quadrature point); dNdXi as (DOF, reference
! direction, quadrature point). Physical coordinates and derivatives require a
! subsequent global mapping. Reuses and may resize private working arrays.
! Requires isotropic order and matching interpolation-point and polynomial
! basis settings.
!
! Formula:
! $$ N_{iq} = N_i(\xi_q), \qquad
! (dNdXi)_{ikq} = \frac{\partial N_i}{\partial \xi_k}(\xi_q). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%TriangleH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1LagFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE TriangleH1LagFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE TriangleH1LagFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!        TriangleH1LagFE_GetLocalFacetElemShapeData@TriangleH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Evaluate local facet shapes for Triangle H1 Lagrange
!
! Evaluates the cell basis at quad, allocates facet shape data and evaluates
! the Lagrange line basis and derivatives at facetQuad. Both quadrature inputs
! must describe the same selected face. The cell output contains traces; the
! facet output uses facet coordinates.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%TriangleH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1LagFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[TriangleH1LagFE_GetLocalElemShapeData]]

INTERFACE
  MODULE SUBROUTINE TriangleH1LagFE_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE TriangleH1LagFE_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!            TriangleH1LagFE_GetGlobalElemShapeData@TriangleH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Map cell shapes to physical coordinates for Triangle H1 Lagrange
!
! Maps prepared solution shape data using xij and required geometry basis
! values and derivatives from geoelemsd. Populates physical coordinates,
! Jacobian data and physical derivatives through ElemshapeData_Method%Set.
! Solution and geometry datasets must use matching quadrature points. When
! geometry and solution have different DOF counts, eligible higher-order
! interior blocks are reversed according to the configured orientations.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%TriangleH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1LagFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE TriangleH1LagFE_GetGlobalElemShapeData(obj, xij, elemsd, &
                                                           geoelemsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required geometry cell shapes at the same points and reference domain.
  END SUBROUTINE TriangleH1LagFE_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                     TriangleH1LagFE_GetGlobalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Map facet shapes and compute normals for Triangle H1 Lagrange
!
! Maps cell traces, extracts the two geometry endpoints of the selected facet,
! maps the facet data and computes facet normals. Requires linear two-node
! facet geometry and matching quadrature datasets.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%TriangleH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1LagFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE TriangleH1LagFE_GetGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd
    !! Required geometry cell shapes at the same points and reference domain.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoFacetElemsd
    !! Required prepared geometry facet data; two geometry nodes.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE TriangleH1LagFE_GetGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!         QuadrangleH1LagFE_GetLocalElemShapeData@QuadrangleH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Evaluate local cell shapes for Quadrangle H1 Lagrange
!
! Allocates cell shape storage, copies quadrature weights and evaluates
! Lagrange basis values N and reference derivatives dNdXi for the configured
! quadrangle. N is stored as (DOF, quadrature point); dNdXi as (DOF, reference
! direction, quadrature point). Physical coordinates and derivatives require a
! subsequent global mapping. Reuses and may resize private working arrays.
! Requires isotropic order and matching interpolation-point and polynomial
! basis settings.
!
! Formula:
! $$ N_{iq} = N_i(\xi_q), \qquad
! (dNdXi)_{ikq} = \frac{\partial N_i}{\partial \xi_k}(\xi_q). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 4)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [1.0_DFP, 1.0_DFP]
! xij(:, 4) = [0.0_DFP, 1.0_DFP]
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! CALL obj%QuadrangleH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%QuadrangleH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%QuadrangleH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%QuadrangleH1LagFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE QuadrangleH1LagFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE QuadrangleH1LagFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!    QuadrangleH1LagFE_GetLocalFacetElemShapeData@QuadrangleH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Evaluate local facet shapes for Quadrangle H1 Lagrange
!
! Evaluates the cell basis at quad, allocates facet shape data and evaluates
! the Lagrange line basis and derivatives at facetQuad. Both quadrature inputs
! must describe the same selected face. The cell output contains traces; the
! facet output uses facet coordinates.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 4)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [1.0_DFP, 1.0_DFP]
! xij(:, 4) = [0.0_DFP, 1.0_DFP]
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! CALL obj%QuadrangleH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%QuadrangleH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%QuadrangleH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%QuadrangleH1LagFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[QuadrangleH1LagFE_GetLocalElemShapeData]]

INTERFACE
  MODULE SUBROUTINE QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE QuadrangleH1LagFE_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!        QuadrangleH1LagFE_GetGlobalElemShapeData@QuadrangleH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Map cell shapes to physical coordinates for Quadrangle H1 Lagrange
!
! Maps prepared solution shape data using xij and required geometry basis
! values and derivatives from geoelemsd. Populates physical coordinates,
! Jacobian data and physical derivatives through ElemshapeData_Method%Set.
! Solution and geometry datasets must use matching quadrature points. When
! geometry and solution have different DOF counts, eligible higher-order
! interior blocks are reversed according to the configured orientations.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 4)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [1.0_DFP, 1.0_DFP]
! xij(:, 4) = [0.0_DFP, 1.0_DFP]
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! CALL obj%QuadrangleH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%QuadrangleH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%QuadrangleH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%QuadrangleH1LagFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
MODULE SUBROUTINE QuadrangleH1LagFE_GetGlobalElemShapeData(obj, xij, elemsd, &
                                                             geoelemsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required geometry cell shapes at the same points and reference domain.
  END SUBROUTINE QuadrangleH1LagFE_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                     QuadrangleH1LagFE_GetGlobalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Map facet shapes and compute normals for Quadrangle H1 Lagrange
!
! Maps cell traces, extracts the two geometry endpoints of the selected facet,
! maps the facet data and computes facet normals. Requires linear two-node
! facet geometry and matching quadrature datasets.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 4)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [1.0_DFP, 1.0_DFP]
! xij(:, 4) = [0.0_DFP, 1.0_DFP]
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! CALL obj%QuadrangleH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%QuadrangleH1LagFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%QuadrangleH1LagFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%QuadrangleH1LagFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE QuadrangleH1LagFE_GetGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd
    !! Required geometry cell shapes at the same points and reference domain.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoFacetElemsd
    !! Required prepared geometry facet data; two geometry nodes.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE QuadrangleH1LagFE_GetGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                     LineH1HieFE_GetLocalElemShapeData@LineH1HierangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-15
! summary: Evaluate local cell shapes for Line H1 hierarchical
!
! Allocates cell shape storage, copies quadrature weights and evaluates
! hierarchical basis values N and reference derivatives dNdXi for the
! configured line. N is stored as (DOF, quadrature point); dNdXi as (DOF,
! reference direction, quadrature point). Physical coordinates and derivatives
! require a subsequent global mapping. Reuses and may resize private working
! arrays. Uses the configured cell and face orders and orientations.
!
! Formula:
! $$ N_{iq} = N_i(\xi_q), \qquad
! (dNdXi)_{ikq} = \frac{\partial N_i}{\partial \xi_k}(\xi_q). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(1, 2)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=1_I4B, &
!   cellOrient=[1_I4B], &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=1_I4B, &
!   cellOrient=[1_I4B], &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(1, :) = [0.0_DFP, 1.0_DFP]
! CALL obj%Line_GetQuadraturePoints(quad)
! CALL obj%LineH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%LineH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:2, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%LineH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:1, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE LineH1HieFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE LineH1HieFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                       Line_GetLocalFacetElemShapeData@LineH1HierangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Evaluate local facet shapes for Line H1 hierarchical
!
! Unimplemented point-facet operation. Reports a WIP error in DEBUG mode and
! does not populate outputs. The example intentionally exposes this
! unsupported operation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(ElemShapeData_) :: shapes, facets
! TYPE(QuadraturePoint_) :: quad, facetQuad
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=1_I4B, &
!   cellOrient=[1_I4B], &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%LineH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Point-facet shape data populated")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[LineH1HieFE_GetLocalElemShapeData]]

INTERFACE
  MODULE SUBROUTINE LineH1HieFE_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE LineH1HieFE_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                    LineH1HieFE_GetGlobalElemShapeData@LineH1HierangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Map cell shapes to physical coordinates for Line H1 hierarchical
!
! Maps prepared solution shape data using xij and required geometry basis
! values and derivatives from geoelemsd. Populates physical coordinates,
! Jacobian data and physical derivatives through ElemshapeData_Method%Set.
! Solution and geometry datasets must use matching quadrature points.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(1, 2)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=1_I4B, &
!   cellOrient=[1_I4B], &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=1_I4B, &
!   cellOrient=[1_I4B], &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(1, :) = [0.0_DFP, 1.0_DFP]
! CALL obj%Line_GetQuadraturePoints(quad)
! CALL obj%LineH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%LineH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:2, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%LineH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:1, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE LineH1HieFE_GetGlobalElemShapeData(obj, xij, elemsd, &
                                                       geoelemsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required geometry cell shapes at the same points and reference domain.
  END SUBROUTINE LineH1HieFE_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!         TriangleH1HieFE_GetLocalElemShapeData@TriangleH1HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Evaluate local cell shapes for Triangle H1 hierarchical
!
! Allocates cell shape storage, copies quadrature weights and evaluates
! hierarchical basis values N and reference derivatives dNdXi for the
! configured triangle. N is stored as (DOF, quadrature point); dNdXi as (DOF,
! reference direction, quadrature point). Physical coordinates and derivatives
! require a subsequent global mapping. Reuses and may resize private working
! arrays. Uses the configured cell and face orders and orientations.
!
! Formula:
! $$ N_{iq} = N_i(\xi_q), \qquad
! (dNdXi)_{ikq} = \frac{\partial N_i}{\partial \xi_k}(\xi_q). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%TriangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE TriangleH1HieFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE TriangleH1HieFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!    TriangleH1HieFE_GetLocalFacetElemShapeData@TriangleH1HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Evaluate local facet shapes for Triangle H1 hierarchical
!
! Evaluates the cell basis at quad, allocates facet shape data and evaluates
! the hierarchical line basis and derivatives at facetQuad. Both quadrature
! inputs must describe the same selected face. The cell output contains
! traces; the facet output uses facet coordinates. The facet evaluation
! currently does not pass an explicit orientation to the line basis utility.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%TriangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[TriangleH1HieFE_GetLocalElemShapeData]]

INTERFACE
  MODULE SUBROUTINE TriangleH1HieFE_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE TriangleH1HieFE_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!   TriangleH1HieFE_GetGlobalElemShapeData@TriangleH1HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Map cell shapes to physical coordinates for Triangle H1
!   hierarchical
!
! Maps prepared solution shape data using xij and required geometry basis
! values and derivatives from geoelemsd. Populates physical coordinates,
! Jacobian data and physical derivatives through ElemshapeData_Method%Set.
! Solution and geometry datasets must use matching quadrature points.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%TriangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE TriangleH1HieFE_GetGlobalElemShapeData(obj, xij, elemsd, &
                                                           geoelemsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required geometry cell shapes at the same points and reference domain.
  END SUBROUTINE TriangleH1HieFE_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                      TriangleH1HieFE_GetGlobalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Map facet shapes and compute normals for Triangle H1 hierarchical
!
! Maps cell traces, extracts the two geometry endpoints of the selected facet,
! maps the facet data and computes facet normals. Requires linear two-node
! facet geometry and matching quadrature datasets.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%Triangle_GetQuadraturePoints(quad)
! CALL obj%TriangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%TriangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%TriangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Triangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%TriangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%TriangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE TriangleH1HieFE_GetGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd
    !! Required geometry cell shapes at the same points and reference domain.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoFacetElemsd
    !! Required prepared geometry facet data; two geometry nodes.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE TriangleH1HieFE_GetGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!     QuadrangleH1HieFE_GetLocalElemShapeData@QuadrangleH1HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Evaluate local cell shapes for Quadrangle H1 hierarchical
!
! Allocates cell shape storage, copies quadrature weights and evaluates
! hierarchical basis values N and reference derivatives dNdXi for the
! configured quadrangle. N is stored as (DOF, quadrature point); dNdXi as
! (DOF, reference direction, quadrature point). Physical coordinates and
! derivatives require a subsequent global mapping. Reuses and may resize
! private working arrays. Uses the configured cell and face orders and
! orientations.
!
! Formula:
! $$ N_{iq} = N_i(\xi_q), \qquad
! (dNdXi)_{ikq} = \frac{\partial N_i}{\partial \xi_k}(\xi_q). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 4)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=4_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=4_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [1.0_DFP, 1.0_DFP]
! xij(:, 4) = [0.0_DFP, 1.0_DFP]
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! CALL obj%QuadrangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%QuadrangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:4, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%QuadrangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%QuadrangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE QuadrangleH1HieFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
  END SUBROUTINE QuadrangleH1HieFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!QuadrangleH1HieFE_GetLocalFacetElemShapeData@QuadrangleH1HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary: Evaluate local facet shapes for Quadrangle H1 hierarchical
!
! Evaluates the cell basis at quad, allocates facet shape data and evaluates
! the hierarchical line basis and derivatives at facetQuad. Both quadrature
! inputs must describe the same selected face. The cell output contains
! traces; the facet output uses facet coordinates.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 4)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=4_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=4_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [1.0_DFP, 1.0_DFP]
! xij(:, 4) = [0.0_DFP, 1.0_DFP]
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! CALL obj%QuadrangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%QuadrangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:4, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%QuadrangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%QuadrangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[QuadrangleH1HieFE_GetLocalElemShapeData]]

INTERFACE
  MODULE SUBROUTINE QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Prepared cell quadrature, or cell reference points on a facet.
    TYPE(QuadraturePoint_), INTENT(IN) :: facetQuad
    !! Prepared facet quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE QuadrangleH1HieFE_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!   QuadrangleH1HieFE_GetGlobalElemShapeData@QuadrangleH1HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Map cell shapes to physical coordinates for Quadrangle H1
!   hierarchical
!
! Maps prepared solution shape data using xij and required geometry basis
! values and derivatives from geoelemsd. Populates physical coordinates,
! Jacobian data and physical derivatives through ElemshapeData_Method%Set.
! Solution and geometry datasets must use matching quadrature points.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 4)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=4_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=4_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [1.0_DFP, 1.0_DFP]
! xij(:, 4) = [0.0_DFP, 1.0_DFP]
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! CALL obj%QuadrangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%QuadrangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:4, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%QuadrangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%QuadrangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
MODULE SUBROUTINE QuadrangleH1HieFE_GetGlobalElemShapeData(obj, xij, elemsd, &
                                                             geoelemsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required geometry cell shapes at the same points and reference domain.
  END SUBROUTINE QuadrangleH1HieFE_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                   QuadrangleH1HieFE_GetGlobalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Map facet shapes and compute normals for Quadrangle H1 hierarchical
!
! Maps cell traces, extracts the two geometry endpoints of the selected facet,
! maps the facet data and computes facet normals. Requires linear two-node
! facet geometry and matching quadrature datasets.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BasisOpt_Class, ONLY: BasisOpt_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(BasisOpt_) :: obj
! TYPE(BasisOpt_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 4)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=4_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchy", &
!   cellOrder=[1_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], faceOrder=RESHAPE([1_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=4_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [1.0_DFP, 1.0_DFP]
! xij(:, 4) = [0.0_DFP, 1.0_DFP]
! CALL obj%Quadrangle_GetQuadraturePoints(quad)
! CALL obj%QuadrangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%QuadrangleH1HieFE_GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns == obj%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips > 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:4, 1:shapes%nips), &
!   DIM=1) - 1.0_DFP) < 1.0E-10_DFP), "Vertex partition of unity")
! CALL obj%QuadrangleH1HieFE_GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) >= -1.0E-10_DFP), &
!   "Physical coordinates inside the unit cell")
! CALL obj%Quadrangle_GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns == 3_I4B, "Quadratic facet basis size")
! CALL obj%QuadrangleH1HieFE_GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips > 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE QuadrangleH1HieFE_GetGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Basis options object.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces evaluated on a facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution facet shape data.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd
    !! Required geometry cell shapes at the same points and reference domain.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoFacetElemsd
    !! Required prepared geometry facet data; two geometry nodes.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry nodes; SIZE(xij,1) >= nsd and columns fit
    !! geoelemsd%nns.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE QuadrangleH1HieFE_GetGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BasisOpt_Class
