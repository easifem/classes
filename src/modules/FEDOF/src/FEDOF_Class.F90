
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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
! date: 2024-05-14
! summary: Finite element degrees of freedom
!
! This module defines FEDOF_, the degrees of freedom of a finite element space
! on a domain, FEDOFPointer_, and FEDOFSetSparsity.

MODULE FEDOF_Class
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractFE_Class, ONLY: AbstractFEPointer_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP, I4B, INT8, LGT
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE

PRIVATE

PUBLIC :: FEDOF_
PUBLIC :: FEDOFPointer_
PUBLIC :: FEDOFSetSparsity

CHARACTER(*), PARAMETER :: DEFAULT_BASETYPE = "Monomial"
CHARACTER(*), PARAMETER :: DEFAULT_IPTYPE = "Equidistance"
REAL(DFP), PARAMETER :: DEFAULT_ALPHA = 0.0_DFP
REAL(DFP), PARAMETER :: DEFAULT_BETA = 0.0_DFP
REAL(DFP), PARAMETER :: DEFAULT_LAMBDA = 0.5_DFP

!----------------------------------------------------------------------------
!                                                                   FEDOF_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Finite element degrees of freedom
!
! FEDOF_ numbers the degrees of freedom of a finite element space on the mesh
! of dimension nsd of a domain, and holds one finite element for each element
! topology of the mesh.
!
! - Degrees of freedom: first one per vertex node (the local node number,
!   tNodes in total), then those of the edges, faces and cells. The degrees of
!   freedom of edge iedge are edgeIA(iedge) to edgeIA(iedge + 1) - 1; faceIA
!   and cellIA do the same for faces and cells. tdof is the total number.
! - Orders: cellOrder(iel) is the order of local element iel; the order of a
!   face or an edge is the minimum order of the cells around it.
! - fe(ii) is the finite element of element topology ii (FEFactory); SetFE
!   sets its orders and orientations for one element.
! - baseContinuity (H1, HCurl, HDiv, DG) and baseInterpolation (LAGR, HIER,
!   ...) are stored in upper case.
!
! See also: [[obj_Initiate1]], [[obj_GetConnectivity]]

TYPE :: FEDOF_
  PRIVATE
  LOGICAL(LGT) :: isLagrange = math%no
  !! It is true when baseInterpolation is Lagrange
  LOGICAL(LGT) :: isinit = math%no
  !! It is set to true when FEDOF is initiated
  LOGICAL(LGT) :: isMaxConSet = math%no
  !! It is set to true when maxCon is set in GetMaxTotalConnectivity
  LOGICAL(LGT) :: isMaxQuadPointSet = math%no
  !! It is set to true when maxQuadPoint is set in
  !! GetMaxTotalQuadraturePoints
  LOGICAL(LGT) :: isSumElemTotalDOFSet = math%no
  !! It is set to true when we set sumElemTotalDOF, Lazy computing
  LOGICAL(LGT) :: isTotalActiveCellsSet = math%no
  !! It is set tot true when we set tActiveCells, Lazy computing in
  !! GetTotalActiveCells method.
  INTEGER(I4B) :: tdof = 0
  !! Total number of degrees of freedom
  INTEGER(I4B) :: tNodes = 0
  !! Total number of nodes
  INTEGER(I4B) :: tEdges = 0
  !! Total number of edges
  INTEGER(I4B) :: tFaces = 0
  !! Total number of faces
  INTEGER(I4B) :: tCells = 0
  !! Total number of cells
  INTEGER(I4B) :: maxCon = 0
  !! maximum number of connectivity
  INTEGER(I4B) :: sumElemTotalDOF = 0
  !! sum of total  degrees of freedom for each element.
  !! this is useful for DGM
  !! This is cached when we initiate the FEDOF
  INTEGER(I4B) :: tActiveCells = 0
  !! total number of active cells in FEDOF and mesh.
  INTEGER(I4B) :: maxQuadPoint = 0
  !! maximum number of quadrature points
  CHARACTER(2) :: baseContinuity = "H1"
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG
  CHARACTER(4) :: baseInterpolation = "LAGR"
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LAGR: LagrangeInterpolation
  !! HIER: HierarchyInterpolation
  !! ORTHO: OrthogonalInterpolation
  !! HERM: HermitInterpolation
  !! SERE: SerendipityInterpolation
  INTEGER(INT8) :: scaleForQuadOrder = 2_INT8
  !! Scale for quadrature order
  !! Quadrature order = element order * scaleForQuadOrder
  !! This is used for constructing the quadrature points
  INTEGER(INT8) :: maxCellOrder = 0_INT8
  !! maximum value of cell order
  INTEGER(INT8) :: maxFaceOrder = 0_INT8
  !! maximum value of face order
  INTEGER(INT8) :: maxEdgeOrder = 0_INT8
  !! maximum value of edge order
  INTEGER(INT8), ALLOCATABLE :: cellOrder(:)
  !! Order of each cell
  !! the size of cellOrder is equal to the obj%tCells
  !! Get connectivity of an element
  !! Get entity number of an element
  !! Get the cell number of an element (this is global element number)
  !! convert it to the local element number
  !! use this local element number to get cell order from cellOrder
  INTEGER(INT8), ALLOCATABLE :: faceOrder(:)
  !! order of each face
  INTEGER(INT8), ALLOCATABLE :: edgeOrder(:)
  !! order of each edge
  INTEGER(I4B), ALLOCATABLE :: edgeIA(:)
  !! sparsity for edge, the size of edgeJA is equal to the total number of
  !! degrees of freedom on edge,
  !! the size of edgeIA is equal to the total number of edges + 1
  !! The degrees of freedom of iedge is stored in
  !! edgeJA(edgeIA(iedge):edgeIA(iedge+1)-1)
  INTEGER(I4B), ALLOCATABLE :: faceIA(:)
  !! sparsity for face, the size of faceJA is equal to the total number of
  !! degrees of freedom on face,
  !! the size of faceIA is equal to the total number of faces + 1
  !! The degrees of freedom of iface is stored in
  !! faceJA(faceIA(iface):faceIA(iface+1)-1)
  INTEGER(I4B), ALLOCATABLE :: cellIA(:)
  !! sparsity for cell, the size of cellJA is equal to the total number of
  !! degrees of freedom on cell,
  !! the size of cellIA is equal to the total number of cells + 1
  !! The degrees of freedom of icell is stored in
  !! cellJA(cellIA(icell):cellIA(icell+1)-1)
  TYPE(AbstractFEPointer_) :: fe(8)
  !! pointer to finite element object
  !! point, line, triangle, quadrangle, tetrahedron, hexahedron, prism,
  !! pyramid
  CLASS(AbstractMesh_), POINTER :: mesh => NULL()
  !! Pointer to domain
  CLASS(AbstractDomain_), POINTER :: dom => NULL()

CONTAINS
  !@ConstructorMethods
  PROCEDURE, PRIVATE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate a FEDOF with the same order on all cells
  PROCEDURE, PRIVATE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate a FEDOF with an order for each cell
  PROCEDURE, PRIVATE, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate a FEDOF from a table of element numbers and orders
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate4
  !! Initiate the FEDOF
  PROCEDURE, PRIVATE, PASS(obj) :: AllocateSizes => obj_AllocateSizes
  !! Allocate the order and degree of freedom arrays
  PROCEDURE, PRIVATE, PASS(obj) :: SetOrdersFromCellOrder => &
    obj_SetOrdersFromCellOrder
  !! Number the degrees of freedom
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy a FEDOF
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Copy a FEDOF (obj = obj2)
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the FEDOF
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Return true if the FEDOF is initiated

  !@SetMethods
  PROCEDURE, PRIVATE, PASS(obj) :: SetCellOrder => obj_SetCellOrder
  !! Set the order of the cells
  PROCEDURE, PRIVATE, PASS(obj) :: SetFaceOrder => obj_SetFaceOrder
  !! Set the order of the faces
  PROCEDURE, PRIVATE, PASS(obj) :: SetEdgeOrder => obj_SetEdgeOrder
  !! Set the order of the edges
  PROCEDURE, PUBLIC, PASS(obj) :: SetNodeCoord => obj_SetNodeCoord
  !! Set the node coordinates of the finite element
  PROCEDURE, PUBLIC, PASS(obj) :: SetFE => obj_SetFE
  !! Set the finite element of an element

  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the FEDOF
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayCellOrder => &
    obj_DisplayCellOrder
  !! Display the order of the cells
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Initiate the FEDOF from a toml table
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Initiate the FEDOF from a toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Initiate the FEDOF from a toml table or file

  !@GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCells => obj_GetTotalCells
  !! Get total number of cells
  PROCEDURE, PUBLIC, PASS(obj) :: GetCaseName => obj_GetCaseName
  !! Get the name of the basis case
  PROCEDURE, PUBLIC, PASS(obj) :: GetVertexDOF => obj_GetVertexDOF
  !! Get the degree of freedom of a vertex node
  PROCEDURE, PRIVATE, PASS(obj) :: GetEdgeDOF1 => obj_GetEdgeDOF1
  !! Get the degrees of freedom on an edge
  PROCEDURE, PRIVATE, PASS(obj) :: GetEdgeDOF2 => obj_GetEdgeDOF2
  !! Get the degrees of freedom on an edge of an element
  GENERIC, PUBLIC :: GetEdgeDOF => GetEdgeDOF1, GetEdgeDOF2
  !! Get the degrees of freedom on an edge
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalEdgeDOF1 => obj_GetTotalEdgeDOF1
  !! Get the number of degrees of freedom on an edge
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalEdgeDOF2 => obj_GetTotalEdgeDOF2
  !! Get the number of degrees of freedom on an edge of an element
  GENERIC, PUBLIC :: GetTotalEdgeDOF => GetTotalEdgeDOF1, GetTotalEdgeDOF2
  !! Get the number of degrees of freedom on an edge
  PROCEDURE, PRIVATE, PASS(obj) :: GetFaceDOF1 => obj_GetFaceDOF1
  !! Get the degrees of freedom on a face
  PROCEDURE, PRIVATE, PASS(obj) :: GetFaceDOF2 => obj_GetFaceDOF2
  !! Get the degrees of freedom on a face of an element
  GENERIC, PUBLIC :: GetFaceDOF => GetFaceDOF1, GetFaceDOF2
  !! Get the degrees of freedom on a face
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalFaceDOF1 => obj_GetTotalFaceDOF1
  !! Get the number of degrees of freedom on a face
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalFaceDOF2 => obj_GetTotalFaceDOF2
  !! Get the number of degrees of freedom on a face of an element
  GENERIC, PUBLIC :: GetTotalFaceDOF => GetTotalFaceDOF1, GetTotalFaceDOF2
  !! Get the number of degrees of freedom on a face
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellDOF => obj_GetCellDOF
  !! Get the degrees of freedom inside a cell
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCellDOF => obj_GetTotalCellDOF
  !! Get the number of degrees of freedom of a cell
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalVertexDOF => obj_GetTotalVertexDOF
  !! Get the number of vertex degrees of freedom
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalDOF1 => obj_GetTotalDOF1
  !! Get the total number of degrees of freedom
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalDOF2 => obj_GetTotalDOF2
  !! Get the number of degrees of freedom of an element
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalDOF3 => obj_GetTotalDOF3
  !! Get the number of degrees of freedom of an element for an entity
  GENERIC, PUBLIC :: GetTotalDOF => GetTotalDOF1, GetTotalDOF2, GetTotalDOF3
  !! Get the number of degrees of freedom
  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => obj_GetConnectivity
  !! Get the degrees of freedom of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity_ => obj_GetConnectivity_
  !! Get the degrees of freedom of an element without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetConnectivity_ => &
    obj_GetFacetConnectivity_
  !! Get the degrees of freedom of a facet of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshPointer => obj_GetMeshPointer
  !! Get a pointer to the mesh of the FEDOF
  PROCEDURE, PUBLIC, PASS(obj) :: GetDomainPointer => obj_GetDomainPointer
  !! Get a pointer to the domain of the FEDOF
  PROCEDURE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Get the base interpolation
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellOrder => obj_GetCellOrder
  !! Get the order of the cell of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetFaceOrder => obj_GetFaceOrder
  !! Get the order of the faces of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeOrder => obj_GetEdgeOrder
  !! Get the order of the edges of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetOrders => obj_GetOrders
  !! Get the cell, face and edge orders and orientations of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxTotalConnectivity => &
    obj_GetMaxTotalConnectivity
  !! Get the largest number of degrees of freedom of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxTotalQuadraturePoints => &
    obj_GetMaxTotalQuadraturePoints
  !! Get the number of quadrature points of the elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEPointer => obj_GetFEPointer
  !! Get the finite element of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetSumElemTotalDOF => obj_GetSumElemTotalDOF
  !! Sum of the total DOFs of all active elements (cached)
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalActiveCells => &
    obj_GetTotalActiveCells
  !! Returns the total number of active cells in the mesh.

  !@SetSparsityMethods
  PROCEDURE, PRIVATE, PASS(obj) :: SetSparsity1 => obj_SetSparsity1
  !! Set the sparsity pattern of a matrix
  PROCEDURE, PRIVATE, PASS(obj) :: SetSparsity2 => obj_SetSparsity2
  !! Set the sparsity pattern between two FEDOF
  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2
  !! Set the sparsity pattern of a matrix
END TYPE FEDOF_

!----------------------------------------------------------------------------
!                                                             FEDOFPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Pointer to a FEDOF_
!
! ptr points to a FEDOF_.

TYPE :: FEDOFPointer_
  TYPE(FEDOF_), POINTER :: ptr => NULL()
END TYPE FEDOFPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate a geometric FEDOF
!
! Initiate the FEDOF with the order of each element given by the mesh
! (GetOrder_ of the mesh), so that the basis matches the geometry of the
! elements. The routine has no type-bound binding yet, so it cannot be called.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   dom : CLASS(AbstractDomain_), intent(in) - domain; the FEDOF uses its mesh
!     of dimension nsd
!   baseContinuity : CHARACTER(*), intent(in) - continuity of the basis: H1,
!     HCurl, HDiv or DG (the first two letters are used)
!   baseInterpolation : CHARACTER(*), intent(in) - interpolation of the basis:
!     Lagrange, Hierarchical, Orthogonal, Hermit or Serendipity (the first
!     four letters are used)
!   feType : INTEGER(I4B), optional, intent(in) - finite element type: scalar,
!     vector or tensor
!   ipType : INTEGER(I4B), optional, intent(in) - interpolation point type;
!     needed for Lagrange basis
!   basisType(:) : INTEGER(I4B), optional, intent(in) - basis used to build
!     Lagrange polynomials; for Lagrange basis
!   alpha(:) : REAL(DFP), optional, intent(in) - Jacobi alpha parameter; for
!     Lagrange basis built on Jacobi polynomials
!   beta(:) : REAL(DFP), optional, intent(in) - Jacobi beta parameter; for
!     Lagrange basis built on Jacobi polynomials
!   lambda(:) : REAL(DFP), optional, intent(in) - Ultraspherical lambda
!     parameter; for Lagrange basis built on Ultraspherical polynomials
!   dofType(:) : INTEGER(I4B), optional, intent(in) - degree of freedom type
!     (default nodal); size 4
!   transformType : INTEGER(I4B), optional, intent(in) - transformation from
!     the reference to the physical element
!   quadratureIsHomogeneous : LOGICAL(LGT), optional, intent(in) - if true,
!     the same quadrature is used in all directions
!   quadratureType(:) : INTEGER(I4B), optional, intent(in) - quadrature type;
!     see QuadratureOpt_; size 3 (x, y, z)
!   quadratureOrder(:) : INTEGER(I4B), optional, intent(in) - quadrature
!     accuracy; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureIsOrder : LOGICAL(LGT), optional, intent(in) - if true,
!     quadratureOrder is used
!   quadratureNips(:) : INTEGER(I4B), optional, intent(in) - number of
!     quadrature points; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureIsNips : LOGICAL(LGT), optional, intent(in) - if true,
!     quadratureNips is used
!   quadratureAlpha(:) : REAL(DFP), optional, intent(in) - Jacobi alpha of the
!     quadrature; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureBeta(:) : REAL(DFP), optional, intent(in) - Jacobi beta of the
!     quadrature; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureLambda(:) : REAL(DFP), optional, intent(in) - Ultraspherical
!     lambda of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
!
! See also: [[obj_Initiate2]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateGeoFEDOF(obj, dom, baseContinuity, &
                                         baseInterpolation, feType, ipType, &
                                         basisType, alpha, beta, lambda, &
                                         dofType, transformType, &
                                         quadratureIsHomogeneous, &
                                         quadratureType, quadratureOrder, &
                                         quadratureIsOrder, quadratureNips, &
                                         quadratureIsNips, quadratureAlpha, &
                                         quadratureBeta, quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain; the FEDOF uses its mesh of dimension nsd
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis: H1, HCurl, HDiv or DG (the first two letters
    !! are used)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation of the basis: Lagrange, Hierarchical, Orthogonal, Hermit
    !! or Serendipity (the first four letters are used)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: feType
    !! finite element type: scalar, vector or tensor
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ipType
    !! interpolation point type; needed for Lagrange basis
    INTEGER(I4B), INTENT(IN), OPTIONAL :: basisType(:)
    !! basis used to build Lagrange polynomials; for Lagrange basis
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(:)
    !! Jacobi alpha parameter; for Lagrange basis built on Jacobi polynomials
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(:)
    !! Jacobi beta parameter; for Lagrange basis built on Jacobi polynomials
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(:)
    !! Ultraspherical lambda parameter; for Lagrange basis built on
    !! Ultraspherical polynomials
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dofType(:)
    !! degree of freedom type (default nodal); size 4
    INTEGER(I4B), INTENT(IN), OPTIONAL :: transformType
    !! transformation from the reference to the physical element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsHomogeneous
    !! if true, the same quadrature is used in all directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    !! quadrature type; see QuadratureOpt_; size 3 (x, y, z)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureOrder(:)
    !! quadrature accuracy; see QuadratureOpt_; size 3 (x, y, z)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsOrder
    !! if true, quadratureOrder is used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! number of quadrature points; see QuadratureOpt_; size 3 (x, y, z)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsNips
    !! if true, quadratureNips is used
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Jacobi alpha of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    !! Jacobi beta of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    !! Ultraspherical lambda of the quadrature; see QuadratureOpt_; size 3 (x,
    !! y, z)
  END SUBROUTINE obj_InitiateGeoFEDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate a FEDOF with the same order on all cells
!
! Initiate sets the base continuity and interpolation, keeps pointers to the
! domain and to its mesh of dimension nsd, and sets the order of the cells.
! The order of a face is the minimum order of the cells around it, and the
! order of an edge is the minimum order of the cells around it. Then the
! degrees of freedom are numbered: first one per vertex node (the local node
! number), then the edge, face and cell degrees of freedom, entity by entity.
! Finally one finite element is made for each element topology of the mesh
! (FEFactory), with the interpolation and quadrature options.
!
! For Lagrange basis ipType is required. Initiate deallocates obj first.
!
! This routine is for a homogeneous order: all cells get the order order. It
! calls the version with an order vector.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   order : INTEGER(I4B), intent(in) - order of all cells
!   dom : CLASS(AbstractDomain_), intent(in) - domain; the FEDOF uses its mesh
!     of dimension nsd
!   baseContinuity : CHARACTER(*), intent(in) - continuity of the basis: H1,
!     HCurl, HDiv or DG (the first two letters are used)
!   baseInterpolation : CHARACTER(*), intent(in) - interpolation of the basis:
!     Lagrange, Hierarchical, Orthogonal, Hermit or Serendipity (the first
!     four letters are used)
!   feType : INTEGER(I4B), optional, intent(in) - finite element type: scalar,
!     vector or tensor
!   ipType : INTEGER(I4B), optional, intent(in) - interpolation point type;
!     needed for Lagrange basis
!   basisType(:) : INTEGER(I4B), optional, intent(in) - basis used to build
!     Lagrange polynomials; for Lagrange basis
!   alpha(:) : REAL(DFP), optional, intent(in) - Jacobi alpha parameter; for
!     Lagrange basis built on Jacobi polynomials
!   beta(:) : REAL(DFP), optional, intent(in) - Jacobi beta parameter; for
!     Lagrange basis built on Jacobi polynomials
!   lambda(:) : REAL(DFP), optional, intent(in) - Ultraspherical lambda
!     parameter; for Lagrange basis built on Ultraspherical polynomials
!   dofType(:) : INTEGER(I4B), optional, intent(in) - degree of freedom type
!     (default nodal); size 4
!   transformType : INTEGER(I4B), optional, intent(in) - transformation from
!     the reference to the physical element
!   quadratureIsHomogeneous : LOGICAL(LGT), optional, intent(in) - if true,
!     the same quadrature is used in all directions
!   quadratureType(:) : INTEGER(I4B), optional, intent(in) - quadrature type;
!     see QuadratureOpt_; size 3 (x, y, z)
!   quadratureOrder(:) : INTEGER(I4B), optional, intent(in) - quadrature
!     accuracy; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureIsOrder : LOGICAL(LGT), optional, intent(in) - if true,
!     quadratureOrder is used
!   quadratureNips(:) : INTEGER(I4B), optional, intent(in) - number of
!     quadrature points; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureIsNips : LOGICAL(LGT), optional, intent(in) - if true,
!     quadratureNips is used
!   quadratureAlpha(:) : REAL(DFP), optional, intent(in) - Jacobi alpha of the
!     quadrature; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureBeta(:) : REAL(DFP), optional, intent(in) - Jacobi beta of the
!     quadrature; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureLambda(:) : REAL(DFP), optional, intent(in) - Ultraspherical
!     lambda of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: Display
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: OK
!   USE Test_Method, ONLY: IS
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BaseType, ONLY: quadopt => TypeQuadratureOpt
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: found, want
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!
!   CALL test1
!   CALL test2
!   CALL test3
!   CALL test4
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!     CALL obj%Initiate( &
!                        order=1, &
!                        dom=dom, &
!                        baseContinuity="H1", &
!                        baseInterpolation="Hierarchical", &
!                        quadratureIsHomogeneous=math%yes, &
!                        quadratureType=[quadopt%GaussLegendre], &
!                        quadratureOrder=[2], &
!                        quadratureIsOrder=math%yes)
!
!     found = obj%GetTotalDOF()
!     want = meshptr%GetTotalNodes()
!     isok = found .EQ. want
!     CALL OK(isok, "Total DOF matched for order = 1: ")
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     CALL obj%Initiate( &
!                        order=2, &
!                        dom=dom, &
!                        baseContinuity="H1", &
!                        baseInterpolation="Hierarchical", &
!                        quadratureIsHomogeneous=math%yes, &
!                        quadratureType=[quadopt%GaussLegendre], &
!                        quadratureOrder=[4], &
!                        quadratureIsOrder=math%yes)
!
!     found = obj%GetTotalDOF()
!     want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces()
!     isok = found .EQ. want
!     CALL OK(isok, "Total DOF matched for order = 2: ")
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                      test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!     CALL obj%Initiate( &
!                        order=3, &
!                        dom=dom, &
!                        baseContinuity="H1", &
!                        baseInterpolation="Hierarchical", &
!                        quadratureIsHomogeneous=math%yes, &
!                        quadratureType=[quadopt%GaussLegendre], &
!                        quadratureOrder=[6], &
!                        quadratureIsOrder=math%yes)
!
!     found = obj%GetTotalDOF()
!     want = meshptr%GetTotalNodes() &
!       + 2 * meshptr%GetTotalFaces() &
!       + meshptr%GetTotalCells()
!
!     isok = found .EQ. want
!     CALL OK(isok, "Total DOF matched for order = 3: ")
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !                                                                      test4
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test4
!     CALL obj%Initiate( &
!                        order=4, &
!                        dom=dom, &
!                        baseContinuity="H1", &
!                        baseInterpolation="Hierarchical", &
!                        quadratureIsHomogeneous=math%yes, &
!                        quadratureType=[quadopt%GaussLegendre], &
!                        quadratureOrder=[8], &
!                        quadratureIsOrder=math%yes)
!
!     found = obj%GetTotalDOF()
!     want = meshptr%GetTotalNodes() &
!       + 3 * meshptr%GetTotalFaces() &
!       + 3 * meshptr%GetTotalCells()
!
!     isok = found .EQ. want
!     CALL OK(isok, "Total DOF matched for order = 4: ")
!   END SUBROUTINE test4
!
! END PROGRAM main
!```
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: Display
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE BaseType, ONLY: poly => TypePolynomialOpt
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE Test_Method, ONLY: OK
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
!   INTEGER(I4B), PARAMETER :: order = 2, ipType = poly%monomial
!   CHARACTER(*), PARAMETER :: baseContinuity = "H1"
!   CHARACTER(*), PARAMETER :: baseInterpolation = "Lagrange"
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: found, want, entities(4)
!   LOGICAL(LGT) :: isok
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!
!   meshptr => dom%GetMeshPointer()
!
!   CALL fedof%Initiate(baseContinuity=baseContinuity, ipType=ipType, &
!                       baseInterpolation=baseInterpolation, order=order, &
!                       dom=dom)
!
!   entities = meshptr%GetTotalEntities()
!   CALL Display(entities, "Total entities in mesh: ")
!
!   found = fedof%GetTotalDOF()
!
!   IF (order .EQ. 1) THEN
!
!     want = meshptr%GetTotalVertexNodes()
!
!   ELSE
!
!     want = meshptr%GetTotalVertexNodes() + &
!       entities(3) * (order - 1) + &
!       entities(4) * (order - 2) * (order - 1) / 2
!
!   END IF
!
!   isok = found .EQ. want
!   CALL OK(isok, "TotalDOF mathched for order= " // tostring(order) // ": ")
!
! !CALL dom%Display("domain:")
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate2]], [[obj_Initiate4]], [[obj_ImportFromToml2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, order, dom, baseContinuity, &
                                  baseInterpolation, feType, ipType, &
                                  basisType, alpha, beta, lambda, dofType, &
                                  transformType, quadratureIsHomogeneous, &
                                  quadratureType, quadratureOrder, &
                                  quadratureIsOrder, quadratureNips, &
                                  quadratureIsNips, quadratureAlpha, &
                                  quadratureBeta, quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: order
    !! order of all cells
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain; the FEDOF uses its mesh of dimension nsd
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis: H1, HCurl, HDiv or DG (the first two letters
    !! are used)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation of the basis: Lagrange, Hierarchical, Orthogonal, Hermit
    !! or Serendipity (the first four letters are used)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: feType
    !! finite element type: scalar, vector or tensor
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ipType
    !! interpolation point type; needed for Lagrange basis
    INTEGER(I4B), INTENT(IN), OPTIONAL :: basisType(:)
    !! basis used to build Lagrange polynomials; for Lagrange basis
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(:)
    !! Jacobi alpha parameter; for Lagrange basis built on Jacobi polynomials
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(:)
    !! Jacobi beta parameter; for Lagrange basis built on Jacobi polynomials
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(:)
    !! Ultraspherical lambda parameter; for Lagrange basis built on
    !! Ultraspherical polynomials
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dofType(:)
    !! degree of freedom type (default nodal); size 4
    INTEGER(I4B), INTENT(IN), OPTIONAL :: transformType
    !! transformation from the reference to the physical element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsHomogeneous
    !! if true, the same quadrature is used in all directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    !! quadrature type; see QuadratureOpt_; size 3 (x, y, z)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureOrder(:)
    !! quadrature accuracy; see QuadratureOpt_; size 3 (x, y, z)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsOrder
    !! if true, quadratureOrder is used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! number of quadrature points; see QuadratureOpt_; size 3 (x, y, z)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsNips
    !! if true, quadratureNips is used
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Jacobi alpha of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    !! Jacobi beta of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    !! Ultraspherical lambda of the quadrature; see QuadratureOpt_; size 3 (x,
    !! y, z)
  END SUBROUTINE obj_Initiate1

END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate a FEDOF with an order for each cell
!
! Initiate sets the base continuity and interpolation, keeps pointers to the
! domain and to its mesh of dimension nsd, and sets the order of the cells.
! The order of a face is the minimum order of the cells around it, and the
! order of an edge is the minimum order of the cells around it. Then the
! degrees of freedom are numbered: first one per vertex node (the local node
! number), then the edge, face and cell degrees of freedom, entity by entity.
! Finally one finite element is made for each element topology of the mesh
! (FEFactory), with the interpolation and quadrature options.
!
! For Lagrange basis ipType is required. Initiate deallocates obj first.
!
! order(ii) is the order of cell ii. If islocal is true, ii is the local
! element number; else ii is the global element number (the size of order is
! then at least the largest global element number). If order has one entry,
! all cells get that order.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   order(:) : INTEGER(I4B), intent(in) - order of each cell, see islocal
!   dom : CLASS(AbstractDomain_), intent(in) - domain; the FEDOF uses its mesh
!     of dimension nsd
!   baseContinuity : CHARACTER(*), intent(in) - continuity of the basis: H1,
!     HCurl, HDiv or DG (the first two letters are used)
!   baseInterpolation : CHARACTER(*), intent(in) - interpolation of the basis:
!     Lagrange, Hierarchical, Orthogonal, Hermit or Serendipity (the first
!     four letters are used)
!   feType : INTEGER(I4B), optional, intent(in) - finite element type: scalar,
!     vector or tensor
!   ipType : INTEGER(I4B), optional, intent(in) - interpolation point type;
!     needed for Lagrange basis
!   basisType(:) : INTEGER(I4B), optional, intent(in) - basis used to build
!     Lagrange polynomials; for Lagrange basis
!   alpha(:) : REAL(DFP), optional, intent(in) - Jacobi alpha parameter; for
!     Lagrange basis built on Jacobi polynomials
!   lambda(:) : REAL(DFP), optional, intent(in) - Ultraspherical lambda
!     parameter; for Lagrange basis built on Ultraspherical polynomials
!   beta(:) : REAL(DFP), optional, intent(in) - Jacobi beta parameter; for
!     Lagrange basis built on Jacobi polynomials
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, order(ii) is the
!     order of local element ii; else of global element ii; default is false
!   dofType(:) : INTEGER(I4B), optional, intent(in) - degree of freedom type
!     (default nodal); size 4
!   transformType : INTEGER(I4B), optional, intent(in) - transformation from
!     the reference to the physical element
!   quadratureIsHomogeneous : LOGICAL(LGT), optional, intent(in) - if true,
!     the same quadrature is used in all directions
!   quadratureType(:) : INTEGER(I4B), optional, intent(in) - quadrature type;
!     see QuadratureOpt_; size 3 (x, y, z)
!   quadratureOrder(:) : INTEGER(I4B), optional, intent(in) - quadrature
!     accuracy; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureIsOrder : LOGICAL(LGT), optional, intent(in) - if true,
!     quadratureOrder is used
!   quadratureNips(:) : INTEGER(I4B), optional, intent(in) - number of
!     quadrature points; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureIsNips : LOGICAL(LGT), optional, intent(in) - if true,
!     quadratureNips is used
!   quadratureAlpha(:) : REAL(DFP), optional, intent(in) - Jacobi alpha of the
!     quadrature; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureBeta(:) : REAL(DFP), optional, intent(in) - Jacobi beta of the
!     quadrature; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureLambda(:) : REAL(DFP), optional, intent(in) - Ultraspherical
!     lambda of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: Display
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE ReallocateUtility, ONLY: Reallocate
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE Test_Method, ONLY: OK
!   IMPLICIT NONE
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseInterpolation = "Hierarchical", &
!     baseContinuity = "H1"
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: found, want
!   INTEGER(I4B), ALLOCATABLE :: cellOrder(:)
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   CALL Reallocate(cellOrder, meshptr%GetTotalCells())
!
!   CALL test1
!   CALL test2
!   CALL test3
!   CALL test4
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!     cellOrder = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=cellOrder, dom=dom, islocal=math%yes)
!     found = fedof%GetTotalDOF()
!     want = meshptr%GetTotalNodes()
!     isok = found .EQ. want
!     CALL OK(isok, "TotalDOF matched for order=1:")
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     cellOrder = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=cellOrder, dom=dom, islocal=math%yes)
!
!     found = fedof%GetTotalDOF()
!     want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces()
!     isok = found .EQ. want
!     CALL OK(isok, "TotalDOF matched for order=2:")
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                      test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!     cellOrder = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=cellOrder, dom=dom, islocal=math%yes)
!
!     found = fedof%GetTotalDOF()
!     want = meshptr%GetTotalNodes() &
!       + 2 * meshptr%GetTotalFaces() &
!       + meshptr%GetTotalCells()
!     isok = found .EQ. want
!     CALL OK(isok, "TotalDOF matched for order=3:")
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !                                                                      test4
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test4
!     cellOrder = 4
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=cellOrder, dom=dom, islocal=math%yes)
!
!     found = fedof%GetTotalDOF()
!     want = meshptr%GetTotalNodes() &
!       + 3 * meshptr%GetTotalFaces() &
!       + 3 * meshptr%GetTotalCells()
!     isok = found .EQ. want
!     CALL OK(isok, "TotalDOF matched for order=4:")
!   END SUBROUTINE test4
!
! !---------------------------------------------------------------------------
! !
! !---------------------------------------------------------------------------
!
! END PROGRAM main
!```
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: Display
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE ReallocateUtility, ONLY: Reallocate
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE Test_Method, ONLY: OK
!   IMPLICIT NONE
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseInterpolation = "Hierarchical", &
!     baseContinuity = "H1"
!   TYPE(HDF5File_) :: meshfile
!   LOGICAL(LGT) :: isok
!   INTEGER(I4B) :: found, want, order, ii, iel
!   INTEGER(I4B), ALLOCATABLE :: cellOrder(:), aintvec(:)
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!
!   meshptr => dom%GetMeshPointer()
!   CALL Reallocate(cellOrder, meshptr%GetTotalCells())
!
!   aintvec = [14, 15, 17, 24, 18]
!   order = 1
!   DO iel = 1, SIZE(aintvec)
!     isok = meshptr%IsElementPresent(aintvec(iel))
!     IF (.NOT. isok) CYCLE
!
!     ii = meshptr%GetLocalElemNumber(aintvec(iel))
!     cellOrder(ii) = order
!   END DO
!
!   aintvec = [22, 23, 25, 21]
!   order = 2
!   DO iel = 1, SIZE(aintvec)
!     isok = meshptr%IsElementPresent(aintvec(iel))
!     IF (.NOT. isok) CYCLE
!
!     ii = meshptr%GetLocalElemNumber(aintvec(iel))
!     cellOrder(ii) = order
!   END DO
!
!   aintvec = [19, 20, 26, 16, 13]
!   order = 3
!   DO iel = 1, SIZE(aintvec)
!     isok = meshptr%IsElementPresent(aintvec(iel))
!     IF (.NOT. isok) CYCLE
!
!     ii = meshptr%GetLocalElemNumber(aintvec(iel))
!     cellOrder(ii) = order
!   END DO
!
!   CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=cellOrder, dom=dom, islocal=math%yes)
!   found = fedof%GetTotalDOF()
!   want = 39
!   isok = found == want
!   CALL OK(isok, "Total DOF: ")
!   IF (.NOT. isok) CALL Display([found, want], "found, want: ")
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate1]], [[obj_Initiate4]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, order, dom, baseContinuity, &
                                  baseInterpolation, feType, ipType, &
                                  basisType, alpha, lambda, beta, islocal, &
                                  dofType, transformType, &
                                  quadratureIsHomogeneous, quadratureType, &
                                  quadratureOrder, quadratureIsOrder, &
                                  quadratureNips, quadratureIsNips, &
                                  quadratureAlpha, quadratureBeta, &
                                  quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order of each cell, see islocal
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain; the FEDOF uses its mesh of dimension nsd
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis: H1, HCurl, HDiv or DG (the first two letters
    !! are used)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation of the basis: Lagrange, Hierarchical, Orthogonal, Hermit
    !! or Serendipity (the first four letters are used)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: feType
    !! finite element type: scalar, vector or tensor
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ipType
    !! interpolation point type; needed for Lagrange basis
    INTEGER(I4B), INTENT(IN), OPTIONAL :: basisType(:)
    !! basis used to build Lagrange polynomials; for Lagrange basis
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(:)
    !! Jacobi alpha parameter; for Lagrange basis built on Jacobi polynomials
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(:)
    !! Jacobi beta parameter; for Lagrange basis built on Jacobi polynomials
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(:)
    !! Ultraspherical lambda parameter; for Lagrange basis built on
    !! Ultraspherical polynomials
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, order(ii) is the order of local element ii; else of global
    !! element ii; default is false
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dofType(:)
    !! degree of freedom type (default nodal); size 4
    INTEGER(I4B), INTENT(IN), OPTIONAL :: transformType
    !! transformation from the reference to the physical element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsHomogeneous
    !! if true, the same quadrature is used in all directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    !! quadrature type; see QuadratureOpt_; size 3 (x, y, z)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureOrder(:)
    !! quadrature accuracy; see QuadratureOpt_; size 3 (x, y, z)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsOrder
    !! if true, quadratureOrder is used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! number of quadrature points; see QuadratureOpt_; size 3 (x, y, z)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsNips
    !! if true, quadratureNips is used
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Jacobi alpha of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    !! Jacobi beta of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    !! Ultraspherical lambda of the quadrature; see QuadratureOpt_; size 3 (x,
    !! y, z)
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate a FEDOF from a table of element numbers and orders
!
! Initiate sets the base continuity and interpolation, keeps pointers to the
! domain and to its mesh of dimension nsd, and sets the order of the cells.
! The order of a face is the minimum order of the cells around it, and the
! order of an edge is the minimum order of the cells around it. Then the
! degrees of freedom are numbered: first one per vertex node (the local node
! number), then the edge, face and cell degrees of freedom, entity by entity.
! Finally one finite element is made for each element topology of the mesh
! (FEFactory), with the interpolation and quadrature options.
!
! For Lagrange basis ipType is required. Initiate deallocates obj first.
!
! order(1, ii) is a global element number and order(2, ii) its order; there is
! one column for each element. The routine makes the order of each local
! element and calls the version with an order vector.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   order(:,:) : INTEGER(I4B), intent(in) - order(1, :) global element
!     numbers, order(2, :) their orders; one column per element
!   dom : CLASS(AbstractDomain_), intent(in) - domain; the FEDOF uses its mesh
!     of dimension nsd
!   baseContinuity : CHARACTER(*), intent(in) - continuity of the basis: H1,
!     HCurl, HDiv or DG (the first two letters are used)
!   baseInterpolation : CHARACTER(*), intent(in) - interpolation of the basis:
!     Lagrange, Hierarchical, Orthogonal, Hermit or Serendipity (the first
!     four letters are used)
!   feType : INTEGER(I4B), optional, intent(in) - finite element type: scalar,
!     vector or tensor
!   ipType : INTEGER(I4B), optional, intent(in) - interpolation point type;
!     needed for Lagrange basis
!   basisType(:) : INTEGER(I4B), optional, intent(in) - basis used to build
!     Lagrange polynomials; for Lagrange basis
!   alpha(:) : REAL(DFP), optional, intent(in) - Jacobi alpha parameter; for
!     Lagrange basis built on Jacobi polynomials
!   beta(:) : REAL(DFP), optional, intent(in) - Jacobi beta parameter; for
!     Lagrange basis built on Jacobi polynomials
!   lambda(:) : REAL(DFP), optional, intent(in) - Ultraspherical lambda
!     parameter; for Lagrange basis built on Ultraspherical polynomials
!   dofType(:) : INTEGER(I4B), optional, intent(in) - degree of freedom type
!     (default nodal); size 4
!   transformType : INTEGER(I4B), optional, intent(in) - transformation from
!     the reference to the physical element
!   quadratureIsHomogeneous : LOGICAL(LGT), optional, intent(in) - if true,
!     the same quadrature is used in all directions
!   quadratureType(:) : INTEGER(I4B), optional, intent(in) - quadrature type;
!     see QuadratureOpt_; size 3 (x, y, z)
!   quadratureOrder(:) : INTEGER(I4B), optional, intent(in) - quadrature
!     accuracy; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureIsOrder : LOGICAL(LGT), optional, intent(in) - if true,
!     quadratureOrder is used
!   quadratureNips(:) : INTEGER(I4B), optional, intent(in) - number of
!     quadrature points; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureIsNips : LOGICAL(LGT), optional, intent(in) - if true,
!     quadratureNips is used
!   quadratureAlpha(:) : REAL(DFP), optional, intent(in) - Jacobi alpha of the
!     quadrature; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureBeta(:) : REAL(DFP), optional, intent(in) - Jacobi beta of the
!     quadrature; see QuadratureOpt_; size 3 (x, y, z)
!   quadratureLambda(:) : REAL(DFP), optional, intent(in) - Ultraspherical
!     lambda of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE Display_Method, ONLY: Display
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE ReallocateUtility, ONLY: Reallocate
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE Test_Method, ONLY: OK
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
!   CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
!     baseInterpolation = "Hierarchical"
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   LOGICAL(LGT) :: isok
!   INTEGER(I4B) :: found, want, order
!   INTEGER(I4B), ALLOCATABLE :: cellOrder(:, :), aintvec(:)
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   CALL Reallocate(cellOrder, 2, meshptr%GetTotalCells())
!
!   aintvec = [14, 15, 17, 24, 18]
!
!   order = 1
!   cellOrder(1, 1:5) = aintvec
!   cellOrder(2, 1:5) = order
!
!   aintvec = [22, 23, 25, 21]
!   order = 2
!   cellOrder(1, 6:9) = aintvec
!   cellOrder(2, 6:9) = order
!
!   aintvec = [19, 20, 26, 16, 13]
!   order = 3
!   cellOrder(1, 10:14) = aintvec
!   cellOrder(2, 10:14) = order
!
!   CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=cellOrder, dom=dom)
!
!   found = fedof%GetTotalDOF()
!   want = 39
!   isok = found == want
!   CALL OK(isok, "TotalDOF matched: ")
!   IF (.NOT. isok) CALL Display([found, want], "found, want: ")
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate4(obj, order, dom, baseContinuity, &
                                  baseInterpolation, feType, ipType, &
                                  basisType, alpha, beta, lambda, dofType, &
                                  transformType, quadratureIsHomogeneous, &
                                  quadratureType, quadratureOrder, &
                                  quadratureIsOrder, quadratureNips, &
                                  quadratureIsNips, quadratureAlpha, &
                                  quadratureBeta, quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: order(:, :)
    !! order(1, :) global element numbers, order(2, :) their orders; one
    !! column per element
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain; the FEDOF uses its mesh of dimension nsd
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis: H1, HCurl, HDiv or DG (the first two letters
    !! are used)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation of the basis: Lagrange, Hierarchical, Orthogonal, Hermit
    !! or Serendipity (the first four letters are used)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: feType
    !! finite element type: scalar, vector or tensor
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ipType
    !! interpolation point type; needed for Lagrange basis
    INTEGER(I4B), INTENT(IN), OPTIONAL :: basisType(:)
    !! basis used to build Lagrange polynomials; for Lagrange basis
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(:)
    !! Jacobi alpha parameter; for Lagrange basis built on Jacobi polynomials
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(:)
    !! Jacobi beta parameter; for Lagrange basis built on Jacobi polynomials
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(:)
    !! Ultraspherical lambda parameter; for Lagrange basis built on
    !! Ultraspherical polynomials
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dofType(:)
    !! degree of freedom type (default nodal); size 4
    INTEGER(I4B), INTENT(IN), OPTIONAL :: transformType
    !! transformation from the reference to the physical element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsHomogeneous
    !! if true, the same quadrature is used in all directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    !! quadrature type; see QuadratureOpt_; size 3 (x, y, z)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureOrder(:)
    !! quadrature accuracy; see QuadratureOpt_; size 3 (x, y, z)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsOrder
    !! if true, quadratureOrder is used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! number of quadrature points; see QuadratureOpt_; size 3 (x, y, z)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: quadratureIsNips
    !! if true, quadratureNips is used
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Jacobi alpha of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    !! Jacobi beta of the quadrature; see QuadratureOpt_; size 3 (x, y, z)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    !! Ultraspherical lambda of the quadrature; see QuadratureOpt_; size 3 (x,
    !! y, z)
  END SUBROUTINE obj_Initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                           AllocateSizes@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary: Allocate the order and degree of freedom arrays
!
! Read the number of vertex nodes, edges, faces and cells of the mesh and
! allocate cellOrder, faceOrder, edgeOrder and the index arrays edgeIA, faceIA
! and cellIA (one entry more than the number of entities).
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!
! See also: [[obj_SetOrdersFromCellOrder]]

INTERFACE
  MODULE SUBROUTINE obj_AllocateSizes(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_AllocateSizes
END INTERFACE

!----------------------------------------------------------------------------
!                                  SetOrdersFromCellOrder@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary: Number the degrees of freedom
!
! Loop over the active cells and count the degrees of freedom of each edge,
! face and cell (from their orders, the base continuity and the base
! interpolation). The degrees of freedom are numbered after the vertex degrees
! of freedom: the degrees of freedom of edge iedge are edgeIA(iedge) to
! edgeIA(iedge + 1) - 1, and likewise for faces (faceIA) and cells (cellIA).
! tdof is the total number.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!
! See also: [[obj_AllocateSizes]]

INTERFACE
  MODULE SUBROUTINE obj_SetOrdersFromCellOrder(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_SetOrdersFromCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Copy a FEDOF
!
! Copy the data of obj2 into obj; ASSIGNMENT(=) calls this method. The
! pointers to the domain, the mesh and the finite elements are copied, not the
! objects they point to, so obj and obj2 share the finite elements.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   obj2 : CLASS(FEDOF_), intent(in) - FEDOF object to copy
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!
!   USE GlobalData, ONLY: I4B
!   USE Test_Method, ONLY: ok
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   IMPLICIT NONE
!
!   TYPE(FEDOF_) :: obj, obj2
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   CHARACTER(*), PARAMETER :: &
!     filename = "./meshdata/small_tri3_mesh.h5", &
!     baseInterpolation = "Hierarchical", &
!     baseContinuity = "H1"
!
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: found, want
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!
!   meshptr => dom%GetMeshPointer()
!
!   CALL obj%Initiate(baseContinuity=baseContinuity, &
!                     baseInterpolation=baseInterpolation, order=1, dom=dom)
! !CALL fedof%Display("FEDOF:")
!   CALL obj2%copy(obj)
!
!   found = obj2%GetTotalDOF()
!   want = meshptr%GetTotalNodes()
!   CALL OK(found == want, "Total DOF (order=1): ")
!
!   CALL obj%Initiate(baseContinuity=baseContinuity, &
!                     baseInterpolation=baseInterpolation, order=2, dom=dom)
!   obj2 = obj
!   found = obj2%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces()
!   CALL OK(found == want, "Total DOF (order=2): ")
!
!   CALL obj%Initiate(baseContinuity=baseContinuity, &
!                     baseInterpolation=baseInterpolation, order=3, dom=dom)
!   CALL obj2%Copy(obj)
!
!   found = obj2%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + 2 * meshptr%GetTotalFaces() + &
!     meshptr%GetTotalCells()
!   CALL OK(found == want, "Total DOF (order=3): ")
!
!   CALL obj%Initiate(baseContinuity=baseContinuity, &
!                     baseInterpolation=baseInterpolation, order=4, dom=dom)
!   obj2 = obj
!
!   found = obj2%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + 3 * meshptr%GetTotalFaces() + &
!     3 * meshptr%GetTotalCells()
!   CALL OK(found == want, "Total DOF (order=4): ")
!
! !CALL dom%Display("domain:")
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CLASS(FEDOF_), INTENT(IN) :: obj2
    !! FEDOF object to copy
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Deallocate the FEDOF
!
! Reset all data, nullify the domain and mesh pointers, and call Deallocate on
! each finite element and nullify its pointer.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractDomain_Class, ONLY: AbstractDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   CLASS(AbstractMesh_), POINTER :: meshptr
!   CLASS(AbstractDomain_), POINTER :: domptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated before Initiate")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!
!   meshptr => obj%GetMeshPointer()
!   CALL OK(ASSOCIATED(meshptr, dom%GetMeshPointer()), "GetMeshPointer")
!   domptr => obj%GetDomainPointer()
!   CALL OK(ASSOCIATED(domptr), "GetDomainPointer")
!   CALL OK(domptr%GetTotalNodes() .EQ. 9_I4B, "domain of the FEDOF")
!
!   CALL OK(obj%GetBaseInterpolation() .EQ. "HIER", "GetBaseInterpolation")
!   CALL OK(obj%GetTotalVertexDOF() .EQ. 9_I4B, "GetTotalVertexDOF")
!
!   meshptr => NULL()
!   domptr => NULL()
!   CALL obj%DEALLOCATE()
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsInitiated]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                              IsInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary: Return true if the FEDOF is initiated
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!
! Returns:
!   ans : LOGICAL(LGT) - true if the FEDOF is initiated
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractDomain_Class, ONLY: AbstractDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   CLASS(AbstractMesh_), POINTER :: meshptr
!   CLASS(AbstractDomain_), POINTER :: domptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated before Initiate")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!
!   meshptr => obj%GetMeshPointer()
!   CALL OK(ASSOCIATED(meshptr, dom%GetMeshPointer()), "GetMeshPointer")
!   domptr => obj%GetDomainPointer()
!   CALL OK(ASSOCIATED(domptr), "GetDomainPointer")
!   CALL OK(domptr%GetTotalNodes() .EQ. 9_I4B, "domain of the FEDOF")
!
!   CALL OK(obj%GetBaseInterpolation() .EQ. "HIER", "GetBaseInterpolation")
!   CALL OK(obj%GetTotalVertexDOF() .EQ. 9_I4B, "GetTotalVertexDOF")
!
!   meshptr => NULL()
!   domptr => NULL()
!   CALL obj%DEALLOCATE()
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    LOGICAL(LGT) :: ans
    !! true if the FEDOF is initiated
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetTotalCells@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Get total number of cells in fedof mesh
!
! Sometimes we just need total number of cell elements in mesh.
! Getting mesh pointer from fedof, just for doing this tasks
! seems waste of resources. So this method returns total number of
! cells in fedof.
!
INTERFACE
  MODULE FUNCTION obj_GetTotalCells(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalCells
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetCaseName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-27
! summary: Get the name of the basis case
!
! Return baseContinuity followed by baseInterpolation (four letters), for
! example H1LAGR or H1HIER.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!
! Returns:
!   ans : CHARACTER(6) - base continuity and base interpolation
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!
!   USE Test_Method, ONLY: ok
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE BaseType, ONLY: TypeQuadratureOpt
!
!   IMPLICIT NONE
!
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_tri3_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   CHARACTER(6) :: found, want
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!
!   meshptr => dom%GetMeshPointer()
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   found = obj%GetCaseName()
!   want = "H1HIER"
!   CALL OK(found == want, "GetCaseName H1HIER: ")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   found = obj%GetCaseName()
!   want = "H1HIER"
!   CALL OK(found == want, "GetCaseName H1HIER: ")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Lagrange", &
!                     order=1, dom=dom, ipType=TypeQuadratureOpt%equidistance)
!   found = obj%GetCaseName()
!   want = "H1LAGR"
!   CALL OK(found == want, "GetCaseName H1LAGR: ")
!
! !CALL dom%Display("domain:")
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_GetCaseName(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    CHARACTER(6) :: ans
    !! base continuity and base interpolation
  END FUNCTION obj_GetCaseName
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get the degrees of freedom of an element
!
! Return the degree of freedom numbers of an element. With opt Vertex, Edge,
! Face or Cell only those degrees of freedom are returned; with All, the
! vertex, edge, face and cell degrees of freedom, in this order. Use a one
! letter opt (V, E, F, C or A): GetConnectivity_ selects the case on the whole
! opt string.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   opt : CHARACTER(*), intent(in) - which degrees of freedom: Vertex, Edge,
!     Face, Cell or All (the first letter is used)
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - degree of freedom numbers of the
!     element
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: ok
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE AppendUtility, ONLY: Append
!   USE AppendUtility, ONLY: OPERATOR(.APPEND.)
!   USE ArangeUtility, ONLY: Arange
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: &
!     filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", &
!     baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: found(:), want(:)
!   INTEGER(I4B) :: order, totalVertexNodes, totalFaces
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   totalVertexNodes = meshptr%GetTotalVertexNodes()
!   totalFaces = meshptr%GetTotalFaces()
!
!   CALL test1
!   CALL test2
!   CALL test3
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!
!     INTEGER(I4B), ALLOCATABLE :: globalNode(:)
!     INTEGER(I4B) :: ent(4), tVertices
!
!     order = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     tVertices = &
!       meshptr%GetTotalVertexNodes(globalElement=[1], islocal=math%yes)
!
!     ent = meshptr%GetTotalEntities(globalElement=1, islocal=math%yes)
!
!     ! Get all connectivity for local element 1
!     found = fedof%GetConnectivity(opt="A", globalElement=1, &
!             islocal=math%yes)
!
!     globalNode = meshptr%GetConnectivity(globalElement=1, islocal=math%yes)
!
!     want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
!                                       islocal=math%no)
!
!     isok = ALL(found == want)
!     CALL OK(isok, testname // " GetConnectivity (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     INTEGER(I4B), ALLOCATABLE :: globalNode(:), temp1(:)
!     INTEGER(I4B) :: ent(4), a, b, tVertices
!
!     order = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetConnectivity(opt="A", globalElement=1, &
!     islocal=math%yes)
!     globalNode = meshptr%GetConnectivity(globalElement=1, islocal=math%yes)
!     tVertices = &
!       meshptr%GetTotalVertexNodes(globalElement=[1], islocal=math%yes)
!
!     want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
!                                       islocal=math%no)
!     a = meshptr%GetTotalVertexNodes() + 1
!     ent = meshptr%GetTotalEntities(globalElement=1, islocal=math%yes)
!
!     b = meshptr%GetTotalVertexNodes() + ent(3) * (order - 1)
!     temp1 = Arange(a, b)
!
!     want = want.APPEND.temp1
!
!     isok = ALL(found == want)
!
!     CALL OK(isok, testname // " GetConnectivity (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!     INTEGER(I4B), ALLOCATABLE :: globalNode(:), temp1(:)
!     INTEGER(I4B) :: ent(4), a, b, tVertices
!
!     order = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetConnectivity(opt="A", globalElement=1, &
!             islocal=math%yes)
!
!     globalNode = meshptr%GetConnectivity(globalElement=1, islocal=math%yes)
!
!     tVertices = &
!       meshptr%GetTotalVertexNodes(globalElement=[1], islocal=math%yes)
!
!     want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
!                                       islocal=math%no)
!
!     ent = meshptr%GetTotalEntities(globalElement=1, islocal=math%yes)
!
!     a = totalVertexNodes + 1
!     b = a - 1 + ent(3) * (order - 1)
!
!     temp1 = Arange(a, b)
!
!     want = want.APPEND.temp1
!
!     a = totalVertexNodes + totalFaces * (order - 1) + 1
!     b = a - 1 + ent(4) * (order - 2) * (order - 1) * 0.5
!
!     temp1 = Arange(a, b)
!     want = want.APPEND.temp1
!
!     isok = ALL(found == want)
!
!     CALL OK(isok, testname // " GetConnectivity (order= " // &
!             ToString(order) // "): ")
!   END SUBROUTINE test3
!
! END PROGRAM main
!```
!
! See also: [[obj_GetConnectivity_]]

INTERFACE
  MODULE FUNCTION obj_GetConnectivity(obj, opt, globalElement, islocal) &
    RESULT(ans)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CHARACTER(*), INTENT(IN) :: opt
    !! which degrees of freedom: Vertex, Edge, Face, Cell or All (the first
    !! letter is used)
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! degree of freedom numbers of the element
  END FUNCTION obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetConnectivity_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get the degrees of freedom of an element without allocation
!
! Same as GetConnectivity, but the numbers are written in ans. ans must have
! at least GetTotalDOF(globalElement, opt) entries. Use a one letter opt (V,
! E, F, C or A); a longer opt such as Vertex is treated as All.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   ans(:) : INTEGER(I4B), intent(inout) - degree of freedom numbers, size
!     must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   opt : CHARACTER(*), intent(in) - which degrees of freedom: Vertex, Edge,
!     Face, Cell or All (the first letter is used)
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! See also: [[obj_GetConnectivity]], [[obj_GetMaxTotalConnectivity]]

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity_(obj, ans, tsize, opt, &
                                         globalElement, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! degree of freedom numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    CHARACTER(*), INTENT(IN) :: opt
    !! which degrees of freedom: Vertex, Edge, Face, Cell or All (the first
    !! letter is used)
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetConnectivity_
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetConnectivity_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get the degrees of freedom of a facet of an element
!
! Write the vertex connectivity of facet localFaceNumber of an element (from
! the mesh) followed by the degree of freedom numbers on the face.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   ans(:) : INTEGER(I4B), intent(inout) - vertex connectivity of the facet
!     followed by the face degrees of freedom
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localFaceNumber : INTEGER(I4B), intent(in) - local face number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: ans(20), tsize, order
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   DO order = 1, 2
!     CALL obj%Initiate(baseContinuity="H1", &
!                       baseInterpolation="Hierarchical", order=order, &
!                       dom=dom)
!     CALL obj%GetFacetConnectivity_(ans=ans, tsize=tsize, globalElement=1, &
!                                    localFaceNumber=1, islocal=math%yes)
!     CALL OK(tsize .EQ. 2 + order - 1, "GetFacetConnectivity_: tsize")
!   END DO
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetFaceDOF2]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetConnectivity_(obj, ans, tsize, &
                                              globalElement, &
                                              localFaceNumber, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! vertex connectivity of the facet followed by the face degrees of
    !! freedom
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetFacetConnectivity_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary: Get the degree of freedom of a vertex node
!
! A vertex node has one degree of freedom: its local node number.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   ans(:) : INTEGER(I4B), intent(inout) - degree of freedom numbers, size
!     must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans (1)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: ok
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE ReallocateUtility, ONLY: Reallocate
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: &
!     filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", &
!     baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: found(:), want(:)
!   INTEGER(I4B) :: order, totalVertexNodes, totalFaces
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   totalVertexNodes = meshptr%GetTotalVertexNodes()
!   totalFaces = meshptr%GetTotalFaces()
!
!   CALL test1
!   CALL test2
!   CALL test3
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!
!     INTEGER(I4B) :: tsize
!
!     order = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL Reallocate(found, 2, want, 2)
!     CALL fedof%GetVertexDOF(globalNode=1, islocal=math%yes, &
!                             ans=found, tsize=tsize)
!
!     want(1) = 1
!
!     isok = ALL(found == want)
!     CALL OK(isok, testname // " GetVertexDOF (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!
!     INTEGER(I4B) :: tsize
!
!     order = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL Reallocate(found, 2, want, 2)
!     CALL fedof%GetVertexDOF(globalNode=1, islocal=math%yes, &
!                             ans=found, tsize=tsize)
!
!     want(1) = 1
!
!     isok = ALL(found == want)
!     CALL OK(isok, testname // " GetVertexDOF (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!
!     INTEGER(I4B) :: tsize
!
!     order = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL Reallocate(found, 2, want, 2)
!     CALL fedof%GetVertexDOF(globalNode=1, islocal=math%yes, &
!                             ans=found, tsize=tsize)
!
!     want(1) = 1
!
!     isok = ALL(found == want)
!     CALL OK(isok, testname // " GetVertexDOF (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !
! !---------------------------------------------------------------------------
!
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalVertexDOF]]

INTERFACE
  MODULE SUBROUTINE obj_GetVertexDOF(obj, globalNode, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! degree of freedom numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans (1)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary: Get the degrees of freedom on an edge
!
! Write the degree of freedom numbers of an edge, edgeIA(globalEdge) to
! edgeIA(globalEdge + 1) - 1. globalEdge is the edge number in the mesh;
! islocal is not used.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalEdge : INTEGER(I4B), intent(in) - edge number in the mesh
!   ans(:) : INTEGER(I4B), intent(inout) - degree of freedom numbers, size
!     must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - not used
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_hexa8_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: ans(10), tsize, iedge, order
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   DO order = 2, 3
!     CALL obj%Initiate(baseContinuity="H1", &
!                       baseInterpolation="Hierarchical", order=order, &
!                       dom=dom)
!     IF (order .EQ. 2) CALL OK(obj%GetTotalDOF() .EQ. 45_I4B, &
!                               "GetTotalDOF for order 2")
!
!     DO iedge = 1, 12
!       CALL OK(obj%GetTotalEdgeDOF(globalElement=1, localEdgeNumber=iedge, &
!                                   islocal=math%yes) .EQ. order - 1, &
!               "GetTotalEdgeDOF")
!     END DO
!
!     CALL obj%GetEdgeDOF(globalElement=1, localEdgeNumber=1, ans=ans, &
!                         tsize=tsize, islocal=math%yes)
!     CALL OK(tsize .EQ. order - 1, "GetEdgeDOF: number of entries")
!     CALL OK(ALL(ans(1:tsize) .GT. 12), &
!             "GetEdgeDOF: edge DOF follow the vertex DOF")
!   END DO
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetEdgeDOF2]], [[obj_GetTotalEdgeDOF1]]

INTERFACE
  MODULE SUBROUTINE obj_GetEdgeDOF1(obj, globalEdge, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalEdge
    !! edge number in the mesh
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! degree of freedom numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! not used
  END SUBROUTINE obj_GetEdgeDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-21
! summary: Get the degrees of freedom on an edge of an element
!
! Get the edge number of edge localEdgeNumber of the element from the mesh,
! then call GetEdgeDOF with the edge number.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localEdgeNumber : INTEGER(I4B), intent(in) - local edge number in the
!     element
!   ans(:) : INTEGER(I4B), intent(inout) - degree of freedom numbers, size
!     must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! See also: [[obj_GetEdgeDOF1]]

INTERFACE
  MODULE SUBROUTINE obj_GetEdgeDOF2(obj, globalElement, localEdgeNumber, &
                                    ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local edge number in the element
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! degree of freedom numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetEdgeDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetTotalEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the number of degrees of freedom on an edge
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalEdge : INTEGER(I4B), intent(in) - edge number in the mesh
!   islocal : LOGICAL(LGT), optional, intent(in) - not used
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom on the edge
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_hexa8_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: ans(10), tsize, iedge, order
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   DO order = 2, 3
!     CALL obj%Initiate(baseContinuity="H1", &
!                       baseInterpolation="Hierarchical", order=order, &
!                       dom=dom)
!     IF (order .EQ. 2) CALL OK(obj%GetTotalDOF() .EQ. 45_I4B, &
!                               "GetTotalDOF for order 2")
!
!     DO iedge = 1, 12
!       CALL OK(obj%GetTotalEdgeDOF(globalElement=1, localEdgeNumber=iedge, &
!                                   islocal=math%yes) .EQ. order - 1, &
!               "GetTotalEdgeDOF")
!     END DO
!
!     CALL obj%GetEdgeDOF(globalElement=1, localEdgeNumber=1, ans=ans, &
!                         tsize=tsize, islocal=math%yes)
!     CALL OK(tsize .EQ. order - 1, "GetEdgeDOF: number of entries")
!     CALL OK(ALL(ans(1:tsize) .GT. 12), &
!             "GetEdgeDOF: edge DOF follow the vertex DOF")
!   END DO
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalEdgeDOF2]], [[obj_GetEdgeDOF1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalEdgeDOF1(obj, globalEdge, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalEdge
    !! edge number in the mesh
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! not used
    INTEGER(I4B) :: ans
    !! number of degrees of freedom on the edge
  END FUNCTION obj_GetTotalEdgeDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the number of degrees of freedom on an edge of an element
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localEdgeNumber : INTEGER(I4B), intent(in) - local edge number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom on the edge
!
! See also: [[obj_GetTotalEdgeDOF1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalEdgeDOF2(obj, globalElement, localEdgeNumber, &
                                       islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local edge number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
    INTEGER(I4B) :: ans
    !! number of degrees of freedom on the edge
  END FUNCTION obj_GetTotalEdgeDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-21
! summary: Get the degrees of freedom on a face
!
! Write the degree of freedom numbers of a face, faceIA(globalFace) to
! faceIA(globalFace + 1) - 1. globalFace is the face number in the mesh;
! islocal is not used.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalFace : INTEGER(I4B), intent(in) - face number in the mesh
!   ans(:) : INTEGER(I4B), intent(inout) - degree of freedom numbers, size
!     must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - not used
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: is
!   USE Test_Method, ONLY: ok
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation // &
!     " GetFaceDOF test"
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: order, totalVertexNodes, totalFaces
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   totalVertexNodes = meshptr%GetTotalVertexNodes()
!   totalFaces = meshptr%GetTotalFaces()
!
!   CALL test1
!   CALL test2
!   CALL test3
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!
!     INTEGER(I4B) :: tsize, found(10), want(10)
!     INTEGER(I4B), ALLOCATABLE :: con(:)
!
!     order = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     con = fedof%GetConnectivity(globalElement=1, islocal=math%yes, opt="A")
!
!     CALL fedof%GetFaceDOF(globalFace=1, islocal=math%yes, &
!                           ans=found, tsize=tsize)
!
!     isok = tsize .EQ. 0
!     CALL OK(isok, testname // " (order= " // ToString(order) // "): ")
!
!     CALL fedof%GetFaceDOF(globalElement=1, islocal=math%yes, &
!                           ans=found, tsize=tsize, localFaceNumber=1)
!     isok = tsize .EQ. 0
!     CALL OK(isok, testname // " interface 2 (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetFaceDOF(globalFace=1, islocal=math%yes, &
!                           ans=found, tsize=tsize)
!
!     CALL IS(tsize, 1, testname // " (order= " // ToString(order) // "): ")
!     CALL IS(found(1), 13, &
!             testname // " (order= " // ToString(order) // "): ")
!
!     CALL fedof%GetFaceDOF(globalElement=1, islocal=math%yes, &
!                           ans=found, tsize=tsize, localFaceNumber=1)
!
!     CALL IS(tsize, 1, testname // " (order= " // ToString(order) // "): ")
!     CALL IS(found(1), 13, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetFaceDOF(globalFace=1, islocal=math%yes, &
!                           ans=found, tsize=tsize)
!
!     CALL IS(tsize, 2, testname // " (order= " // ToString(order) // "): ")
!     CALL IS(found(1), 13, &
!             testname // " (order= " // ToString(order) // "): ")
!
!     CALL fedof%GetFaceDOF(globalElement=1, islocal=math%yes, &
!                           ans=found, tsize=tsize, localFaceNumber=1)
!
!     CALL IS(tsize, 2, testname // " (order= " // ToString(order) // "): ")
!     CALL IS(found(1), 13, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !
! !---------------------------------------------------------------------------
!
! END PROGRAM main
!```
!
! See also: [[obj_GetFaceDOF2]], [[obj_GetTotalFaceDOF1]]

INTERFACE
  MODULE SUBROUTINE obj_GetFaceDOF1(obj, globalFace, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalFace
    !! face number in the mesh
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! degree of freedom numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! not used
  END SUBROUTINE obj_GetFaceDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-21
! summary: Get the degrees of freedom on a face of an element
!
! Get the face number of face localFaceNumber of the element from the mesh,
! then call GetFaceDOF with the face number.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localFaceNumber : INTEGER(I4B), intent(in) - local face number in the
!     element
!   ans(:) : INTEGER(I4B), intent(inout) - degree of freedom numbers, size
!     must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! See also: [[obj_GetFaceDOF1]]

INTERFACE
  MODULE SUBROUTINE obj_GetFaceDOF2(obj, globalElement, localFaceNumber, &
                                    ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in the element
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! degree of freedom numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetFaceDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetCellDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary: Get the degrees of freedom inside a cell
!
! Write the degree of freedom numbers inside a cell, cellIA(iel) to cellIA(iel
! + 1) - 1, where iel is the local element number.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalCell : INTEGER(I4B), intent(in) - global element number of the cell,
!     or local element number if islocal is true
!   ans(:) : INTEGER(I4B), intent(inout) - degree of freedom numbers, size
!     must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: is
!   USE Test_Method, ONLY: ok
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation // &
!     " GetCellDOF test"
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: order, totalVertexNodes, totalFaces
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   totalVertexNodes = meshptr%GetTotalVertexNodes()
!   totalFaces = meshptr%GetTotalFaces()
!
!   CALL test1
!   CALL test2
!   CALL test3
!   CALL test4
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetCellDOF(globalCell=1, islocal=math%yes, &
!                           ans=found, tsize=tsize)
!
!     isok = tsize .EQ. 0
!     CALL OK(isok, testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetCellDOF(globalCell=1, islocal=math%yes, &
!                           ans=found, tsize=tsize)
!
!     CALL IS(tsize, 0, testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetCellDOF(globalCell=1, islocal=math%yes, &
!                           ans=found, tsize=tsize)
!
!     CALL IS(tsize, 1, testname // " (order= " // ToString(order) // "): ")
!     CALL IS(found(1), 63, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test4
!
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 4
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetCellDOF(globalCell=1, islocal=math%yes, &
!                           ans=found, tsize=tsize)
!
!     CALL IS(tsize, 3, testname // " (order= " // ToString(order) // "): ")
!     CALL IS(found(1), 12 + 25 * 3 + 1, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test4
!
! !---------------------------------------------------------------------------
! !
! !---------------------------------------------------------------------------
!
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalCellDOF]]

INTERFACE
  MODULE SUBROUTINE obj_GetCellDOF(obj, globalCell, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalCell
    !! global element number of the cell, or local element number if islocal
    !! is true
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! degree of freedom numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetCellDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary: Get the number of degrees of freedom of a cell
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalCell : INTEGER(I4B), intent(in) - global element number of the cell,
!     or local element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom inside the cell
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: is
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation // &
!     " GetTotalCellDOF test"
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: order, totalVertexNodes, totalFaces
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   totalVertexNodes = meshptr%GetTotalVertexNodes()
!   totalFaces = meshptr%GetTotalFaces()
!
!   CALL test1
!   CALL test2
!   CALL test3
!   CALL test4
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!
!     INTEGER(I4B) :: tsize, want, found
!
!     order = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalCellDOF(globalCell=1, islocal=math%yes)
!     want = 0
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!
!     INTEGER(I4B) :: tsize, want, found
!
!     order = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalCellDOF(globalCell=1, islocal=math%yes)
!     want = 0
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!
!     INTEGER(I4B) :: tsize, want, found
!
!     order = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalCellDOF(globalCell=1, islocal=math%yes)
!     want = 1
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test4
!
!     INTEGER(I4B) :: tsize, want, found
!
!     order = 4
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalCellDOF(globalCell=1, islocal=math%yes)
!     want = 3
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test4
!
! !---------------------------------------------------------------------------
! !
! !---------------------------------------------------------------------------
!
! END PROGRAM main
!```
!
! See also: [[obj_GetCellDOF]]

INTERFACE
  MODULE FUNCTION obj_GetTotalCellDOF(obj, globalCell, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalCell
    !! global element number of the cell, or local element number if islocal
    !! is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
    INTEGER(I4B) :: ans
    !! number of degrees of freedom inside the cell
  END FUNCTION obj_GetTotalCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-27
! summary: Get the number of degrees of freedom on a face
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalFace : INTEGER(I4B), intent(in) - face number in the mesh
!   islocal : LOGICAL(LGT), optional, intent(in) - not used
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom on the face
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: is
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation // &
!     " GetTotalFaceDOF test"
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: order, totalVertexNodes, totalFaces
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   totalVertexNodes = meshptr%GetTotalVertexNodes()
!   totalFaces = meshptr%GetTotalFaces()
!
!   CALL test1
!   CALL test2
!   CALL test3
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!
!     INTEGER(I4B) :: tsize, found, want
!     INTEGER(I4B), ALLOCATABLE :: con(:)
!
!     order = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalFaceDOF(globalFace=1, islocal=math%yes)
!     want = 0
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!     found = fedof%GetTotalFaceDOF(globalElement=1, islocal=math%yes, &
!                                   localFaceNumber=1)
!
!     CALL IS(found, want, &
!             testname // " interface 2 (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     INTEGER(I4B) :: tsize, found, want
!
!     order = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalFaceDOF(globalFace=1, islocal=math%yes)
!     want = 1
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!     found = fedof%GetTotalFaceDOF(globalElement=1, islocal=math%yes, &
!                                   localFaceNumber=1)
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!     INTEGER(I4B) :: tsize, found, want
!
!     order = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalFaceDOF(globalFace=1, islocal=math%yes)
!     want = 2
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!     found = fedof%GetTotalFaceDOF(globalElement=1, islocal=math%yes, &
!                                   localFaceNumber=1)
!
!     CALL IS(found, want, &
!             testname // " (order= " // ToString(order) // "): ")
!
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !
! !---------------------------------------------------------------------------
!
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalFaceDOF2]]

INTERFACE
  MODULE FUNCTION obj_GetTotalFaceDOF1(obj, globalFace, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalFace
    !! face number in the mesh
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! not used
    INTEGER(I4B) :: ans
    !! number of degrees of freedom on the face
  END FUNCTION obj_GetTotalFaceDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-27
! summary: Get the number of degrees of freedom on a face of an element
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localFaceNumber : INTEGER(I4B), intent(in) - local face number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom on the face
!
! See also: [[obj_GetTotalFaceDOF1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalFaceDOF2(obj, globalElement, localFaceNumber, &
                                       islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
    INTEGER(I4B) :: ans
    !! number of degrees of freedom on the face
  END FUNCTION obj_GetTotalFaceDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-29
! summary: Get the number of vertex degrees of freedom
!
! This is the number of vertex nodes of the mesh.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex degrees of freedom
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractDomain_Class, ONLY: AbstractDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   CLASS(AbstractMesh_), POINTER :: meshptr
!   CLASS(AbstractDomain_), POINTER :: domptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated before Initiate")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!
!   meshptr => obj%GetMeshPointer()
!   CALL OK(ASSOCIATED(meshptr, dom%GetMeshPointer()), "GetMeshPointer")
!   domptr => obj%GetDomainPointer()
!   CALL OK(ASSOCIATED(domptr), "GetDomainPointer")
!   CALL OK(domptr%GetTotalNodes() .EQ. 9_I4B, "domain of the FEDOF")
!
!   CALL OK(obj%GetBaseInterpolation() .EQ. "HIER", "GetBaseInterpolation")
!   CALL OK(obj%GetTotalVertexDOF() .EQ. 9_I4B, "GetTotalVertexDOF")
!
!   meshptr => NULL()
!   domptr => NULL()
!   CALL obj%DEALLOCATE()
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetVertexDOF]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexDOF(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B) :: ans
    !! number of vertex degrees of freedom
  END FUNCTION obj_GetTotalVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Get the total number of degrees of freedom
!
! GetTotalDOF is a generic method: without arguments it returns the number of
! degrees of freedom of the FEDOF; with globalElement, the number of degrees
! of freedom of that element; with globalElement and opt, only those of the
! vertices, edges, faces or cell (opt V, E, F or C) or all (A).
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!
! Returns:
!   ans : INTEGER(I4B) - total number of degrees of freedom
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: is
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: &
!     filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", &
!     baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation // " GetTotalDOF"
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: order, totalVertexNodes, totalFaces
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   totalVertexNodes = meshptr%GetTotalVertexNodes()
!   totalFaces = meshptr%GetTotalFaces()
!
!   CALL test1
!   CALL test2
!   CALL test3
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!     INTEGER(I4B) :: tsize, found, want
!
!     order = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalDOF()
!     want = meshptr%GetTotalVertexNodes()
!     CALL IS(found, want, testname // "  interface 1 (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes)
!     want = meshptr%GetTotalVertexNodes(globalElement=1, islocal=math%yes)
!     CALL IS(found, want, testname // " interface 2 (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="A")
!     want = meshptr%GetTotalVertexNodes(globalElement=1, islocal=math%yes)
!     CALL IS(found, want, testname // "  interface 3 a (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="E")
!     want = 0
!     CALL IS(found, want, testname // "  interface 3 e (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="F")
!     want = 0
!     CALL IS(found, want, testname // "  interface 3 f (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="c")
!     want = 0
!     CALL IS(found, want, testname // "  interface 3 c (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     INTEGER(I4B) :: tsize, found, want
!
!     order = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalDOF()
!     want = &
!       meshptr%GetTotalVertexNodes() + meshptr%GetTotalFaces() * (order - 1)
!     CALL IS(found, want, testname // "  interface 1 (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes)
!     want = 6
!     CALL IS(found, want, testname // " interface 2 (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="A")
!     want = 6
!     CALL IS(found, want, testname // "  interface 3 a (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="E")
!     want = 0
!     CALL IS(found, want, testname // "  interface 3 e (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="F")
!     want = 3
!     CALL IS(found, want, testname // "  interface 3 f (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="c")
!     want = 0
!     CALL IS(found, want, testname // "  interface 3 c (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!     INTEGER(I4B) :: tsize, found, want
!
!     order = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     found = fedof%GetTotalDOF()
!     want = meshptr%GetTotalVertexNodes() + &
!       meshptr%GetTotalFaces() * (order - 1) + &
!       meshptr%GetTotalCells() * (order - 2) * (order - 1) * 0.5
!     CALL IS(found, want, testname // "  interface 1 (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes)
!     want = 10
!     CALL IS(found, want, testname // " interface 2 (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="A")
!     want = 10
!     CALL IS(found, want, testname // "  interface 3 a (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="E")
!     want = 0
!     CALL IS(found, want, testname // "  interface 3 e (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="F")
!     want = 6
!     CALL IS(found, want, testname // "  interface 3 f (order= " // &
!             ToString(order) // "): ")
!
!     found = fedof%GetTotalDOF(globalElement=1, islocal=math%yes, opt="c")
!     want = 1
!     CALL IS(found, want, testname // "  interface 3 c (order= " // &
!             ToString(order) // "): ")
!
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !
! !---------------------------------------------------------------------------
!
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalDOF2]], [[obj_GetTotalDOF3]]

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF1(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B) :: ans
    !! total number of degrees of freedom
  END FUNCTION obj_GetTotalDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Get the number of degrees of freedom of an element
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom of the element
!     (vertices, edges, faces and cell)
!
! See also: [[obj_GetTotalDOF1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF2(obj, globalElement, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
    INTEGER(I4B) :: ans
    !! number of degrees of freedom of the element (vertices, edges, faces and
    !! cell)
  END FUNCTION obj_GetTotalDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Get the number of degrees of freedom of an element for an entity
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   opt : CHARACTER(*), intent(in) - which degrees of freedom: Vertex, Edge,
!     Face, Cell or All (the first letter is used)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom of the element selected
!     by opt
!
! See also: [[obj_GetTotalDOF1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF3(obj, globalElement, opt, &
                                   islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    CHARACTER(*), INTENT(IN) :: opt
    !! which degrees of freedom: Vertex, Edge, Face, Cell or All (the first
    !! letter is used)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
    INTEGER(I4B) :: ans
    !! number of degrees of freedom of the element selected by opt
  END FUNCTION obj_GetTotalDOF3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-19
! summary: Display the FEDOF
!
! Display the flags, sizes, base continuity and interpolation, orders, the
! domain and mesh information, the sizes of the order and index arrays, and
! each finite element.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=2, dom=dom)
!   CALL obj%Display("FEDOF:")
!   CALL obj%DisplayCellOrder("cell order:", full=math%yes)
!   CALL OK(obj%IsInitiated(), "Display")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_DisplayCellOrder]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-19
! summary: Display the order of the cells
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!   full : LOGICAL(LGT), optional, intent(in) - if true, display all entries
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=2, dom=dom)
!   CALL obj%Display("FEDOF:")
!   CALL obj%DisplayCellOrder("cell order:", full=math%yes)
!   CALL OK(obj%IsInitiated(), "Display")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Display]]

INTERFACE
  MODULE SUBROUTINE obj_DisplayCellOrder(obj, msg, unitno, full)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: full
    !! if true, display all entries
  END SUBROUTINE obj_DisplayCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetCellOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the order of the cells
!
! If order has one entry, all cells get that order. Else order(ii) is the
! order of element ii, where ii is a local element number if islocal is true
! and a global element number if not. Also sets the largest cell, face and
! edge orders.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   order(:) : INTEGER(I4B), intent(in) - order of the cells (one entry: all
!     cells), see islocal
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, order(ii) is the
!     order of local element ii; else of global element ii; default is false
!
! See also: [[obj_SetFaceOrder]], [[obj_SetEdgeOrder]]

INTERFACE
  MODULE SUBROUTINE obj_SetCellOrder(obj, order, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order of the cells (one entry: all cells), see islocal
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, order(ii) is the order of local element ii; else of global
    !! element ii; default is false
  END SUBROUTINE obj_SetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetFaceOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the order of the faces
!
! The order of a face is the minimum order of the two cells which share it.
! Call SetCellOrder first.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!
! See also: [[obj_SetCellOrder]]

INTERFACE
  MODULE SUBROUTINE obj_SetFaceOrder(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_SetFaceOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetEdgeOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the order of the edges
!
! The order of an edge is the minimum order of the cells around it. Call
! SetCellOrder first.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!
! See also: [[obj_SetCellOrder]]

INTERFACE
  MODULE SUBROUTINE obj_SetEdgeOrder(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_SetEdgeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetNodeCoord@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-18
! summary: Set the node coordinates of the finite element
!
! TODO: This routine needs to be implemented; it does nothing.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! See also: [[obj_SetFE]]

INTERFACE
  MODULE SUBROUTINE obj_SetNodeCoord(obj, globalElement, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_SetNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetFE@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-22
! summary: Set the finite element of an element
!
! Set the cell, face and edge orders and orientations of the element in the
! finite element of its topology, and set the quadrature order to
! scaleForQuadOrder times the cell order. Call it before using the finite
! element of GetFEPointer for that element.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: Display
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE BaseType, ONLY: QuadraturePoint_
!   USE BaseType, ONLY: quadopt => TypeQuadratureOpt
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE AbstractFE_Class, ONLY: AbstractFE_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", &
!     baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // &
!     baseInterpolation // &
!     " GetQuadraturePoints test:"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   CLASS(AbstractFE_), POINTER :: feptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   TYPE(QuadraturePoint_) :: quad
!   INTEGER(I4B) :: globalElement, telements, order
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!
!   CALL test1
!   CALL test2
!   CALL test3
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!     order = 1
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, &
!                       dom=dom, &
!                       ipType=quadopt%equidistance, &
!                       quadratureIsHomogeneous=math%yes, &
!                       quadratureType=[quadopt%GaussLegendre], &
!                       quadratureOrder=[2 * order], &
!                       quadratureIsOrder=math%yes)
!
!     telements = 1
!     globalElement = 1
!     order = 2
!
!     CALL obj%SetFE(globalElement=1, islocal=math%yes)
!     feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
!
!     CALL feptr%GetQuadraturePoints(quad=quad)
!
!     CALL check_quad(3_I4B, 2.0_DFP, testname // " test1")
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     order = 2
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, &
!                       dom=dom, &
!                       ipType=quadopt%equidistance, &
!                       quadratureIsHomogeneous=math%yes, &
!                       quadratureType=[quadopt%GaussLegendre], &
!                       quadratureOrder=[2 * order], &
!                       quadratureIsOrder=math%yes)
!
!     CALL obj%SetFE(globalElement=1, islocal=math%yes)
!     feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
!     CALL feptr%GetQuadraturePoints(quad=quad)
!     CALL check_quad(6_I4B, 2.0_DFP, testname // " test2")
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!     order = 3
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, &
!                       dom=dom, &
!                       ipType=quadopt%equidistance, &
!                       quadratureIsHomogeneous=math%yes, &
!                       quadratureType=[quadopt%GaussLegendre], &
!                       quadratureOrder=[2 * order], &
!                       quadratureIsOrder=math%yes)
!
!     CALL obj%SetFE(globalElement=1, islocal=math%yes)
!     feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
!     CALL feptr%GetQuadraturePoints(quad=quad)
!     CALL check_quad(0_I4B, 2.0_DFP, testname // " test3")
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !                                                                check_quad
! !---------------------------------------------------------------------------
!
! ! Check the number of quadrature points (if nwant > 0) and the sum of the
! ! weights (the last row of quad%points), which is the area of the
! ! reference triangle.
!
!   SUBROUTINE check_quad(nwant, area, msg)
!     INTEGER(I4B), INTENT(IN) :: nwant
!     REAL(DFP), INTENT(IN) :: area
!     CHARACTER(*), INTENT(IN) :: msg
!     INTEGER(I4B) :: nips, nrow
!
!     nrow = SIZE(quad%points, 1)
!     nips = SIZE(quad%points, 2)
!     IF (nwant .GT. 0) CALL OK(nips .EQ. nwant, msg // " number of points")
!     CALL OK(ABS(SUM(quad%points(nrow, :)) - area) .LT. 1.0E-8_DFP, &
!             msg // " sum of weights")
!   END SUBROUTINE check_quad
!
! END PROGRAM main
!```
!
! See also: [[obj_GetFEPointer]]

INTERFACE
  MODULE SUBROUTINE obj_SetFE(obj, globalElement, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_SetFE
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Get a pointer to the mesh of the FEDOF
!
! This is the mesh of dimension nsd of the domain.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!
! Returns:
!   ans : CLASS(AbstractMesh_), pointer - pointer to the mesh
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractDomain_Class, ONLY: AbstractDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   CLASS(AbstractMesh_), POINTER :: meshptr
!   CLASS(AbstractDomain_), POINTER :: domptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated before Initiate")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!
!   meshptr => obj%GetMeshPointer()
!   CALL OK(ASSOCIATED(meshptr, dom%GetMeshPointer()), "GetMeshPointer")
!   domptr => obj%GetDomainPointer()
!   CALL OK(ASSOCIATED(domptr), "GetDomainPointer")
!   CALL OK(domptr%GetTotalNodes() .EQ. 9_I4B, "domain of the FEDOF")
!
!   CALL OK(obj%GetBaseInterpolation() .EQ. "HIER", "GetBaseInterpolation")
!   CALL OK(obj%GetTotalVertexDOF() .EQ. 9_I4B, "GetTotalVertexDOF")
!
!   meshptr => NULL()
!   domptr => NULL()
!   CALL obj%DEALLOCATE()
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetDomainPointer]]

INTERFACE
  MODULE FUNCTION obj_GetMeshPointer(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    CLASS(AbstractMesh_), POINTER :: ans
    !! pointer to the mesh
  END FUNCTION obj_GetMeshPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetDomainPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-24
! summary: Get a pointer to the domain of the FEDOF
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!
! Returns:
!   ans : CLASS(AbstractDomain_), pointer - pointer to the domain
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractDomain_Class, ONLY: AbstractDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   CLASS(AbstractMesh_), POINTER :: meshptr
!   CLASS(AbstractDomain_), POINTER :: domptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated before Initiate")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!
!   meshptr => obj%GetMeshPointer()
!   CALL OK(ASSOCIATED(meshptr, dom%GetMeshPointer()), "GetMeshPointer")
!   domptr => obj%GetDomainPointer()
!   CALL OK(ASSOCIATED(domptr), "GetDomainPointer")
!   CALL OK(domptr%GetTotalNodes() .EQ. 9_I4B, "domain of the FEDOF")
!
!   CALL OK(obj%GetBaseInterpolation() .EQ. "HIER", "GetBaseInterpolation")
!   CALL OK(obj%GetTotalVertexDOF() .EQ. 9_I4B, "GetTotalVertexDOF")
!
!   meshptr => NULL()
!   domptr => NULL()
!   CALL obj%DEALLOCATE()
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetMeshPointer]]

INTERFACE
  MODULE FUNCTION obj_GetDomainPointer(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    CLASS(AbstractDomain_), POINTER :: ans
    !! pointer to the domain
  END FUNCTION obj_GetDomainPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetBaseInterpolation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the base interpolation
!
! Return the first four letters of the base interpolation in upper case, for
! example LAGR or HIER.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!
! Returns:
!   ans : CHARACTER(:), allocatable - base interpolation
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractDomain_Class, ONLY: AbstractDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   CLASS(AbstractMesh_), POINTER :: meshptr
!   CLASS(AbstractDomain_), POINTER :: domptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated before Initiate")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!
!   meshptr => obj%GetMeshPointer()
!   CALL OK(ASSOCIATED(meshptr, dom%GetMeshPointer()), "GetMeshPointer")
!   domptr => obj%GetDomainPointer()
!   CALL OK(ASSOCIATED(domptr), "GetDomainPointer")
!   CALL OK(domptr%GetTotalNodes() .EQ. 9_I4B, "domain of the FEDOF")
!
!   CALL OK(obj%GetBaseInterpolation() .EQ. "HIER", "GetBaseInterpolation")
!   CALL OK(obj%GetTotalVertexDOF() .EQ. 9_I4B, "GetTotalVertexDOF")
!
!   meshptr => NULL()
!   domptr => NULL()
!   CALL obj%DEALLOCATE()
!   CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetCaseName]]

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    CHARACTER(:), ALLOCATABLE :: ans
    !! base interpolation
  END FUNCTION obj_GetBaseInterpolation
END INTERFACE

!----------------------------------------------------------------------------
!
!                                                      GetCellOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: Get the order of the cell of an element
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   ans(:) : INTEGER(I4B), intent(inout) - order of the cell, size at least 1
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans (1)
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: is
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation // &
!     " GetCellDOF test"
!
!   TYPE(FEDOF_) :: fedof
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: order, totalVertexNodes, totalFaces
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   totalVertexNodes = meshptr%GetTotalVertexNodes()
!   totalFaces = meshptr%GetTotalFaces()
!
!   CALL test1
!   CALL test2
!   CALL test3
!   CALL test4
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 1
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetCellOrder(globalElement=1, islocal=math%yes, &
!                             ans=found, tsize=tsize)
!
!     want(1) = 1
!     CALL IS(found(1), want(1), &
!             testname // " (order= " // ToString(order) // "): ")
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 2
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetCellOrder(globalElement=1, islocal=math%yes, &
!                             ans=found, tsize=tsize)
!
!     want(1) = 2
!     CALL IS(found(1), want(1), &
!             testname // " (order= " // ToString(order) // "): ")
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 3
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetCellOrder(globalElement=1, islocal=math%yes, &
!                             ans=found, tsize=tsize)
!
!     want(1) = 3
!     CALL IS(found(1), want(1), &
!             testname // " (order= " // ToString(order) // "): ")
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test4
!     INTEGER(I4B) :: tsize, found(10), want(10)
!
!     order = 4
!     CALL fedof%Initiate(baseContinuity=baseContinuity, &
!                         baseInterpolation=baseInterpolation, &
!                         order=order, dom=dom)
!
!     CALL fedof%GetCellOrder(globalElement=1, islocal=math%yes, &
!                             ans=found, tsize=tsize)
!
!     want(1) = 4
!     CALL IS(found(1), want(1), &
!             testname // " (order= " // ToString(order) // "): ")
!   END SUBROUTINE test4
!
! !---------------------------------------------------------------------------
! !
! !---------------------------------------------------------------------------
!
! END PROGRAM main
!```
!
! See also: [[obj_GetOrders]]

INTERFACE
  MODULE SUBROUTINE obj_GetCellOrder(obj, ans, tsize, globalElement, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! order of the cell, size at least 1
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans (1)
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!
!                                                      GetCellOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: Get the order of the faces of an element
!
! ans(1:3, jj) is the order of face jj of the element; the three rows hold the
! same order.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   ans(:,:) : INTEGER(I4B), intent(inout) - order of each face, SIZE(ans, 1)
!     >= 3
!   nrow : INTEGER(I4B), intent(out) - number of rows written in ans (3)
!   ncol : INTEGER(I4B), intent(out) - number of columns written in ans
!     (number of faces)
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_hexa8_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: faceOrder(3, 10), edgeOrder(20), nrow, ncol, tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=2, dom=dom)
!
!   CALL obj%GetFaceOrder(ans=faceOrder, nrow=nrow, ncol=ncol, &
!                         globalElement=1, islocal=math%yes)
!   CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 6_I4B, "GetFaceOrder: shape")
!   CALL OK(ALL(faceOrder(1:nrow, 1:ncol) .EQ. 2), "GetFaceOrder: order")
!
!   CALL obj%GetEdgeOrder(ans=edgeOrder, tsize=tsize, globalElement=1, &
!                         islocal=math%yes)
!   CALL OK(tsize .EQ. 12_I4B, "GetEdgeOrder: number of edges")
!   CALL OK(ALL(edgeOrder(1:tsize) .EQ. 2), "GetEdgeOrder: order")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetOrders]]

INTERFACE
  MODULE SUBROUTINE obj_GetFaceOrder(obj, ans, nrow, ncol, &
                                     globalElement, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! order of each face, SIZE(ans, 1) >= 3
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow: number of rows written in ans (3)
    !! ncol: number of columns written in ans (number of faces)
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetFaceOrder
END INTERFACE

!----------------------------------------------------------------------------
!
!                                                      GetCellOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: Get the order of the edges of an element
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   ans(:) : INTEGER(I4B), intent(inout) - order of each edge
!   tsize : INTEGER(I4B), intent(out) - number of edges written in ans
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_hexa8_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: faceOrder(3, 10), edgeOrder(20), nrow, ncol, tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=2, dom=dom)
!
!   CALL obj%GetFaceOrder(ans=faceOrder, nrow=nrow, ncol=ncol, &
!                         globalElement=1, islocal=math%yes)
!   CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 6_I4B, "GetFaceOrder: shape")
!   CALL OK(ALL(faceOrder(1:nrow, 1:ncol) .EQ. 2), "GetFaceOrder: order")
!
!   CALL obj%GetEdgeOrder(ans=edgeOrder, tsize=tsize, globalElement=1, &
!                         islocal=math%yes)
!   CALL OK(tsize .EQ. 12_I4B, "GetEdgeOrder: number of edges")
!   CALL OK(ALL(edgeOrder(1:tsize) .EQ. 2), "GetEdgeOrder: order")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetOrders]]

INTERFACE
  MODULE SUBROUTINE obj_GetEdgeOrder(obj, ans, tsize, globalElement, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! order of each edge
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of edges written in ans
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetEdgeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetOrders@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the cell, face and edge orders and orientations of an element
!
! Get the order of the cell, of each face and of each edge of an element (as
! GetCellOrder, GetFaceOrder and GetEdgeOrder), and their orientation flags
! from the mesh. SetFE uses this method.
!
! Note: the orientations are read with islocal = math%yes, so pass a local
! element number (islocal = math%yes).
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   cellOrder(:) : INTEGER(I4B), intent(inout) - order of the cell, size at
!     least 1
!   faceOrder(:,:) : INTEGER(I4B), intent(inout) - order of each face,
!     SIZE(faceOrder, 1) >= 3 (the three rows hold the same order)
!   edgeOrder(:) : INTEGER(I4B), intent(inout) - order of each edge
!   cellOrient(:) : INTEGER(I4B), intent(inout) - orientation flags of the
!     cell
!   faceOrient(:,:) : INTEGER(I4B), intent(inout) - orientation flags of each
!     face, 3 rows
!   edgeOrient(:) : INTEGER(I4B), intent(inout) - orientation flag of each
!     edge
!   tCellOrder : INTEGER(I4B), intent(out) - number of entries written in
!     cellOrder (1)
!   tFaceOrder : INTEGER(I4B), intent(out) - number of faces written in
!     faceOrder
!   tEdgeOrder : INTEGER(I4B), intent(out) - number of entries written in
!     edgeOrder
!   tCellOrient : INTEGER(I4B), intent(out) - number of entries written in
!     cellOrient
!   tFaceOrient(2) : INTEGER(I4B), intent(out) - number of rows and columns
!     written in faceOrient
!   tEdgeOrient : INTEGER(I4B), intent(out) - number of entries written in
!     edgeOrient
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   INTEGER(I4B), PARAMETER :: order = 2
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   INTEGER(I4B) :: cellOrder(3), faceOrder(3, 10), edgeOrder(20), &
!     cellOrient(3), faceOrient(3, 10), edgeOrient(20), &
!     tCellOrder, tFaceOrder, tEdgeOrder, tCellOrient, &
!     tFaceOrient(2), tEdgeOrient, iel
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!   meshptr => dom%GetMeshPointer()
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=order, dom=dom)
!
!   isok = math%yes
!   DO iel = 1, meshptr%GetTotalElements()
!     CALL obj%GetOrders(cellOrder=cellOrder, faceOrder=faceOrder, &
!                        edgeOrder=edgeOrder, cellOrient=cellOrient, &
!                        faceOrient=faceOrient, edgeOrient=edgeOrient, &
!                        tCellOrder=tCellOrder, tFaceOrder=tFaceOrder, &
!                        tEdgeOrder=tEdgeOrder, tCellOrient=tCellOrient, &
!                        tFaceOrient=tFaceOrient, tEdgeOrient=tEdgeOrient, &
!                        globalElement=iel, islocal=math%yes)
!
!     isok = isok .AND. (tCellOrder .EQ. 1) .AND. (cellOrder(1) .EQ. order)
!     isok = isok .AND. (tFaceOrder .EQ. 4) &
!       .AND. ALL(faceOrder(1:3, 1:tFaceOrder) .EQ. order)
!   END DO
!   CALL OK(isok, "GetOrders: cell and face orders of all elements")
!
!   meshptr => NULL()
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetCellOrder]], [[obj_GetFaceOrder]], [[obj_GetEdgeOrder]]

INTERFACE
  MODULE SUBROUTINE obj_GetOrders(obj, cellOrder, faceOrder, edgeOrder, &
                                  cellOrient, faceOrient, edgeOrient, &
                                  tCellOrder, tFaceOrder, tEdgeOrder, &
                                  tCellOrient, tFaceOrient, tEdgeOrient, &
                                  globalElement, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: cellOrder(:)
    !! order of the cell, size at least 1
    INTEGER(I4B), INTENT(INOUT) :: faceOrder(:, :)
    !! order of each face, SIZE(faceOrder, 1) >= 3 (the three rows hold the
    !! same order)
    INTEGER(I4B), INTENT(INOUT) :: edgeOrder(:)
    !! order of each edge
    INTEGER(I4B), INTENT(INOUT) :: cellOrient(:)
    !! orientation flags of the cell
    INTEGER(I4B), INTENT(INOUT) :: faceOrient(:, :)
    !! orientation flags of each face, 3 rows
    INTEGER(I4B), INTENT(INOUT) :: edgeOrient(:)
    !! orientation flag of each edge
    INTEGER(I4B), INTENT(OUT) :: tCellOrder
    !! number of entries written in cellOrder (1)
    INTEGER(I4B), INTENT(OUT) :: tFaceOrder
    !! number of faces written in faceOrder
    INTEGER(I4B), INTENT(OUT) :: tEdgeOrder
    !! number of entries written in edgeOrder
    INTEGER(I4B), INTENT(OUT) :: tCellOrient
    !! number of entries written in cellOrient
    INTEGER(I4B), INTENT(OUT) :: tFaceOrient(2)
    !! number of rows and columns written in faceOrient
    INTEGER(I4B), INTENT(OUT) :: tEdgeOrient
    !! number of entries written in edgeOrient
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
  END SUBROUTINE obj_GetOrders
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetMaxTotalConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Get the largest number of degrees of freedom of an element
!
! Return the largest number of degrees of freedom of an element, the size
! needed for ans of GetConnectivity_. The value is computed once and stored.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!
! Returns:
!   ans : INTEGER(I4B) - largest number of degrees of freedom of an element
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: TypeQuadratureOpt
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: is
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   IMPLICIT NONE
!
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   CHARACTER(*), PARAMETER :: &
!     filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", &
!     baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // baseInterpolation // &
!     " GetMaxTotalConnectivity test"
!
!   INTEGER(I4B) :: order = 1
!
!   TYPE(HDF5File_) :: meshfile
!   LOGICAL(LGT) :: isok
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!
!   CALL test1
!   CALL test2
!   CALL test3
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
!   SUBROUTINE test1
!     INTEGER(I4B) :: found, want
!
!     order = 1
!
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, dom=dom, &
!                       ipType=TypeQuadratureOpt%equidistance)
!
!     found = obj%GetMaxTotalConnectivity()
!     want = 3
!     isok = found .EQ. want
!
!     CALL IS(found, want, testname // " test 1")
!   END SUBROUTINE test1
!
!   SUBROUTINE test2
!     INTEGER(I4B) :: found, want
!
!     order = 2
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, dom=dom, &
!                       ipType=TypeQuadratureOpt%equidistance)
!
!     found = obj%GetMaxTotalConnectivity()
!     want = 6
!     isok = found .EQ. want
!
!     CALL IS(found, want, testname // " test 2")
!   END SUBROUTINE test2
!
!   SUBROUTINE test3
!     INTEGER(I4B) :: found, want
!
!     order = 3
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, dom=dom, &
!                       ipType=TypeQuadratureOpt%equidistance)
!
!     found = obj%GetMaxTotalConnectivity()
!     want = 10
!     isok = found .EQ. want
!
!     CALL IS(found, want, testname // " test 3")
!   END SUBROUTINE test3
!
! END PROGRAM main
!```
!
! See also: [[obj_GetConnectivity_]]

INTERFACE
  MODULE FUNCTION obj_GetMaxTotalConnectivity(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B) :: ans
    !! largest number of degrees of freedom of an element
  END FUNCTION obj_GetMaxTotalConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetMaxTotalQuadraturePoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Get the number of quadrature points of the elements
!
! For each element, set the quadrature order of its finite element to
! scaleForQuadOrder times its cell order and get the number of quadrature
! points. The value is computed once and stored.
!
! Note: the value of the last element is returned, not the largest one; it is
! the same when all elements have the same topology and order.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!
! Returns:
!   ans : INTEGER(I4B) - number of quadrature points
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_tri3_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=1, dom=dom)
!   CALL OK(obj%GetMaxTotalQuadraturePoints() .EQ. 3_I4B, "order 1")
!
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     order=2, dom=dom)
!   CALL OK(obj%GetMaxTotalQuadraturePoints() .EQ. 6_I4B, "order 2")
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetFE]]

INTERFACE
  MODULE FUNCTION obj_GetMaxTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B) :: ans
    !! number of quadrature points
  END FUNCTION obj_GetMaxTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetSparsity@SetSparsityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-09
! summary: Set the sparsity pattern of a matrix
!
! Two degrees of freedom are connected if they belong to the same element: for
! each element, all its degrees of freedom are connected to each other. Then
! the pattern of mat is finished (CSRMatrix SetSparsity).
! FEDOFSetSparsity(obj, mat) calls this method too.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: CSRMatrix_
!   USE BaseType, ONLY: DOF_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
!   USE CSRMatrix_Method, ONLY: ASSIGNMENT(=)
!   USE DOF_Method, ONLY: DOFInitiate => Initiate
!   USE DOF_Method, ONLY: DOFSize => SIZE
!   USE Display_Method, ONLY: Display
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE GlobalData, ONLY: NODES_FMT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
!     baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // &
!     baseInterpolation // " SetSparsity "
!   INTEGER(I4B) :: ii, jj, order
!   TYPE(CSRMatrix_) :: mat
!   TYPE(DOF_) :: dofobj
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
!   LOGICAL(LGT) :: isok
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   CALL test1
!   CALL test2
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                     test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!     order = 1
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, dom=dom)
!     CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
!                      names=["K"], spaceCompo=[1], timeCompo=[1], &
!                      storageFMT=NODES_FMT)
!     CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), &
!                            nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
!     CALL obj%SetSparsity(mat=mat)
!
!     ! set all entries of the pattern to 1, and convert to a dense matrix
!     mat = 1.0_DFP
!     found = mat
!
!     IF (ALLOCATED(want)) DEALLOCATE(want)
!     ALLOCATE(want(9, 9))
!     want = 0.0_DFP
!     want(1, [1, 5, 7, 9]) = 1.0_DFP
!     want(2, [2, 5, 8, 9]) = 1.0_DFP
!     want(3, [3, 6, 7, 9]) = 1.0_DFP
!     want(4, [4, 6, 8, 9]) = 1.0_DFP
!     want(5, [5, 1, 2, 7, 8, 9]) = 1.0_DFP
!     want(6, [6, 3, 4, 7, 8, 9]) = 1.0_DFP
!     want(7, [7, 1, 3, 5, 6, 9]) = 1.0_DFP
!     want(8, [8, 2, 4, 5, 6, 9]) = 1.0_DFP
!     want(9, [9, 1, 2, 3, 4, 5, 6, 7, 8]) = 1.0_DFP
!
!     isok = ALL(SHAPE(found) .EQ. [9, 9])
!     IF (isok) THEN
!       do_jj: DO jj = 1, 9
!         DO ii = 1, 9
!           isok = found(ii, jj) .EQ. want(ii, jj)
!           IF (.NOT. isok) THEN
!             CALL Display([ii, jj], "ii, jj: ")
!             EXIT do_jj
!           END IF
!         END DO
!       END DO do_jj
!     END IF
!     CALL OK(isok, testname // "order 1, all rows")
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                     test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     order = 2
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, dom=dom)
!     CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
!                      names=["K"], spaceCompo=[1], timeCompo=[1], &
!                      storageFMT=NODES_FMT)
!     CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), &
!                            nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
!     CALL obj%SetSparsity(mat=mat)
!
!     mat = 1.0_DFP
!     found = mat
!     want = found
!     want = 0.0_DFP
!
!     want(1, [1, 5, 7, 9, 10, 11, 12, 13, 22]) = 1.0_DFP
!     CALL check_row(1, "order 2, row 1")
!
!     want(5, [5, 1, 2, 7, 8, 9, 10, 11, 12, 13, 22, 17, 18, 19, 24]) = &
!       1.0_DFP
!     CALL check_row(5, "order 2, row 5")
!
!     want(11, [11, 1, 2, 5, 7, 8, 9, 10, 12, 13, 22, 17, 18, 19, 24]) = &
!       1.0_DFP
!     CALL check_row(11, "order 2, row 11")
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                 check_row
! !---------------------------------------------------------------------------
!
!   SUBROUTINE check_row(irow, msg)
!     INTEGER(I4B), INTENT(IN) :: irow
!     CHARACTER(*), INTENT(IN) :: msg
!
!     DO jj = 1, SIZE(want, 2)
!       isok = found(irow, jj) .EQ. want(irow, jj)
!       IF (.NOT. isok) THEN
!         CALL Display([irow, jj], "ii, jj: ")
!         EXIT
!       END IF
!     END DO
!     CALL OK(isok, testname // msg)
!   END SUBROUTINE check_row
!
! END PROGRAM main
!```
!
! See also: [[obj_SetSparsity2]], [[obj_SetSparsity3]]

INTERFACE FEDOFSetSparsity
  MODULE SUBROUTINE obj_SetSparsity1(obj, mat)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
  END SUBROUTINE obj_SetSparsity1
END INTERFACE FEDOFSetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the sparsity pattern between two FEDOF
!
! For each element iel of obj, connect its degrees of freedom (rows) to the
! degrees of freedom of element cellToCell(iel) of col_fedof (columns), in the
! block (ivar, jvar) of mat. The pattern is not finished; the caller finishes
! it.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   col_fedof : CLASS(FEDOF_), intent(inout) - FEDOF of the columns
!   cellToCell(:) : INTEGER(I4B), intent(in) - cell to cell map:
!     cellToCell(iel) is the global element of col_fedof for local element iel
!     of obj
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!   ivar : INTEGER(I4B), intent(in) - physical variable of the rows
!   jvar : INTEGER(I4B), intent(in) - physical variable of the columns
!
! See also: [[obj_SetSparsity3]]

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity2(obj, col_fedof, cellToCell, mat, &
                                     ivar, jvar)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CLASS(FEDOF_), INTENT(INOUT) :: col_fedof
    !! FEDOF of the columns
    INTEGER(I4B), INTENT(IN) :: cellToCell(:)
    !! cell to cell map: cellToCell(iel) is the global element of col_fedof
    !! for local element iel of obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
  END SUBROUTINE obj_SetSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetSparsity@SetSparsityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-12
! summary: Set the sparsity pattern for a list of FEDOF
!
! Each FEDOF is a physical variable. For a RECTANGLE matrix, fedofs(1) gives
! the rows and fedofs(2) the columns. Else the block (ivar, jvar) of mat gets
! the pattern between fedofs(ivar) and fedofs(jvar); the cell to cell map
! between their meshes is made by FEDomainConnectivity_. All FEDOF must have
! the same nsd.
!
! Arguments:
!   fedofs(:) : CLASS(FEDOFPointer_), intent(inout) - FEDOF of each physical
!     variable
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: CSRMatrix_
!   USE BaseType, ONLY: DOF_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
!   USE CSRMatrix_Method, ONLY: GetNNZ
!   USE DOF_Method, ONLY: DOFInitiate => Initiate
!   USE DOF_Method, ONLY: DOFSize => SIZE
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDOF_Class, ONLY: FEDOFPointer_
!   USE FEDOF_Class, ONLY: FEDOFSetSparsity
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: NODES_FMT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEDOF_), TARGET :: obj
!   TYPE(FEDOFPointer_) :: fedofs(1)
!   TYPE(FEDomain_) :: dom
!   TYPE(HDF5File_) :: meshfile
!   TYPE(DOF_) :: dofobj
!   TYPE(CSRMatrix_) :: mat1, mat2
!   INTEGER(I4B) :: order
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, "")
!
!   DO order = 1, 2
!     CALL obj%Initiate(baseContinuity="H1", &
!                       baseInterpolation="Hierarchical", order=order, &
!                       dom=dom)
!     CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
!                      names=["K"], spaceCompo=[1], timeCompo=[1], &
!                      storageFMT=NODES_FMT)
!
!     ! one FEDOF: type-bound SetSparsity
!     CALL CSRMatrixInitiate(mat1, ncol=DOFSize(dofobj), &
!                            nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
!     CALL obj%SetSparsity(mat=mat1)
!
!     ! list of FEDOF: FEDOFSetSparsity
!     CALL CSRMatrixInitiate(mat2, ncol=DOFSize(dofobj), &
!                            nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
!     fedofs(1)%ptr => obj
!     CALL FEDOFSetSparsity(fedofs=fedofs, mat=mat2)
!     fedofs(1)%ptr => NULL()
!
!     CALL OK(GetNNZ(mat2) .EQ. GetNNZ(mat1), &
!             "FEDOFSetSparsity: same nonzeros as SetSparsity")
!     IF (order .EQ. 1) CALL OK(GetNNZ(mat1) .EQ. 49_I4B, &
!                               "order 1: 49 nonzeros")
!   END DO
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetSparsity1]], [[obj_SetSparsity2]]

INTERFACE FEDOFSetSparsity
  MODULE SUBROUTINE obj_SetSparsity3(fedofs, mat)
    CLASS(FEDOFPointer_), INTENT(INOUT) :: fedofs(:)
    !! FEDOF of each physical variable
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
  END SUBROUTINE obj_SetSparsity3
END INTERFACE FEDOFSetSparsity

!----------------------------------------------------------------------------
!                                                  GetFEPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-21
! summary: Get the finite element of an element
!
! Return the finite element of the topology of the element. Call SetFE for the
! element first, so that the orders and orientations of the finite element are
! those of the element.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(in) - FEDOF object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element,
!     node, edge or face number is a local number; default is false
!
! Returns:
!   ans : CLASS(AbstractFE_), pointer - pointer to the finite element
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: Display
!   USE Display_Method, ONLY: ToString
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE BaseType, ONLY: QuadraturePoint_
!   USE BaseType, ONLY: quadopt => TypeQuadratureOpt
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE AbstractFE_Class, ONLY: AbstractFE_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     baseContinuity = "H1", &
!     baseInterpolation = "Hierarchical", &
!     testname = baseContinuity // " " // &
!     baseInterpolation // &
!     " GetQuadraturePoints test:"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   CLASS(AbstractFE_), POINTER :: feptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   TYPE(QuadraturePoint_) :: quad
!   INTEGER(I4B) :: globalElement, telements, order
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!
!   CALL test1
!   CALL test2
!   CALL test3
!
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                      test1
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test1
!     order = 1
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, &
!                       dom=dom, &
!                       ipType=quadopt%equidistance, &
!                       quadratureIsHomogeneous=math%yes, &
!                       quadratureType=[quadopt%GaussLegendre], &
!                       quadratureOrder=[2 * order], &
!                       quadratureIsOrder=math%yes)
!
!     telements = 1
!     globalElement = 1
!     order = 2
!
!     CALL obj%SetFE(globalElement=1, islocal=math%yes)
!     feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
!
!     CALL feptr%GetQuadraturePoints(quad=quad)
!
!     CALL check_quad(3_I4B, 2.0_DFP, testname // " test1")
!   END SUBROUTINE test1
!
! !---------------------------------------------------------------------------
! !                                                                      test2
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test2
!     order = 2
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, &
!                       dom=dom, &
!                       ipType=quadopt%equidistance, &
!                       quadratureIsHomogeneous=math%yes, &
!                       quadratureType=[quadopt%GaussLegendre], &
!                       quadratureOrder=[2 * order], &
!                       quadratureIsOrder=math%yes)
!
!     CALL obj%SetFE(globalElement=1, islocal=math%yes)
!     feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
!     CALL feptr%GetQuadraturePoints(quad=quad)
!     CALL check_quad(6_I4B, 2.0_DFP, testname // " test2")
!   END SUBROUTINE test2
!
! !---------------------------------------------------------------------------
! !                                                                     test3
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test3
!     order = 3
!     CALL obj%Initiate(baseContinuity=baseContinuity, &
!                       baseInterpolation=baseInterpolation, &
!                       order=order, &
!                       dom=dom, &
!                       ipType=quadopt%equidistance, &
!                       quadratureIsHomogeneous=math%yes, &
!                       quadratureType=[quadopt%GaussLegendre], &
!                       quadratureOrder=[2 * order], &
!                       quadratureIsOrder=math%yes)
!
!     CALL obj%SetFE(globalElement=1, islocal=math%yes)
!     feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
!     CALL feptr%GetQuadraturePoints(quad=quad)
!     CALL check_quad(0_I4B, 2.0_DFP, testname // " test3")
!   END SUBROUTINE test3
!
! !---------------------------------------------------------------------------
! !                                                                check_quad
! !---------------------------------------------------------------------------
!
! ! Check the number of quadrature points (if nwant > 0) and the sum of the
! ! weights (the last row of quad%points), which is the area of the
! ! reference triangle.
!
!   SUBROUTINE check_quad(nwant, area, msg)
!     INTEGER(I4B), INTENT(IN) :: nwant
!     REAL(DFP), INTENT(IN) :: area
!     CHARACTER(*), INTENT(IN) :: msg
!     INTEGER(I4B) :: nips, nrow
!
!     nrow = SIZE(quad%points, 1)
!     nips = SIZE(quad%points, 2)
!     IF (nwant .GT. 0) CALL OK(nips .EQ. nwant, msg // " number of points")
!     CALL OK(ABS(SUM(quad%points(nrow, :)) - area) .LT. 1.0E-8_DFP, &
!             msg // " sum of weights")
!   END SUBROUTINE check_quad
!
! END PROGRAM main
!```
!
! See also: [[obj_SetFE]]

INTERFACE
  MODULE FUNCTION obj_GetFEPointer(obj, globalElement, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element, node, edge or face number is a local
    !! number; default is false
    CLASS(AbstractFE_), POINTER :: ans
    !! pointer to the finite element
  END FUNCTION obj_GetFEPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetSumElemTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Sum of the total DOFs of all active elements
!
! Returns the sum over all active elements of the number of degrees
! of freedom of the element (GetTotalDOF with globalElement). Shared
! DOFs are counted once per element, so the sum is the size of a
! fully discontinuous (DG) vector. The value is computed at the first
! call and cached in obj%sumElemTotalDOF; DEALLOCATE (and therefore
! Initiate) resets the cache.
!
! Arguments:
!   obj : CLASS(FEDOF_) - initiated FEDOF (the cache is set)
!
! Returns:
!   ans : INTEGER(I4B) - sum of the element DOFs
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BaseType, ONLY: quadopt => TypeQuadratureOpt
!   USE Display_Method, ONLY: ToString
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: telems
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!   meshptr => dom%GetMeshPointer()
!   telems = meshptr%GetTotalElements()
!
!   CALL test(order=1, nloc=3)
!   CALL test(order=2, nloc=6)
!   CALL test(order=3, nloc=10)
!
!   CALL obj%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! !---------------------------------------------------------------------------
! !                                                                       test
! !---------------------------------------------------------------------------
!
!   SUBROUTINE test(order, nloc)
!     INTEGER(I4B), INTENT(IN) :: order, nloc
!     INTEGER(I4B) :: found, want, iel
!     CHARACTER(:), ALLOCATABLE :: msg
!
!     msg = "order = "//ToString(order)//": "
!
!     CALL obj%Initiate(order=order, dom=dom, baseContinuity="H1", &
!                       baseInterpolation="Hierarchical", &
!                       quadratureIsHomogeneous=math%yes, &
!                       quadratureType=[quadopt%GaussLegendre], &
!                       quadratureOrder=[2 * order], &
!                       quadratureIsOrder=math%yes)
!
!     found = obj%GetSumElemTotalDOF()
!     CALL OK(found .EQ. nloc * telems, msg//"nloc * telems")
!
!     want = 0
!     DO iel = 1, telems
!       want = want + obj%GetTotalDOF(globalElement=iel, islocal=math%yes)
!     END DO
!     CALL OK(found .EQ. want, msg//"sum of GetTotalDOF")
!
!     found = obj%GetSumElemTotalDOF()
!     CALL OK(found .EQ. want, msg//"cached value")
!   END SUBROUTINE test
!
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalDOF2]], [[obj_GetMaxTotalConnectivity]]

INTERFACE
  MODULE FUNCTION obj_GetSumElemTotalDOF(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF; the cached sum is set at the first call
    INTEGER(I4B) :: ans
    !! sum of the total DOFs of all active elements
  END FUNCTION obj_GetSumElemTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalActiveCells@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Get total number of active cells in fedof.
!
! This method lazy compute the total number of active cells in
! mesh.

INTERFACE
  MODULE FUNCTION obj_GetTotalActiveCells(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF; the cached sum is set at the first call
    INTEGER(I4B) :: ans
    !! total number of active cells in fedof.
  END FUNCTION obj_GetTotalActiveCells
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-08
! summary: Initiate the FEDOF from a toml table
!
! Read order (a number, a list, or the name of a csv file with the global
! element number and the order of each element), islocal and scaleForQuadOrder
! (default 1) from the table. If order is absent, the order of each element is
! the order of its geometry. The finite elements read the other keys
! (baseContinuity, baseInterpolation, ipType, quadrature options) from the
! same table.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   table : TYPE(toml_table), intent(inout) - toml table of the FEDOF
!   dom : CLASS(AbstractDomain_), intent(in) - domain; the FEDOF uses its mesh
!     of dimension nsd
!
! See also: [[obj_ImportFromToml2]]

INTERFACE FEDOFImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table of the FEDOF
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain; the FEDOF uses its mesh of dimension nsd
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE FEDOFImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-08
! summary: Initiate the FEDOF from a toml file
!
! Read the table tomlName from the toml file (filename or afile) and call
! ImportFromToml with it. dom must be present.
!
! Arguments:
!   obj : CLASS(FEDOF_), intent(inout) - FEDOF object
!   tomlName : CHARACTER(*), intent(in) - name of the toml table in the file
!   afile : TYPE(TxtFile_), optional, intent(inout) - toml file (opened)
!   filename : CHARACTER(*), optional, intent(in) - name of the toml file
!   printToml : LOGICAL(LGT), optional, intent(in) - not used
!   dom : CLASS(AbstractDomain_), optional, intent(in) - domain; it must be
!     present
!
! Examples
!
!```fortran
! PROGRAM main
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Display_Method, ONLY: Display
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE Test_Method, ONLY: OK
!   USE Test_Method, ONLY: IS
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!
!   IMPLICIT NONE
!
!   TYPE(FEDOF_) :: obj
!   TYPE(FEDomain_) :: dom
!   CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
!   CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
!     tomlFileName = "./toml/ImportFromToml_test_1.toml"
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: found, want
!
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL e%setQuietMode(EXCEPTION_DEBUG, math%yes)
!
!   CALL meshfile%Initiate(filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL dom%Initiate(meshfile, '')
!
!   meshptr => dom%GetMeshPointer()
!
!   CALL obj%ImportFromToml( &
!                            tomlName="test1", filename=tomlFileName, dom=dom)
!
! !CALL fedof%Display("FEDOF:")
!   found = obj%GetTotalDOF()
!   want = meshptr%GetTotalNodes()
!   CALL IS(found, want, "Total DOF (order=1): ")
!
!   CALL obj%ImportFromToml( &
!                            tomlName="test2", filename=tomlFileName, dom=dom)
!   found = obj%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces() + &
!     0 * meshptr%GetTotalCells()
!   CALL IS(found, want, "Total DOF (order=2): ")
!
!   CALL obj%ImportFromToml( &
!                            tomlName="test3", filename=tomlFileName, dom=dom)
!
!   found = obj%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + 2 * meshptr%GetTotalFaces() + &
!     1 * meshptr%GetTotalCells()
!   CALL IS(found, want, "Total DOF (order=3): ")
!
!   CALL obj%ImportFromToml( &
!                            tomlName="test4", filename=tomlFileName, dom=dom)
!
!   found = obj%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + 3 * meshptr%GetTotalFaces() + &
!     3 * meshptr%GetTotalCells()
!   CALL OK(found == want, "Total DOF (order=4): ")
!
!   CALL obj%ImportFromToml(tomlName="test5", filename=tomlFileName, &
!                           dom=dom)
!
!   found = obj%GetTotalDOF()
!   want = meshptr%GetTotalNodes()
!   CALL IS(found, want, "Total DOF (order=1): ")
!
!   CALL obj%ImportFromToml(tomlName="test6", filename=tomlFileName, &
!                           dom=dom)
!   found = obj%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces() + &
!     0 * meshptr%GetTotalCells()
!   CALL IS(found, want, "Total DOF (order=2): ")
!
!   CALL obj%ImportFromToml(tomlName="test7", filename=tomlFileName, &
!                           dom=dom)
!   found = obj%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + 2 * meshptr%GetTotalFaces() + &
!     1 * meshptr%GetTotalCells()
!   CALL IS(found, want, "Total DOF (order=3): ")
!
!   CALL obj%ImportFromToml(tomlName="test8", filename=tomlFileName, &
!                           dom=dom)
!   found = obj%GetTotalDOF()
!   want = meshptr%GetTotalNodes() + 3 * meshptr%GetTotalFaces() + &
!     3 * meshptr%GetTotalCells()
!   CALL OK(found == want, "Total DOF (order=4): ")
!
! !CALL dom%Display("domain:")
!   CALL dom%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
!
! END PROGRAM main
!```
!
! See also: [[obj_ImportFromToml1]]

INTERFACE FEDOFImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml, dom)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the toml table in the file
    TYPE(TxtFile_), INTENT(INOUT), OPTIONAL :: afile
    !! toml file (opened)
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! name of the toml file
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: printToml
    !! not used
    CLASS(AbstractDomain_), INTENT(IN), OPTIONAL :: dom
    !! domain; it must be present
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE FEDOFImportFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEDOF_Class
