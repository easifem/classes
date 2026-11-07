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
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: LGT
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE
PRIVATE

PUBLIC :: FEDOF_
PUBLIC :: FEDOFPointer_
PUBLIC :: FEDOFSetSparsity

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
! - H1 numbering: vertex DOFs are 1:tNodes (mesh-local vertex numbers),
!   followed by edge-, face- and cell-interior blocks. Each block is ordered
!   by increasing mesh entity number, independently of cell traversal.
!   In 2D, line-interior DOFs belong to faces; the edge block is empty.
! - edgeIA, faceIA and cellIA are cumulative offsets, not CSR column
!   indices. Entity i owns IA(i):IA(i+1)-1. Equal adjacent offsets denote
!   an empty entity block. The last cell offset is tdof+1.
! - Broken spaces (isBrokenSpace true, baseContinuity H1): every cell owns
!   a disjoint block of DOFs (the complete local basis), so vertices, edges
!   and faces own no DOFs. The size of a block is the full basis size of
!   the element type at the cell order, including constant cells.
! - Orders: cellOrder(iel) is the order of local element iel. For H1, each
!   face or edge uses the minimum order of the cells around it. Hierarchical
!   H1 in 2D preserves these entity orders when configuring the shared FE.
! - fe(ii) is the finite element of element topology ii (FEFactory); SetFE
!   sets its orders and orientations for one element.
! - baseContinuity (H1, HCurl, HDiv) and baseInterpolation (LAGR, HIER,
!   ...) are stored in upper case.
!
! Changes:
!   - 2026-10-30: Define entity-ordered cumulative H1 DOF offsets.
!   - 2026-10-30: Preserve variable hierarchical H1 entity orders in 2D.
!   - 2026-11-02: broken spaces are set by isBrokenSpace (not by a DG
!     baseContinuity)
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
  !! It is set to true when we set tActiveCells, Lazy computing in
  !! GetTotalActiveCells method.
  LOGICAL(LGT) :: isBrokenSpace = math%no
  !! True for a broken space: all DOFs belong to cells (see
  !! SetFedofForH1Broken)
  LOGICAL(LGT) :: ownFePointers = math%no
  !! If the object initiates the AbstractFE_ pointer
  !! then ownFePointer is set to true, in this case
  !! fedof will deallocate AbstractFE_ pointers in Deallocate Method.
  INTEGER(I4B) :: tdof = math%zero_i
  !! Total number of degrees of freedom
  INTEGER(I4B) :: tNodes = math%zero_i
  !! Shared vertex DOF count: H1 mesh vertices, zero for a broken space
  INTEGER(I4B) :: tEdges = math%zero_i
  !! Total number of edges
  INTEGER(I4B) :: tFaces = math%zero_i
  !! Total number of faces
  INTEGER(I4B) :: tCells = math%zero_i
  !! Total number of cells
  INTEGER(I4B) :: maxCon = math%zero_i
  !! maximum number of connectivity
  INTEGER(I4B) :: sumElemTotalDOF = math%zero_i
  !! sum of total  degrees of freedom for each element.
  !! this is useful for DGM
  !! This is cached when we initiate the FEDOF
  INTEGER(I4B) :: tActiveCells = math%zero_i
  !! total number of active cells in FEDOF and mesh.
  INTEGER(I4B) :: maxQuadPoint = math%zero_i
  !! maximum number of quadrature points
  CHARACTER(2) :: baseContinuity = "H1"
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv (a broken space is H1 with isBrokenSpace true)
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
  !! Edge-interior DOF offsets, size tEdges+1; edge i owns
  !! edgeIA(i):edgeIA(i+1)-1. Starts at tNodes+1.
  INTEGER(I4B), ALLOCATABLE :: faceIA(:)
  !! Face-interior DOF offsets, size tFaces+1; face i owns
  !! faceIA(i):faceIA(i+1)-1. Starts at the last edge offset.
  INTEGER(I4B), ALLOCATABLE :: cellIA(:)
  !! Cell DOF offsets, size tCells+1; local cell i owns
  !! cellIA(i):cellIA(i+1)-1. Starts at the last face offset;
  !! cellIA(tCells+1) is tdof+1. Empty ranges have equal offsets.
  !! A broken space stores the full local basis here; H1 stores cell
  !! interiors.
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
  PROCEDURE, PRIVATE, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate a FEDOF from a table of element numbers and orders
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3
  !! Initiate the FEDOF
  PROCEDURE, PRIVATE, PASS(obj) :: AllocateSizes => obj_AllocateSizes
  !! Allocate the order and degree of freedom arrays
  PROCEDURE, PRIVATE, PASS(obj) :: Set => obj_Set
  !! Set degrees of freedom numbering and offsets edgeIA, faceIA, cellIA
  !!   based on conformity
  PROCEDURE, PRIVATE, PASS(obj) :: SetFedofForH1 => obj_SetFedofForH1
  !! Set degrees of freedom numbering and offsets edgeIA, faceIA, cellIA
  !!   for H1 conformity
  PROCEDURE, PRIVATE, PASS(obj) :: SetFedofForH1Broken => &
    obj_SetFedofForH1Broken
  !! Set degrees of freedom numbering and offsets edgeIA, faceIA, cellIA
  !!   for broken H1 spaces
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
  PROCEDURE, PUBLIC, PASS(obj) :: SetFE => obj_SetFE
  !! Configure the shared FE, preserving hierarchical H1 entity orders in 2D

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
  !! Get cell-interior H1 DOFs or the full cell block of a broken space
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
  !! Get element DOFs; a broken space returns only the owning-cell block
  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity_ => obj_GetConnectivity_
  !! Get element DOFs without allocation, using the first option letter
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetConnectivity_ => &
    obj_GetFacetConnectivity_
  !! Get the H1 DOFs of a facet of an element
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
  !! Get orders and orientations using global or local element numbering
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
    !! continuity of the basis: H1, HCurl or HDiv (the first two letters
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
! For conforming spaces, a face or edge uses the minimum order of its
! neighboring cells. Then the
! degrees of freedom are numbered: first one per vertex node (the local node
! number), then edge-, face- and cell-interior DOFs in increasing entity
! number within each block. Counts are collected before offsets are built.
! Finally one finite element is made for each element topology of the mesh
! (FEFactory), with the interpolation and quadrature options.
!
! For Lagrange basis ipType is required. Initiate deallocates obj first.
!
! This routine is for a homogeneous order: all cells get the order order. It
! calls the version with an order vector.
!
! With isBrokenSpace true the space is broken: every cell owns all its
! DOFs and there are no shared vertex, edge or face DOFs. Cell orders may
! be zero (see [[obj_SetFedofForH1Broken]]).
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!   - 2026-11-02: optional argument isBrokenSpace replaces the DG
!     baseContinuity
!
! Examples
!
! Dummy example; the complete program is Initiate_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_) :: dom
! ! ... initiate dom from a mesh file ...
! CALL obj%Initiate(order=1, dom=dom, baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   quadratureIsHomogeneous=math%yes, &
!   quadratureType=[quadopt%GaussLegendre], &
!   quadratureOrder=[2], &
!   quadratureIsOrder=math%yes)
!```
!
! See also: [[obj_Initiate2]], [[obj_Initiate3]], [[obj_ImportFromToml2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1( &
    obj, order, dom, baseContinuity, baseInterpolation, isBrokenSpace, &
    feType, ipType, basisType, alpha, beta, lambda, dofType, transformType, &
    quadratureIsHomogeneous, quadratureType, quadratureOrder, &
    quadratureIsOrder, quadratureNips, quadratureIsNips, quadratureAlpha, &
    quadratureBeta, quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: order
    !! order of all cells
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain; the FEDOF uses its mesh of dimension nsd
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis: H1, HCurl or HDiv (the first two letters
    !! are used)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation of the basis: Lagrange, Hierarchical, Orthogonal, Hermit
    !! or Serendipity (the first four letters are used)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isBrokenSpace
    !! If true, the space is broken: each cell owns all its DOFs and no
    !! vertex, edge or face DOFs are shared (default false)
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
! With isBrokenSpace true (broken space), cell orders may be zero and
! neighboring cells may have independent orders. The minimum-neighbor
! rule below applies to conforming spaces only. A broken space puts the
! complete local basis in
! cellIA, starting at DOF one; GetConnectivity with All or Cell returns
! that owning-cell block. Vertex, Edge and Face options return empty
! blocks. There is no shared entity order: edgeOrder and faceOrder are
! zero. The local FE is an H1 polynomial evaluator; conformity is imposed
! only by the FEDOF numbering, so a broken space shares no DOFs.
!
! Keep pointers to the domain and its mesh and assign supplied cell orders.
! For conforming spaces, edges and faces use minimum neighboring orders.
! Collect entity DOF counts before building cumulative numbering offsets:
! vertices, edges, faces, then cell interiors in entity-number order.
! Create one FE per topology. Call SetFE before evaluating a cell's FE.
!
! For hierarchical H1 triangles and quadrangles on linear 2D geometry,
! each shared edge uses the lower neighboring order, while each cell keeps
! its own interior order. The high-order cell uses a reduced trace space;
! it retains the higher-order modes on its other edges and in its interior.
! This is a conforming hierarchical space without additional constraints.
! This path does not implement constraints for variable-order Lagrange.
!
! order(ii) belongs to local cell ii when islocal is true, and to global
! element ii otherwise. Global-indexed arrays must cover the largest
! element ID; entries for absent elements are ignored. A one-entry array
! assigns the same order to every cell. Initiate deallocates obj first.
! For Lagrange interpolation, ipType is required.
!
! Changes:
!   - 2026-10-30: Verify minimum-order hierarchical H1 traces in 2D.
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!   - 2026-11-02: optional argument isBrokenSpace replaces the DG
!     baseContinuity
!
! Formula:
!   For neighboring cells K and L sharing edge e:
!   $$ p_e=\min(p_K,p_L), \qquad n_e=p_e-1. $$
!   Triangle interior counts are (p_K-1)(p_K-2)/2; quadrangle interior
!   counts are (p_K-1)^2. Each vertex has one DOF.
!
! Examples
!
! Dummy example; the complete program is H1VariableOrder_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: obj, geometry
! TYPE(FEDomain_), TARGET :: dom
! ! ... initiate dom from a mesh file ...
! CALL obj%Initiate(order=cellOrders, islocal=.TRUE., dom=dom, &
!   baseContinuity="H1", baseInterpolation="Hierarchical")
!```
!
! See also: [[obj_SetFE]], [[obj_GetConnectivity]], [[obj_Initiate1]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2( &
    obj, order, dom, baseContinuity, baseInterpolation, isBrokenSpace, &
    feType, ipType, basisType, alpha, lambda, beta, islocal, dofType, &
    transformType, quadratureIsHomogeneous, quadratureType, &
    quadratureOrder, quadratureIsOrder, quadratureNips, quadratureIsNips, &
    quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order of each cell, see islocal
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain; the FEDOF uses its mesh of dimension nsd
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis: H1, HCurl or HDiv (the first two letters
    !! are used)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation of the basis: Lagrange, Hierarchical, Orthogonal, Hermit
    !! or Serendipity (the first four letters are used)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isBrokenSpace
    !! If true, the space is broken: each cell owns all its DOFs and no
    !! vertex, edge or face DOFs are shared (default false)
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
! For conforming spaces, a face or edge uses the minimum order of its
! neighboring cells. Then the
! degrees of freedom are numbered: first one per vertex node (the local node
! number), then edge-, face- and cell-interior DOFs in increasing entity
! number within each block. Counts are collected before offsets are built.
! Finally one finite element is made for each element topology of the mesh
! (FEFactory), with the interpolation and quadrature options.
!
! For Lagrange basis ipType is required. Initiate deallocates obj first.
!
! order(1, ii) is a global element number and order(2, ii) its order; there is
! one column for each element. The routine makes the order of each local
! element and calls the version with an order vector.
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
  MODULE SUBROUTINE obj_Initiate3( &
    obj, order, dom, baseContinuity, baseInterpolation, isBrokenSpace, &
    feType, ipType, basisType, alpha, beta, lambda, dofType, transformType, &
    quadratureIsHomogeneous, quadratureType, quadratureOrder, &
    quadratureIsOrder, quadratureNips, quadratureIsNips, quadratureAlpha, &
    quadratureBeta, quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: order(:, :)
    !! order(1, :) global element numbers, order(2, :) their orders; one
    !! column per element
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain; the FEDOF uses its mesh of dimension nsd
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis: H1, HCurl or HDiv (the first two letters
    !! are used)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isBrokenSpace
    !! If true, the space is broken: each cell owns all its DOFs and no
    !! vertex, edge or face DOFs are shared (default false)
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
  END SUBROUTINE obj_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                           AllocateSizes@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary: Allocate the order and degree of freedom arrays
!
! In a broken space (isBrokenSpace), tNodes counts shared vertex DOFs
! and is zero. Mesh vertex, edge
! and face topology remains available through the mesh; the offset arrays
! still have one more entry than their mesh entity counts.
!
! For a 3D mesh, first call InitiateEdgeConnectivity of the mesh (it
! returns at once when the edges are numbered already), because the mesh
! does not number the edges by itself.
! Read the number of vertex nodes, edges, faces and cells of the mesh and
! allocate cellOrder, faceOrder, edgeOrder and the index arrays edgeIA, faceIA
! and cellIA (one entry more than the number of entities).
!
! Changes:
!   - 2026-10-29: initiate the edge connectivity of a 3D mesh
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! See also: [[obj_Set]]

INTERFACE
  MODULE SUBROUTINE obj_AllocateSizes(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_AllocateSizes
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Set@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary: Dispatch DOF numbering according to the continuity of the space
!
! Check active-cell data pointers in DEBUG mode, then delegate numbering
! to SetFedofForH1Broken when isBrokenSpace is true, and to SetFedofForH1
! otherwise. The helpers build offsets and reset layout caches.
!
! For a broken space, skip shared entities and count the full cell basis
! of the element type at the cell order, for example (p+1)(p+2)/2 on
! triangles and (p+1)^2 on quadrangles. Cell
! blocks start at one and follow local cell order. Inactive cells have empty
! ranges. The vertex, edge and face DOF blocks are empty.
!
! For H1, first visit active cells and store the DOF count of each edge,
! face and cell once, using its order, continuity and interpolation.
! Entities not incident to an active cell have zero interior DOFs.
! Then accumulate counts in increasing entity-number order. Offset arrays
! start after the vertex block, and each entity owns the half-open range
! from IA(i) to IA(i+1). Empty blocks have equal offsets.
!
! For H1 on linear 2D geometry, the edge block is empty and each face
! (a line) has p-1 interior DOFs. A triangular cell has
! (p-1)(p-2)/2 interior DOFs, and a quadrangle has (p-1)^2.
! For H1, all mesh vertices retain a vertex DOF. This does not compact
! vertices when cells are inactive. It resets the layout-dependent caches.
!
! Changes:
!   - 2026-10-30: Separate entity counting from cumulative numbering.
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!   - 2026-10-30: Rename to Set and delegate H1 and DG numbering to helpers.
!   - 2026-11-02: the broken numbering is chosen by isBrokenSpace
!
! Formula:
!   For entity counts n_i and the first DOF b of a block:
!   $$ I_1=b, \qquad I_{i+1}=I_i+n_i. $$
!   In H1, blocks start at N_v+1, then the last edge and face offsets.
!   A broken space has N_v=0 shared vertex DOFs, so its cell block
!   starts at one.
!   The total DOF count is the last cell offset minus one.
!
! Examples
!
! Dummy example; the complete program is H1Numbering_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_), TARGET :: dom
! ! ... initiate dom from a mesh file ...
! CALL obj%Initiate(order=2, dom=dom, baseContinuity="H1", &
!   baseInterpolation="LAGR", &
!   ipType=TypeInterpolationOpt%Equidistance)
!```
!
! See also: [[obj_AllocateSizes]], [[obj_Initiate1]],
! [[obj_SetFedofForH1Broken]], [[obj_SetFedofForH1]]

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                           SetFedofForH1@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Set edgeIA, faceIA, cellIA for H1 conformity
!
! Count each active edge, face and cell interior once. Build cumulative
! offsets in entity-number order after the shared vertex DOFs. Entities
! without active incident cells have empty ranges. Set tdof from the final
! cell offset and invalidate the layout-dependent caches.
!
! Called by [[obj_Set]] after storage and approximation orders are set.
!
! See also: [[obj_SetFedofForH1Broken]], [[obj_AllocateSizes]]
!
INTERFACE
  MODULE SUBROUTINE obj_SetFedofForH1(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_SetFedofForH1
END INTERFACE

!----------------------------------------------------------------------------
!                                     SetFedofForH1Broken@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Set edgeIA, faceIA, cellIA for H1 broken spaces
!
! Count the complete polynomial basis of each active cell using its element
! type, cell order and interpolation family. Build disjoint cell blocks in
! local cell order, starting at one. Inactive cells have empty ranges.
! Every edge and face offset is one, representing empty shared DOF blocks.
! Set tdof from the final cell offset and invalidate layout caches.
!
! Called by [[obj_Set]] when isBrokenSpace is true, after storage and cell
! orders are set. AllocateSizes sets tNodes to zero for a broken space;
! mesh vertices do not own shared DOFs.
!
! See also: [[obj_SetFedofForH1]], [[obj_AllocateSizes]]
!
INTERFACE
  MODULE SUBROUTINE obj_SetFedofForH1Broken(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_SetFedofForH1Broken
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
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
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
! Returns true if the FEDOF is initiated, and false otherwise.
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
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
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
! In a broken space (isBrokenSpace), All and Cell return the complete,
! contiguous owning-cell block. Vertex, Edge and Face return empty arrays
! because these mesh entities own no shared DOFs. Local basis rows use
! this same cell ordering.
!
! Return the degree of freedom numbers of an element. Vertex, Edge, Face
! or Cell selects that entity; All returns vertex, edge, face and cell
! DOFs in this order. The first character of opt is used, in either case.
! Global element identifiers and local element numbers are both supported.
!
! Changes:
!   - 2026-10-30: Accept long connectivity options consistently.
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Returns:
!   ans(:): Allocated array of the selected element DOF numbers.
!
! Examples
!
! Dummy example; the complete program is GetConnectivity_test_5.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_) :: dom
! INTEGER(I4B), ALLOCATABLE :: con(:)
! ! ... initiate dom from a mesh file of triangles ...
! CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
!                   isBrokenSpace=math%yes, order=2, dom=dom)
! con = obj%GetConnectivity(opt="All", globalElement=1, islocal=math%yes)
! ! con is the contiguous block of the 6 DOFs owned by cell 1
!```
!
! See also: [[obj_GetConnectivity_]], [[obj_GetTotalDOF3]]

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
    !! if true, globalElement is a mesh-local element number; default false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! degree of freedom numbers of the element
  END FUNCTION obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetConnectivity_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get element degrees of freedom without allocation
!
! In a broken space, All and Cell write the complete owning-cell block.
! Vertex, Edge and Face write nothing and set tsize to zero. A broken
! example is in GetConnectivity; the H1 example below uses shared
! entities.
!
! Write the same DOFs as GetConnectivity into ans, using only the first
! character of opt. The caller provides at least GetTotalDOF for the same
! element and option entries; only ans(1:tsize) is written.
!
! Changes:
!   - 2026-10-30: Accept long connectivity options consistently.
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MSHFile_Class, ONLY: MSHFile_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_), TARGET :: dom
! TYPE(HDF5File_) :: meshfile
! TYPE(MSHFile_) :: SOURCE
! CLASS(AbstractMesh_), POINTER :: mesh => NULL()
! CHARACTER(12), PARAMETER :: families(2) = &
!                             [CHARACTER(12) :: "Lagrange", "Hierarchical"]
! INTEGER(I4B), PARAMETER :: elements(2) = [101_I4B, 205_I4B]
! INTEGER(I4B) :: order, family, iel
! INTEGER :: unitno
! CHARACTER(6), PARAMETER :: options(5) = &
!                            [CHARACTER(6) :: "Vertex", "Edge", "Face", &
!                            "Cell", "All"]
! INTEGER(I4B) :: shortCon(64), longCon(64), nshort, nlong, ii, jj, SHARED
! INTEGER(I4B), ALLOCATABLE :: first(:), second(:), allocatedCon(:)
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
! CALL SOURCE%Initiate(filename="./fixtures/h1_two_quads.msh.txt", &
!                      STATUS="OLD", ACTION="READ")
! CALL SOURCE%OPEN()
! CALL SOURCE%READ()
! CALL meshfile%Initiate(filename="H1Connectivity_test_1.h5", mode="NEW")
! CALL meshfile%SetOverwriteStat(.TRUE.)
! CALL meshfile%OPEN()
! CALL SOURCE%Export(hdf5=meshfile, group="")
! CALL SOURCE%DEALLOCATE()
! CALL dom%Initiate(meshfile, "")
! mesh => dom%GetMeshPointer()
! CALL OK(mesh%GetTotalElements() .EQ. 2_I4B, "Fixture has two cells")
! CALL OK(mesh%GetTotalVertexNodes() .EQ. 6_I4B, "Fixture has six vertices")
! CALL OK(mesh%GetMaxNodeNumber() .GT. 6_I4B, "Mesh vertex IDs have gaps")
!
! DO family = 1, SIZE(families)
!   DO order = 1, 3
!     CALL obj%Initiate(order=order, dom=dom, baseContinuity="H1", &
!                       baseInterpolation=TRIM(families(family)), &
!                       ipType=TypeInterpolationOpt%Equidistance)
!     CALL OK(obj%GetTotalDOF() .EQ. &
!             6_I4B + 7_I4B * (order - 1_I4B) + &
!             2_I4B * (order - 1_I4B)**2, "Uniform H1 global DOF count")
!     DO iel = 1, SIZE(elements)
!       DO ii = 1, SIZE(options)
!         CALL obj%GetConnectivity_(ans=shortCon, tsize=nshort, &
!                                   opt=options(ii) (1:1), &
!                                   globalElement=elements(iel))
!         CALL obj%GetConnectivity_(ans=longCon, tsize=nlong, &
!                                   opt=TRIM(options(ii)), &
!                                   globalElement=elements(iel))
!         CALL OK(nlong .EQ. nshort, "Long and short option sizes agree")
!         IF (nlong .EQ. nshort) CALL OK( &
!           ALL(longCon(1:nlong) .EQ. shortCon(1:nshort)), &
!           "Long and short option contents agree")
!         allocatedCon = obj%GetConnectivity(opt=TRIM(options(ii)), &
!                                            globalElement=elements(iel))
!         CALL OK(SIZE(allocatedCon) .EQ. nshort, "Allocated output size")
!         IF (SIZE(allocatedCon) .EQ. nshort) CALL OK( &
!           ALL(allocatedCon .EQ. shortCon(1:nshort)), "Allocated output IDs")
!       END DO
!       CALL OK(obj%GetTotalDOF(globalElement=elements(iel)) .EQ. &
!               (order + 1_I4B)**2, "Uniform quadrangle local basis size")
!     END DO
!     first = obj%GetConnectivity(opt="All", globalElement=elements(1))
!     second = obj%GetConnectivity(opt="All", globalElement=elements(2))
!     SHARED = 0
!     DO jj = 1, SIZE(first)
!       IF (ANY(second .EQ. first(jj))) SHARED = SHARED + 1
!     END DO
!     CALL OK(SHARED .EQ. order + 1_I4B, "Shared H1 trace DOFs counted once")
!   END DO
! END DO
!
! CALL obj%DEALLOCATE()
! mesh => NULL()
! CALL dom%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
! OPEN (NEWUNIT=unitno, FILE="H1Connectivity_test_1.h5", &
!       STATUS="old", ACTION="read")
! CLOSE (unitno, STATUS="delete")
! END PROGRAM main
!```
!
! See also: [[obj_GetConnectivity]], [[obj_GetOrders]]

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity_(obj, ans, tsize, opt, &
                                         globalElement, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! degree of freedom numbers; capacity at least tsize; may be empty
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    CHARACTER(*), INTENT(IN) :: opt
    !! which degrees of freedom: Vertex, Edge, Face, Cell or All (the first
    !! letter is used)
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, globalElement is a mesh-local element number; default false
  END SUBROUTINE obj_GetConnectivity_
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get the degrees of freedom of a facet
!
! @note
!   Broken spaces are not handled yet: the code still checks for a DG
!   baseContinuity (the owning-cell block is returned only then).
! @endnote
!
! For H1 on two-dimensional linear geometry, write the facet's
! vertex DOF numbers followed by its face-interior DOF numbers. Vertex
! DOFs use mesh-local node numbers, consistently with GetConnectivity;
! mesh-global node identifiers are converted before returning them.
! The example checks uniform H1 spaces of orders one through three.
!
! Changes:
!   - 2026-10-30: Convert facet mesh node identifiers to vertex DOF numbers.
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MSHFile_Class, ONLY: MSHFile_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_), TARGET :: dom
! TYPE(HDF5File_) :: meshfile
! TYPE(MSHFile_) :: SOURCE
! CLASS(AbstractMesh_), POINTER :: mesh => NULL()
! CHARACTER(12), PARAMETER :: families(2) = &
!                             [CHARACTER(12) :: "Lagrange", "Hierarchical"]
! INTEGER(I4B), PARAMETER :: elements(2) = [101_I4B, 205_I4B]
! INTEGER(I4B) :: order, family, iel
! INTEGER :: unitno
! INTEGER(I4B) :: actual(16), want(16), nodes(8), nactual, nnode, nface
! INTEGER(I4B) :: iface, left(16), right(16), nleft, nright, ii, SHARED
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
! CALL SOURCE%Initiate(filename="./fixtures/h1_two_quads.msh.txt", &
!                      STATUS="OLD", ACTION="READ")
! CALL SOURCE%OPEN()
! CALL SOURCE%READ()
! CALL meshfile%Initiate(filename="H1FacetConnectivity_test_1.h5", mode="NEW")
! CALL meshfile%SetOverwriteStat(.TRUE.)
! CALL meshfile%OPEN()
! CALL SOURCE%Export(hdf5=meshfile, group="")
! CALL SOURCE%DEALLOCATE()
! CALL dom%Initiate(meshfile, "")
! mesh => dom%GetMeshPointer()
! CALL OK(mesh%GetTotalElements() .EQ. 2_I4B, "Fixture has two cells")
! CALL OK(mesh%GetTotalVertexNodes() .EQ. 6_I4B, "Fixture has six vertices")
! CALL OK(mesh%GetMaxNodeNumber() .GT. 6_I4B, "Mesh vertex IDs have gaps")
!
! DO family = 1, SIZE(families)
!   DO order = 1, 3
!     CALL obj%Initiate(order=order, dom=dom, baseContinuity="H1", &
!                       baseInterpolation=TRIM(families(family)), &
!                       ipType=TypeInterpolationOpt%Equidistance)
!     DO iel = 1, SIZE(elements)
!       DO iface = 1, 4
!         CALL mesh%GetFacetConnectivity_(ans=nodes, tsize=nnode, &
!                                         globalElement=elements(iel), &
!                                         iface=iface)
!         want(1:nnode) = mesh%GetLocalNodeNumber(globalNode=nodes(1:nnode))
!         CALL obj%GetFaceDOF(globalElement=elements(iel), &
!                             localFaceNumber=iface, ans=want(nnode + 1:), &
!                             tsize=nface)
!         CALL obj%GetFacetConnectivity_(globalElement=elements(iel), &
!                                        localFaceNumber=iface, ans=actual, &
!                                        tsize=nactual)
!         CALL OK(nactual .EQ. order + 1_I4B, "H1 facet connectivity size")
!         CALL OK(nactual .EQ. nnode + nface, "Facet block sizes agree")
!         IF (nactual .EQ. nnode + nface) CALL OK( &
!           ALL(actual(1:nactual) .EQ. want(1:nactual)), "Facet DOF IDs")
!       END DO
!     END DO
!     CALL obj%GetFacetConnectivity_(globalElement=elements(1), &
!                                    localFaceNumber=2_I4B, ans=left, &
!                                    tsize=nleft)
!     CALL obj%GetFacetConnectivity_(globalElement=elements(2), &
!                                    localFaceNumber=4_I4B, ans=right, &
!                                    tsize=nright)
!     SHARED = 0
!     DO ii = 1, nleft
!       IF (ANY(right(1:nright) .EQ. left(ii))) SHARED = SHARED + 1
!     END DO
!     CALL OK(SHARED .EQ. order + 1_I4B, &
!             "Both cells share the same facet DOFs")
!   END DO
! END DO
!
! CALL obj%DEALLOCATE()
! mesh => NULL()
! CALL dom%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
! OPEN (NEWUNIT=unitno, FILE="H1FacetConnectivity_test_1.h5", &
!       STATUS="old", ACTION="read")
! CLOSE (unitno, STATUS="delete")
! END PROGRAM main
!```
!
! See also: [[obj_GetConnectivity]], [[obj_GetOrders]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetConnectivity_(obj, ans, tsize, &
                                              globalElement, &
                                              localFaceNumber, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! H1 vertex and face-interior DOFs;
    !! only ans(1:tsize) is written (two-dimensional linear geometry)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, globalElement is a mesh-local element number; default false
  END SUBROUTINE obj_GetFacetConnectivity_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary: Get the degree of freedom of a vertex node
!
! In a broken space (isBrokenSpace), a mesh vertex owns no DOF: tsize is
! zero and ans is left unchanged. Vertex-position nodal values, when
! present, belong to the owning cell block instead.
!
! In H1, a vertex has one degree of freedom: its mesh-local node number.
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Examples
!
! Dummy example; the complete program is GetVertexDOF_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_) :: dom
! INTEGER(I4B) :: ans(10), tsize
! ! ... initiate dom from a mesh file ...
! CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
!                   isBrokenSpace=math%no, order=1, dom=dom)
! CALL obj%GetVertexDOF(globalNode=1, islocal=math%yes, ans=ans, &
!                       tsize=tsize)
! ! ans(1) is the local node number of node 1, tsize = 1
! CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
!                   isBrokenSpace=math%yes, order=1, dom=dom)
! CALL obj%GetVertexDOF(globalNode=1, islocal=math%yes, ans=ans, &
!                       tsize=tsize)
! ! tsize = 0: a vertex owns no DOF in a broken space
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
    !! degree of freedom numbers; capacity at least tsize; may be empty
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! entries written: one for H1 vertices, zero for a broken space
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
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = &
!                            "./meshdata/small_hexa8_mesh.h5"
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_) :: dom
! TYPE(HDF5File_) :: meshfile
! INTEGER(I4B) :: ans(10), tsize, iedge, order
!
! CALL e%SetQuietMode(math%yes)
!
! CALL meshfile%Initiate(filename, mode="READ")
! CALL meshfile%OPEN()
! CALL dom%Initiate(meshfile, "")
!
! ! Continuous case
! DO order = 2, 3
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     isBrokenSpace=math%no, order=order, dom=dom)
!
!   IF (order .EQ. 2) CALL OK(obj%GetTotalDOF() .EQ. 45_I4B, &
!                             "GetTotalDOF for order 2")
!
!   DO iedge = 1, 12
!     CALL OK(obj%GetTotalEdgeDOF(globalElement=1, localEdgeNumber=iedge, &
!                                 islocal=math%yes) .EQ. order - 1, &
!             "GetTotalEdgeDOF")
!   END DO
!
!   CALL obj%GetEdgeDOF(globalElement=1, localEdgeNumber=1, ans=ans, &
!                       tsize=tsize, islocal=math%yes)
!   CALL OK(tsize .EQ. order - 1, "GetEdgeDOF: number of entries")
!   CALL OK(ALL(ans(1:tsize) .GT. 12), &
!           "GetEdgeDOF: edge DOF follow the vertex DOF")
! END DO
!
! ! BrokenSpaces
! DO order = 2, 3
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     isBrokenSpace=math%yes, order=order, dom=dom)
!
!   DO iedge = 1, 12
!     ans(1) = obj%GetTotalEdgeDOF(globalElement=1, localEdgeNumber=iedge, &
!                                  islocal=math%yes)
!     CALL OK(ans(1) .EQ. math%zero_i, "GetTotalEdgeDOF for BrokenSpaces")
!   END DO
!
!   CALL obj%GetEdgeDOF(globalElement=1, localEdgeNumber=1, ans=ans, &
!                       tsize=tsize, islocal=math%yes)
!   CALL OK(tsize .EQ. 0, &
!           "GetEdgeDOF for BrokenSpaces: zero number of entries")
! END DO
!
! CALL obj%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
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
    !! degree of freedom numbers; capacity at least tsize; may be empty
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
    !! degree of freedom numbers; capacity at least tsize; may be empty
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
! Returns the number of degrees of freedom on the edge.
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom on the edge
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = &
!                            "./meshdata/small_hexa8_mesh.h5"
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_) :: dom
! TYPE(HDF5File_) :: meshfile
! INTEGER(I4B) :: ans(10), tsize, iedge, order
!
! CALL e%SetQuietMode(math%yes)
!
! CALL meshfile%Initiate(filename, mode="READ")
! CALL meshfile%OPEN()
! CALL dom%Initiate(meshfile, "")
!
! ! Continuous case
! DO order = 2, 3
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     isBrokenSpace=math%no, order=order, dom=dom)
!
!   IF (order .EQ. 2) CALL OK(obj%GetTotalDOF() .EQ. 45_I4B, &
!                             "GetTotalDOF for order 2")
!
!   DO iedge = 1, 12
!     CALL OK(obj%GetTotalEdgeDOF(globalElement=1, localEdgeNumber=iedge, &
!                                 islocal=math%yes) .EQ. order - 1, &
!             "GetTotalEdgeDOF")
!   END DO
!
!   CALL obj%GetEdgeDOF(globalElement=1, localEdgeNumber=1, ans=ans, &
!                       tsize=tsize, islocal=math%yes)
!   CALL OK(tsize .EQ. order - 1, "GetEdgeDOF: number of entries")
!   CALL OK(ALL(ans(1:tsize) .GT. 12), &
!           "GetEdgeDOF: edge DOF follow the vertex DOF")
! END DO
!
! ! BrokenSpaces
! DO order = 2, 3
!   CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
!                     isBrokenSpace=math%yes, order=order, dom=dom)
!
!   DO iedge = 1, 12
!     ans(1) = obj%GetTotalEdgeDOF(globalElement=1, localEdgeNumber=iedge, &
!                                  islocal=math%yes)
!     CALL OK(ans(1) .EQ. math%zero_i, "GetTotalEdgeDOF for BrokenSpaces")
!   END DO
!
!   CALL obj%GetEdgeDOF(globalElement=1, localEdgeNumber=1, ans=ans, &
!                       tsize=tsize, islocal=math%yes)
!   CALL OK(tsize .EQ. 0, &
!           "GetEdgeDOF for BrokenSpaces: zero number of entries")
! END DO
!
! CALL obj%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
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
! Returns the number of degrees of freedom on the edge.
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
! Examples
!
! Dummy example; the complete program is GetFaceDOF_test_2.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: fedof
! TYPE(FEDomain_) :: dom
! ! ... initiate dom from a mesh file ...
! CALL fedof%Initiate(baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   order=2, dom=dom)
! CALL fedof%GetFaceDOF(globalFace=1, islocal=math%yes, &
!   ans=found, tsize=tsize)
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
    !! degree of freedom numbers; capacity at least tsize; may be empty
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
    !! degree of freedom numbers; capacity at least tsize; may be empty
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
! In a broken space, the range is the complete local polynomial basis of the
! cell, including its vertex-position and boundary-position nodal values; it
! is not restricted to cell-interior shape functions.
!
! Write the degree of freedom numbers inside a cell, cellIA(iel) to cellIA(iel
! + 1) - 1, where iel is the local element number.
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Examples
!
! Dummy example; the complete program is GetCellDOF_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: fedof
! TYPE(FEDomain_) :: dom
! ! ... initiate dom from a mesh file ...
! CALL fedof%Initiate(baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   order=2, dom=dom)
! CALL fedof%GetCellDOF(globalCell=1, islocal=math%yes, &
!   ans=found, tsize=tsize)
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
    !! degree of freedom numbers; capacity at least tsize; may be empty
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
! In a broken space, count the complete owning-cell polynomial basis rather
! than only the interior modes. Degree-zero cells have one DOF.
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom inside the cell
!
! Examples
!
! Dummy example; the complete program is GetTotalCellDOF_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: fedof
! TYPE(FEDomain_) :: dom
! ! ... initiate dom from a mesh file ...
! CALL fedof%Initiate(baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   order=2, dom=dom)
! found = fedof%GetTotalCellDOF(globalCell=1, islocal=math%yes)
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
! Returns the number of degrees of freedom on the face.
!
! Returns:
!   ans : INTEGER(I4B) - number of degrees of freedom on the face
!
! Examples
!
! Dummy example; the complete program is GetTotalFaceDOF_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: fedof
! TYPE(FEDomain_) :: dom
! ! ... initiate dom from a mesh file ...
! CALL fedof%Initiate(baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   order=2, dom=dom)
! found = fedof%GetTotalFaceDOF(globalFace=1, islocal=math%yes)
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
! Returns the number of degrees of freedom on the face.
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
! A broken space returns zero: all local nodal values belong to cell
! blocks, even
! when their interpolation points coincide with mesh vertices.
!
! This is the number of vertex nodes of the mesh.
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
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
! For BrokenSpaces, this is the sum of the active cell basis sizes.
! No shared mesh vertex, edge or face DOFs are added.
! See [[GetConnectivity]] for a
! complete broken-space numbering and basis-evaluation example.
!
! GetTotalDOF is a generic method: without arguments it returns the number of
! degrees of freedom of the FEDOF; with globalElement, the number of degrees
! of freedom of that element; with globalElement and opt, only those of the
! vertices, edges, faces or cell (opt V, E, F or C) or all (A).
!
! Changes:
!
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Returns:
!
!   ans : INTEGER(I4B) - total number of degrees of freedom
!
! Examples:
!
! Dummy example; the complete program is GetTotalDOF_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: fedof
! TYPE(FEDomain_) :: dom
! ! ... initiate dom from a mesh file ...
! CALL fedof%Initiate(baseContinuity=baseContinuity, &
!   baseInterpolation=baseInterpolation, &
!   order=order, dom=dom)
! found = fedof%GetTotalDOF()
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
! In a broken space, return the length of the owning-cell block, with no
! additional shared-vertex contribution.
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
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
! In a broken space, All and Cell return the owning-cell basis size; Vertex,
! Edge and Face return zero.
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
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
! Call SetCellOrder first. In a broken space (isBrokenSpace) a face owns no
! DOFs, so all face orders and the largest face order are zero.
!
! Changes:
!   - 2026-11-02: in a broken space all face orders are zero
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
! SetCellOrder first. In a broken space (isBrokenSpace) an edge owns no
! DOFs, so all edge orders and the largest edge order are zero.
!
! Changes:
!   - 2026-11-02: in a broken space all edge orders are zero
!
! See also: [[obj_SetCellOrder]]

INTERFACE
  MODULE SUBROUTINE obj_SetEdgeOrder(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
  END SUBROUTINE obj_SetEdgeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetFE@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-22
! summary: Configure the shared finite element for one mesh element
!
! In a broken space, use the complete local Lagrange basis at the cell order,
! including degree zero. Disable shared-edge orientation corrections so
! the nodal ordering remains local to the cell. The quadrature order is
! at least one even for constant cells. The returned FE reports its local
! H1 basis implementation; the FEDOF itself stays a broken space.
!
! Set the cell, face and edge orders and orientations in the finite element
! of the requested topology. Set its quadrature order to scaleForQuadOrder
! times the cell order. Both global and local element IDs are accepted.
!
! Call SetFE before evaluating the object returned by GetFEPointer.
! Elements of one topology share that object: consume its shape data
! before calling SetFE for another element of that topology.
!
! For hierarchical H1 triangles and quadrangles, pass cell and face orders
! without the scalar-order shortcut. Shared-face orders remain the minimum
! of neighboring cell orders, including when the cell has higher order.
! Variable-order Lagrange constraints are not provided by this path.
!
! For uniform H1 triangles and quadrangles, connectivity stays in canonical
! global entity order. Orientation is applied by the basis evaluation:
! Lagrange permutes edge-interior rows during physical mapping, whereas
! hierarchical evaluation applies modal orientation factors locally.
! Do not reverse connectivity as an additional orientation correction.
! Geometry and solution shape data must use the same reference domain;
! the example uses an order-one geometry basis from the same family.
!
! Changes:
!   - 2026-10-30: Verify reversed-edge H1 traces through the shared FE.
!   - 2026-10-30: Preserve per-face orders for hierarchical H1 in 2D.
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Examples
!
! Dummy example; the complete program is H1Orientation_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: obj, geometry
! TYPE(FEDomain_), TARGET :: dom
! ! ... initiate dom from a mesh file ...
! CALL obj%Initiate(order=2, dom=dom, baseContinuity="H1", &
!   baseInterpolation="LAGR", &
!   ipType=TypeInterpolationOpt%Equidistance)
! CALL obj%SetFE(globalElement=elements(iel))
!```
!
! See also: [[obj_GetFEPointer]], [[obj_GetOrders]]

INTERFACE
  MODULE SUBROUTINE obj_SetFE(obj, globalElement, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, globalElement is a mesh-local element number; default false
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
! Returns the pointer to the domain.
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
! Examples
!
! Dummy example; the complete program is GetCellOrder_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: fedof
! TYPE(FEDomain_) :: dom
! ! ... initiate dom from a mesh file ...
! CALL fedof%Initiate(baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   order=2, dom=dom)
! CALL fedof%GetCellOrder(globalElement=1, islocal=math%yes, &
!   ans=found, tsize=tsize)
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
    !! nrow - number of rows written in ans (3)
    !! ncol - number of columns written in ans (number of faces)
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
! summary: Get element orders and orientations
!
! @note
!   Broken spaces are not handled yet: the code still checks for a DG
!   baseContinuity, which no longer exists. It should return the cell
!   order for every local face and edge of a broken space.
! @endnote
!
! Get the cell, face and edge polynomial orders and their mesh
! orientations. SetFE uses this method. Convert the supplied element
! identifier once and use the same local element for orders and
! orientations; both global and local element numbering are supported.
!
! Changes:
!   - 2026-10-30: Use the converted local element for orientation lookup.
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MSHFile_Class, ONLY: MSHFile_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_), TARGET :: dom
! TYPE(HDF5File_) :: meshfile
! TYPE(MSHFile_) :: SOURCE
! CLASS(AbstractMesh_), POINTER :: mesh => NULL()
! CHARACTER(12), PARAMETER :: families(2) = &
!                             [CHARACTER(12) :: "Lagrange", "Hierarchical"]
! INTEGER(I4B), PARAMETER :: elements(2) = [101_I4B, 205_I4B]
! INTEGER(I4B) :: order, family, iel
! INTEGER :: unitno
! INTEGER(I4B) :: cellOrder(3), faceOrder(3, 6), edgeOrder(12)
! INTEGER(I4B) :: cellOrient(3), faceOrient(3, 6), edgeOrient(12), sizes(7)
! INTEGER(I4B) :: cellOrderLocal(3), faceOrderLocal(3, 6), edgeOrderLocal(12)
! INTEGER(I4B) :: cellOrientLocal(3), faceOrientLocal(3, 6)
! INTEGER(I4B) :: edgeOrientLocal(12), localSizes(7)
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
! CALL SOURCE%Initiate(filename="./fixtures/h1_two_quads.msh.txt", &
!                      STATUS="OLD", ACTION="READ")
! CALL SOURCE%OPEN()
! CALL SOURCE%READ()
! CALL meshfile%Initiate(filename="H1Orders_test_1.h5", mode="NEW")
! CALL meshfile%SetOverwriteStat(.TRUE.)
! CALL meshfile%OPEN()
! CALL SOURCE%Export(hdf5=meshfile, group="")
! CALL SOURCE%DEALLOCATE()
! CALL dom%Initiate(meshfile, "")
! mesh => dom%GetMeshPointer()
! CALL OK(mesh%GetTotalElements() .EQ. 2_I4B, "Fixture has two cells")
! CALL OK(mesh%GetTotalVertexNodes() .EQ. 6_I4B, "Fixture has six vertices")
! CALL OK(mesh%GetMaxNodeNumber() .GT. 6_I4B, "Mesh vertex IDs have gaps")
!
! DO family = 1, SIZE(families)
!   DO order = 1, 3
!     CALL obj%Initiate(order=order, dom=dom, baseContinuity="H1", &
!                       baseInterpolation=TRIM(families(family)), &
!                       ipType=TypeInterpolationOpt%Equidistance)
!     DO iel = 1, SIZE(elements)
!       CALL obj%GetOrders( &
!         cellOrder=cellOrderLocal, faceOrder=faceOrderLocal, &
!         edgeOrder=edgeOrderLocal, cellOrient=cellOrientLocal, &
!         faceOrient=faceOrientLocal, edgeOrient=edgeOrientLocal, &
!         tCellOrder=localSizes(1), tFaceOrder=localSizes(2), &
!         tEdgeOrder=localSizes(3), &
!         tCellOrient=localSizes(4), tFaceOrient=localSizes(5:6), &
!         tEdgeOrient=localSizes(7), &
!         globalElement=iel, islocal=.TRUE.)
!       CALL obj%GetOrders( &
!         cellOrder=cellOrder, faceOrder=faceOrder, &
!         edgeOrder=edgeOrder, cellOrient=cellOrient, &
!         faceOrient=faceOrient, edgeOrient=edgeOrient, &
!         tCellOrder=sizes(1), tFaceOrder=sizes(2), tEdgeOrder=sizes(3), &
!         tCellOrient=sizes(4), tFaceOrient=sizes(5:6), &
!         tEdgeOrient=sizes(7), &
!         globalElement=elements(iel), islocal=.FALSE.)
!       CALL OK(ALL(sizes .EQ. localSizes), "Local/global output sizes")
!       CALL OK(ALL(cellOrder(1:sizes(1)) .EQ. &
!                   cellOrderLocal(1:localSizes(1))), &
!               "Local/global cell order")
!       CALL OK(ALL(faceOrder(:, 1:sizes(2)) .EQ. &
!                   faceOrderLocal(:, 1:localSizes(2))), &
!                   "Local/global facet orders")
!       CALL OK(ALL(edgeOrder(1:sizes(3)) .EQ. &
!                   edgeOrderLocal(1:localSizes(3))), &
!                   "Local/global edge orders")
!       CALL OK(ALL(cellOrient(1:sizes(4)) .EQ. &
!                   cellOrientLocal(1:localSizes(4))), &
!                   "Local/global cell orientation")
!       CALL OK(ALL(faceOrient(1:sizes(5), 1:sizes(6)) .EQ. &
!                   faceOrientLocal(1:localSizes(5), 1:localSizes(6))), &
!               "Local/global facet orientations")
!       CALL OK(ALL(edgeOrient(1:sizes(7)) .EQ. &
!                   edgeOrientLocal(1:localSizes(7))), &
!                   "Local/global edge orientations")
!     END DO
!   END DO
! END DO
!
! CALL obj%DEALLOCATE()
! mesh => NULL()
! CALL dom%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
! OPEN (NEWUNIT=unitno, FILE="H1Orders_test_1.h5", &
!       STATUS="old", ACTION="read")
! CLOSE (unitno, STATUS="delete")
! END PROGRAM main
!```
!
! See also: [[obj_SetFE]], [[obj_GetCellOrder]]

INTERFACE
  MODULE SUBROUTINE obj_GetOrders(obj, cellOrder, faceOrder, edgeOrder, &
                                  cellOrient, faceOrient, edgeOrient, &
                                  tCellOrder, tFaceOrder, tEdgeOrder, &
                                  tCellOrient, tFaceOrient, tEdgeOrient, &
                                  globalElement, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: cellOrder(:)
    !! cell order in entries 1:tCellOrder; capacity at least one
    INTEGER(I4B), INTENT(INOUT) :: faceOrder(:, :)
    !! face orders in (1:3, 1:tFaceOrder); three identical order rows
    INTEGER(I4B), INTENT(INOUT) :: edgeOrder(:)
    !! edge orders in entries 1:tEdgeOrder
    INTEGER(I4B), INTENT(INOUT) :: cellOrient(:)
    !! cell orientations in entries 1:tCellOrient
    INTEGER(I4B), INTENT(INOUT) :: faceOrient(:, :)
    !! face orientations in (1:tFaceOrient(1), 1:tFaceOrient(2))
    INTEGER(I4B), INTENT(INOUT) :: edgeOrient(:)
    !! edge orientations in entries 1:tEdgeOrient
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
    !! if true, globalElement is a mesh-local element number; default false
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
!   CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
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
! summary: Get the largest number of quadrature points of an element
!
! For each element, set the quadrature order of its finite element to
! scaleForQuadOrder times its cell order and get the number of quadrature
! points. The largest of these numbers is returned. The value is computed
! once and stored; it is computed again after the orders change.
!
! Changes:
!   - 2026-10-29: return the largest value, not the one of the last element
!
! Returns:
!   ans : INTEGER(I4B) - largest number of quadrature points of an element
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
    !! largest number of quadrature points of an element
  END FUNCTION obj_GetMaxTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetSparsity@SetSparsityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-09
! summary: Set the sparsity pattern of a matrix
!
! Two degrees of freedom are connected if they belong to the same element:
! for each active element, all its degrees of freedom are connected to each
! other. Inactive elements are skipped.
!
! In a broken space (isBrokenSpace is true) the degrees of freedom of a cell
! are also connected to the degrees of freedom of its neighbour cells (the
! cells which share a facet with it, see GetElementToElements_). These
! couplings are needed by the numerical fluxes on the interior facets. Set
! onlyCell to true to skip them and keep only the cell blocks. onlyCell is
! ignored for continuous spaces.
!
! At the end the pattern of mat is finished (CSRMatrix SetSparsity).
! FEDOFSetSparsity(obj, mat) calls this method too.
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!   - 2026-11-03: Skip inactive elements; add neighbour-cell couplings for
!     broken spaces, controlled by onlyCell.
!
! Examples
!
! Dummy example; the complete program is SetSparsity_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_) :: dom
! TYPE(CSRMatrix_) :: mat
! ! ... initiate dom from a mesh file ...
! ! ... initiate mat with the total number of DOFs ...
!
! ! continuous space: couplings within each element
! CALL obj%Initiate(baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   isBrokenSpace=.FALSE., order=2, dom=dom)
! CALL obj%SetSparsity(mat=mat)
!
! ! broken space: cell blocks and neighbour-cell couplings
! CALL obj%Initiate(baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   isBrokenSpace=.TRUE., order=1, dom=dom)
! CALL obj%SetSparsity(mat=mat, onlyCell=.FALSE.)
!
! ! broken space: cell blocks only
! CALL obj%SetSparsity(mat=mat, onlyCell=.TRUE.)
!```
!
! See also: [[obj_SetSparsity2]], [[obj_SetSparsity3]]

INTERFACE FEDOFSetSparsity
  MODULE SUBROUTINE obj_SetSparsity1(obj, mat, onlyCell)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyCell
    !! used only for broken spaces; if true, only the couplings within
    !! each cell are set, and the neighbour-cell couplings (jumps at the
    !! interior facets) are skipped; default is false
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
!   USE GlobalData, ONLY: FMT_DOF
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
!                      storageFMT=FMT_DOF)
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
! In a broken space, the pointer refers to the reused local H1 Lagrange
! evaluator. The broken property comes from cell-owned numbering, not a
! different local polynomial. Call SetFE before consuming it. Shared-face
! order reduction and shared-edge orientation corrections are not used
! for a broken space.
!
! Return the finite element of the topology of the element. Call SetFE for the
! element first, so that the orders and orientations of the finite element are
! those of the element.
!
! Changes:
!   - 2026-10-30: Support cell-owned DG numbering and local evaluation.
!
! Returns:
!   ans : CLASS(AbstractFE_), pointer - pointer to the finite element
!
! Examples
!
! Dummy example; the complete program is GetQuadraturePoints_test_1.F90
! in src/modules/FEDOF/examples.
!
!```fortran
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_) :: dom
! ! ... initiate dom from a mesh file ...
! CALL obj%Initiate(baseContinuity="H1", &
!   baseInterpolation="HIER", &
!   order=2, &
!   dom=dom, &
!   ipType=quadopt%equidistance, &
!   quadratureIsHomogeneous=math%yes, &
!   quadratureType=[quadopt%GaussLegendre], &
!   quadratureOrder=[4], &
!   quadratureIsOrder=math%yes)
! feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
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
! fully discontinuous vector. The value is computed at the first
! call and cached in obj%sumElemTotalDOF; DEALLOCATE (and therefore
! Initiate) resets the cache.
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
!     msg = "order = " // ToString(order) // ": "
!
!     CALL obj%Initiate(order=order, dom=dom, baseContinuity="H1", &
!                       baseInterpolation="Hierarchical", &
!                       quadratureIsHomogeneous=math%yes, &
!                       quadratureType=[quadopt%GaussLegendre], &
!                       quadratureOrder=[2 * order], &
!                       quadratureIsOrder=math%yes)
!
!     found = obj%GetSumElemTotalDOF()
!     CALL OK(found == nloc * telems, msg // "nloc * telems")
!
!     want = 0
!     DO iel = 1, telems
!       want = want + obj%GetTotalDOF(globalElement=iel, islocal=math%yes)
!     END DO
!     CALL OK(found == want, msg // "sum of GetTotalDOF")
!
!     found = obj%GetSumElemTotalDOF()
!     CALL OK(found == want, msg // "cached value")
!
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
! (baseContinuity, baseInterpolation, brokenSpace, ipType, quadrature
! options) from the same table; isBrokenSpace of the FEDOF is taken from
! them (key brokenSpace, default false).
!
! Changes:
!   - 2026-11-02: isBrokenSpace is read from the finite elements
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
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE Display_Method, ONLY: Display
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! USE Test_Method, ONLY: IS
!
! IMPLICIT NONE
!
! TYPE(FEDOF_) :: obj
! TYPE(FEDomain_) :: dom
! CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
! CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
! CHARACTER(*), PARAMETER :: tomlFileName = &
!                            "./toml/ImportFromToml_test_1.toml"
! TYPE(HDF5File_) :: meshfile
! INTEGER(I4B) :: found, want
!
! CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
! CALL e%setQuietMode(EXCEPTION_DEBUG, .TRUE.)
!
! CALL meshfile%Initiate(filename, mode="READ")
! CALL meshfile%OPEN()
! CALL dom%Initiate(meshfile, '')
!
! meshptr => dom%GetMeshPointer()
!
! CALL obj%ImportFromToml( &
!   tomlName="test1", filename=tomlFileName, dom=dom)
!
! !CALL dom%Display("domain:")
! CALL dom%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
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
