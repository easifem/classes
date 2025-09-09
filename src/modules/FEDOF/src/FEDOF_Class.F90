! This program is a part of EASIFEM library
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

MODULE FEDOF_Class
USE GlobalData, ONLY: DFP, I4B, LGT, INT8
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE BaseType, ONLY: CSRMatrix_, &
                    QuadraturePoint_, &
                    ElemshapeData_
USE AbstractFE_Class, ONLY: AbstractFE_, AbstractFEPointer_
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE
PRIVATE

PUBLIC :: FEDOF_
PUBLIC :: FEDOFPointer_
PUBLIC :: FEDOFSetSparsity
PUBLIC :: SetFEDOFParam

CHARACTER(*), PARAMETER :: modName = "FEDOF_Class"
CHARACTER(*), PARAMETER :: myprefix = "FEDOF"
CHARACTER(*), PARAMETER :: DEFAULT_BASETYPE = "Monomial"
CHARACTER(*), PARAMETER :: DEFAULT_IPTYPE = "Equidistance"
REAL(DFP), PARAMETER :: DEFAULT_ALPHA = 0.0_DFP
REAL(DFP), PARAMETER :: DEFAULT_BETA = 0.0_DFP
REAL(DFP), PARAMETER :: DEFAULT_LAMBDA = 0.5_DFP
CHARACTER(*), PARAMETER :: fedofEssentialParam = &
  "baseContinuity/baseInterpolation/orderFile/ipType/basisType/alpha/&
  &beta/lambda/"

!----------------------------------------------------------------------------
!                                                                   FEDOF_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: FEDOF data type

TYPE :: FEDOF_
  PRIVATE
  LOGICAL(LGT) :: isLagrange = .FALSE.
  !! It is true when baseInterpolation is Lagrange
  LOGICAL(LGT) :: isinit = .FALSE.
  !! It is set to true when FEDOF is initiated
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
  INTEGER(I4B) :: maxTotalConnectivity = 0
  !! maximum number of connectivity

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

  INTEGER(INT8) :: scaleForQuadOrder = 1_INT8
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
  PRIVATE

  !CONSTRUCTOR:
  !@ConstructorMethods
  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate FEDOF by using homogeneous order
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate FEDOF by using inhomogeneous order
  PROCEDURE, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate FEDOF from ParameterList
  PROCEDURE, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate FEDOF from order vector defined for global elements
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3, &
    Initiate4
  !! Generic method for initiating FEDOF
  PROCEDURE, PASS(obj) :: AllocateSizes => obj_AllocateSizes
  !! This method is called in the Intiate methods
  !! This is a private method. It is used for allocating the size of
  !! cellOrder, faceOrder, edgeOrder, edgeIA, faceIA, cellIA
  PROCEDURE, PASS(obj) :: SetOrdersFromCellOrder => &
    obj_SetOrdersFromCellOrder
  !! This method is private method
  !! This method is used to set the faceOrder, edgeOrder from
  !! cellOrder. This method is called internally from
  !! Initiate methods. This put data in faceIA, edgeIA, cellIA
  PROCEDURE, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! Check essential parameters
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Returns true of the fedof is initiated

  !SET:
  !@SetMethods
  PROCEDURE, PASS(obj) :: SetCellOrder => obj_SetCellOrder
  !! Set the cell order, this is a private method
  PROCEDURE, PASS(obj) :: SetFaceOrder => obj_SetFaceOrder
  !! Set the face order, this is a private method
  PROCEDURE, PASS(obj) :: SetEdgeOrder => obj_SetEdgeOrder
  !! Set the edge order, this is a private method
  PROCEDURE, PUBLIC, PASS(obj) :: SetNodeCoord => obj_SetNodeCoord
  !! Set the node coordinates

  !IO:
  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the contents of FEDOF
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayCellOrder => &
    obj_DisplayCellOrder
  !! Display cell order
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import from toml
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Import from toml file

  !GET:
  !@GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetCaseName => obj_GetCaseName
  !! Get the case name of fedof, it returns baseContinuity+baseInterpolation

  PROCEDURE, PUBLIC, PASS(obj) :: GetVertexDOF => obj_GetVertexDOF
  !! Get vertex degrees of freedom

  PROCEDURE, PASS(obj) :: GetEdgeDOF1 => obj_GetEdgeDOF1
  !! Get edge degrees of freedom
  PROCEDURE, PASS(obj) :: GetEdgeDOF2 => obj_GetEdgeDOF2
  !! Get edge degree of freedom from global element and
  !! local edge number
  GENERIC, PUBLIC :: GetEdgeDOF => GetEdgeDOF1, GetEdgeDOF2

  PROCEDURE, PASS(obj) :: GetTotalEdgeDOF1 => obj_GetTotalEdgeDOF1
  !! Get total edge dof
  PROCEDURE, PASS(obj) :: GetTotalEdgeDOF2 => obj_GetTotalEdgeDOF2
  !! Get total edge dof from global element and local edge number
  GENERIC, PUBLIC :: GetTotalEdgeDOF => GetTotalEdgeDOF1, GetTotalEdgeDOF2

  PROCEDURE, PASS(obj) :: GetFaceDOF1 => obj_GetFaceDOF1
  !! Get face degrees of freedom
  PROCEDURE, PASS(obj) :: GetFaceDOF2 => obj_GetFaceDOF2
  !! Get face degrees of freedom from globbal element and
  !! local face number
  GENERIC, PUBLIC :: GetFaceDOF => GetFaceDOF1, GetFaceDOF2

  PROCEDURE, PASS(obj) :: GetTotalFaceDOF1 => obj_GetTotalFaceDOF1
  !! Get total face dof
  PROCEDURE, PASS(obj) :: GetTotalFaceDOF2 => obj_GetTotalFaceDOF2
  !! Get total face dof from global element and local face number
  GENERIC, PUBLIC :: GetTotalFaceDOF => GetTotalFaceDOF1, GetTotalFaceDOF2

  PROCEDURE, PUBLIC, PASS(obj) :: GetCellDOF => obj_GetCellDOF
  !! Get cell degrees of freedom
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCellDOF => obj_GetTotalCellDOF
  !! Get total cell degrees of freedom

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalVertexDOF => obj_GetTotalVertexDOF
  !! Retuns the total number of vertex dof

  PROCEDURE, PASS(obj) :: GetTotalDOF1 => obj_GetTotalDOF1
  !! Retuns the total degrees of freedom in FEDOF
  PROCEDURE, PASS(obj) :: GetTotalDOF2 => obj_GetTotalDOF2
  !! Retuns the total dof of an element
  PROCEDURE, PASS(obj) :: GetTotalDOF3 => obj_GetTotalDOF3
  !! Retuns the total dof of an element with opt filter
  GENERIC, PUBLIC :: GetTotalDOF => GetTotalDOF1, GetTotalDOF2, GetTotalDOF3
  !! Generic mehthod for getting the total dof

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Get the prefix for setting the data

  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity_ => obj_GetConnectivity_
  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => obj_GetConnectivity

  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshPointer => obj_GetMeshPointer
  !! Get the mesh pointer

  PROCEDURE, PUBLIC, PASS(obj) :: GetDomainPointer => obj_GetDomainPointer
  !! Get the domain pointer

  PROCEDURE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Get the base interpolation

  PROCEDURE, PUBLIC, PASS(obj) :: GetCellOrder => obj_GetCellOrder
  !! Get the cell order

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrders => obj_GetOrders
  !! Get cell,face, and edge orders
  !! Also get orientation of face and edge

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxTotalConnectivity => &
    obj_GetMaxTotalConnectivity
  !! Get the maximum size of connectivity

  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Get quadrature points for isotropic order

  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetQuadraturePoints => &
    obj_GetFacetQuadraturePoints
  !! Get quadrature points on a local face of global element

  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData

  PROCEDURE, PASS(obj) :: GetLocalElemShapeDataH1Lagrange => &
    obj_GetLocalElemShapeDataH1Lagrange

  PROCEDURE, PASS(obj) :: GetLocalElemShapeDataH1Hierarchical => &
    obj_GetLocalElemShapeDataH1Hierarchical

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData

  !SET:
  !@SetSparsityMethods

  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_SetSparsity1
  !! Set sparsity in the CSRMatrix by using single FEDOF
  !! This is for non block matrix

  PROCEDURE, PASS(obj) :: SetSparsity2 => obj_SetSparsity2
  !! Set sparsity in the CSRMatrix by using single FEDOF
  !! This is for non block matrix

  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2

END TYPE FEDOF_

!----------------------------------------------------------------------------
!                                                             FEDOFPointer_
!----------------------------------------------------------------------------

TYPE :: FEDOFPointer_
  TYPE(FEDOF_), POINTER :: ptr => NULL()
END TYPE FEDOFPointer_

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine Check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(FEDOF_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           SetFEDOFParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Set the essential parameters for constructing the FEDOF

INTERFACE
  MODULE SUBROUTINE SetFEDOFParam(param, baseContinuity, baseInterpolation, &
                            orderFile, ipType, basisType, alpha, beta, lambda)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity or conformity of basis defined on reference
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Type of basis functions used for interpolation on reference
    CHARACTER(*), INTENT(IN) :: orderFile
    !! file containing the order of each element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! basis type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! jacobian parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! jacobian parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! ultraspherical parameter
  END SUBROUTINE SetFEDOFParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate geometric fedof

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
    !! Finite element degree of freedom object
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of basis (regularity)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis function used for interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
    !! Finite element type, scalar, vector, tensor
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation type
    !! used when baseInterpolation is Lagrange
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! type of basis function used for
    !! constructing the Lagrange polynomial
    !! Used when baseInterpolation is Lagrange
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! alpha parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! beta parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! lambda parameter for Ultraspherical parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Ultraspherical
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! Degree of freedom type, default is nodal
    !! Size should be 4
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! is quadratur homogeneous in all dimensions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature type in x, y, and z directions
    !! Size should be 3, Read more at QuadratureOpt_
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! quadrature accuracy in x, y, and z direction
    !! Size should be 3, Read more at QuadratureOpt_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Is quadrature order considered
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(:)
    !! Number of interpolation points in x, y, and z directions
    !! Size should be 3, Read more at QuadratureOpt_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Should we consider quadratureNips
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha(:)
    !! Size should be 3, Read more at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta(:)
    !! Size should be 3, Read more at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda(:)
    !! Size should be 3, Read more at QuadratureOpt_
  END SUBROUTINE obj_InitiateGeoFEDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof
!
!# Introduction
!
! This method makes order0(1) from order and calls obj_Initiate2.

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
    INTEGER(I4B), INTENT(IN) :: order
    !! homogeneous value of order
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! Domain
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of basis (regularity)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis function used for interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
    !! Finite element type, scalar, vector, tensor
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation type
    !! used when baseInterpolation is Lagrange
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! type of basis function used for
    !! constructing the Lagrange polynomial
    !! Used when baseInterpolation is Lagrange
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! alpha parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! beta parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! lambda parameter for Ultraspherical parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Ultraspherical
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! Degree of freedom type, default is nodal
    !! Size should be 4
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! is quadratur homogeneous in all dimensions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature type in x, y, and z directions
    !! Size should be 3, Read more at QuadratureOpt_
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! quadrature accuracy in x, y, and z direction
    !! Size should be 3, Read more at QuadratureOpt_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Is quadrature order considered
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(:)
    !! Number of interpolation points in x, y, and z directions
    !! Size should be 3, Read more at QuadratureOpt_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Should we consider quadratureNips
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha(:)
    !! Size should be 3, Read more at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta(:)
    !! Size should be 3, Read more at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda(:)
    !! Size should be 3, Read more at QuadratureOpt_
  END SUBROUTINE obj_Initiate1

END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, order, dom, baseContinuity, &
                                  baseInterpolation, feType, ipType, &
                                  basisType, alpha, lambda, beta, islocal, &
                                  dofType, transformType, &
                                  quadratureIsHomogeneous, &
                                  quadratureType, quadratureOrder, &
                                  quadratureIsOrder, quadratureNips, &
                                  quadratureIsNips, quadratureAlpha, &
                                  quadratureBeta, quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! Finite degree of freedom object
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! Inhomogeneous value of order
    !! This is order of each cell element
    !! see the note on islocal
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of basis (regularity)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis function used for interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
    !! Fintie element type, scalar, vector, tensor
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation type
    !! used when baseInterpolation is Lagrange
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! type of basis function used for
    !! constructing the Lagrange polynomial
    !! Used when baseInterpolation is Lagrange
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! alpha parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! beta parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! lambda parameter for Ultraspherical parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Ultraspherical
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! islocal denotes whether the order(:) is based on
    !! local element or global element number.
    !! local element means in order(ii) ii is the local
    !! element number, global element means in order(ii), ii is the
    !! global element number. Note that getting local element
    !! number is difficult for user, so it is better to use
    !! global element number.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! Degree of freedom type, default is nodal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! is quadratur homogeneous in all dimensions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature type in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! quadrature accuracy in x, y, and z direction
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Is quadrature order considered
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(:)
    !! Number of interpolation points in x, y, and z directions
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Infor at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha(:)
    !! Info at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta(:)
    !! Info at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda(:)
    !! Info at QuadratureOpt_
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, param, dom)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! Fintie degree of freedom object
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter list
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain
  END SUBROUTINE obj_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof
!
!# Introduction
!
! This routine is similar to the obj_Initiate2, but the order of the
! element is defined for global element numbers.
! The number of rows in order is equal to 2
! the first row contains the global element number
! the second row contains the order.
!
! This routine will make order0(:) from order(:,:) and call initiate2

INTERFACE
  MODULE SUBROUTINE obj_Initiate4(obj, order, dom, baseContinuity, &
                                  baseInterpolation, feType, ipType, &
                                  basisType, alpha, beta, lambda, &
                                  dofType, transformType, &
                                  quadratureIsHomogeneous, &
                                  quadratureType, quadratureOrder, &
                                  quadratureIsOrder, quadratureNips, &
                                  quadratureIsNips, quadratureAlpha, &
                                  quadratureBeta, quadratureLambda)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order(:, :)
    !! the number of columns in order is equal to total number of elements
    !! the number of rows in order is equal to 2
    !! the first row contains the global element number
    !! the second rows contains the order of that element
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! Domain
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of basis function
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation of basis
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
    !! finite element type, scalar, vector, tensor
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation type
    !! used when baseInterpolation is Lagrange
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! type of basis function used for
    !! constructing the Lagrange polynomial
    !! Used when baseInterpolation is Lagrange
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! alpha parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! beta parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! lambda parameter for Ultraspherical parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Ultraspherical
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! Degree of freedom type, default is nodal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! is quadratur homogeneous in all dimensions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature type in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! quadrature accuracy in x, y, and z direction
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Is quadrature order considered
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(:)
    !! Number of interpolation points in x, y, and z directions
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Infor at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha(:)
    !! Info at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta(:)
    !! Info at QuadratureOpt_
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda(:)
    !! Info at QuadratureOpt_
  END SUBROUTINE obj_Initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                           AllocateSizes@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary:  Allocate the sizes of cellOrder, faceOrder, edgeOrder, edgeIA,
! faceIA, cellIA

INTERFACE
  MODULE SUBROUTINE obj_AllocateSizes(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AllocateSizes
END INTERFACE

!----------------------------------------------------------------------------
!                                  SetOrdersFromCellOrder@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary:  Set the faceOrder, edgeOrder from cellOrder

INTERFACE
  MODULE SUBROUTINE obj_SetOrdersFromCellOrder(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetOrdersFromCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: This method Copy obj2 in obj
!
!# Introduction
! This method is used to copy the contents of obj2 in obj.
! This method is same as the assignment operator (=).

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    CLASS(FEDOF_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Deallocate the data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                              IsInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary:  Returns true if the FEDOF is initiated

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetCaseName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-27
! summary:  Get the case name of fedof

INTERFACE
  MODULE FUNCTION obj_GetCaseName(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    CHARACTER(6) :: ans
  END FUNCTION obj_GetCaseName
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get the connectivity (function)

INTERFACE
  MODULE FUNCTION obj_GetConnectivity(obj, opt, globalElement, islocal) &
    RESULT(ans)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CHARACTER(*), INTENT(IN) :: opt
    !! opt = Vertex
    !! opt = Edge
    !! opt = Face
    !! opt = Cell
    !! opt = All
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if islocal true then globalElement is local element number
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! connectivity of element
  END FUNCTION obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetConnectivity_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get the connectivity

INTERFACE
 MODULE SUBROUTINE obj_GetConnectivity_(obj, ans, tsize, opt, globalElement, &
                                         islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! connectivity of element
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in con
    CHARACTER(*), INTENT(IN) :: opt
    !! opt = Vertex
    !! opt = Edge
    !! opt = Face
    !! opt = Cell
    !! opt = All
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if islocal true then globalElement is local element number
  END SUBROUTINE obj_GetConnectivity_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary:  Get vertex degree of freedom

INTERFACE
  MODULE SUBROUTINE obj_GetVertexDOF(obj, globalNode, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    !! this number should be obtained from the connectivity methods
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! vertex degree of freedom
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true then globalNode is local node number
  END SUBROUTINE obj_GetVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary: Get the dof on a global edge number

INTERFACE
  MODULE SUBROUTINE obj_GetEdgeDOF1(obj, globalEdge, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalEdge
    !! global edge number
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! edge degree of freedom
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true then globalEdge is local edge number
  END SUBROUTINE obj_GetEdgeDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-21
! summary:  Get edge degree of freedom

INTERFACE
  MODULE SUBROUTINE obj_GetEdgeDOF2(obj, globalElement, localEdgeNumber, &
                                    ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local cell element number
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local edge number in global element
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! edge degree of freedom
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! tota size of data written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true then globalElement is local element
  END SUBROUTINE obj_GetEdgeDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetTotalEdgeDOF@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetTotalEdgeDOF1(obj, globalEdge, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalEdge
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalEdgeDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalEdgeDOF@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetTotalEdgeDOF2(obj, globalElement, localEdgeNumber, &
                                       islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local cell element number
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local edge number in global element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true then globalElement is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalEdgeDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2024-07-21
! summary:  Get face degree of freedom

INTERFACE
  MODULE SUBROUTINE obj_GetFaceDOF1(obj, globalFace, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(IN) :: globalFace
    !! global face number
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! face degree of freedom
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! If true then globalFace is local face number
  END SUBROUTINE obj_GetFaceDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2024-07-21
! summary:  Get face degree of freedom

INTERFACE
  MODULE SUBROUTINE obj_GetFaceDOF2(obj, globalElement, localFaceNumber, &
                                    ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! DOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in globall element
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! face degree of freedom
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true then globalElement is local element
  END SUBROUTINE obj_GetFaceDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetCellDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary:  Get cell degree of freedom

INTERFACE
  MODULE SUBROUTINE obj_GetCellDOF(obj, globalCell, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalCell
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
  END SUBROUTINE obj_GetCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetCellDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary: Get total cell degree of freedom

INTERFACE
  MODULE FUNCTION obj_GetTotalCellDOF(obj, globalCell, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalCell
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-27
! summary:  Get total face dof

INTERFACE
  MODULE FUNCTION obj_GetTotalFaceDOF1(obj, globalFace, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalFace
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalFaceDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-27
! summary:  Get total number of degree of freedom on face

INTERFACE
  MODULE FUNCTION obj_GetTotalFaceDOF2(obj, globalElement, localFaceNumber, &
                                       islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! DOF object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in globall element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true then globalElement is local element
    INTEGER(I4B) :: ans
    !! Total number of degree of freedom on face
  END FUNCTION obj_GetTotalFaceDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-29
! summary:  Returns total number of vertex dof

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexDOF(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Returns total number of dof in the FEDOF

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF1(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Returns total number of dof in the FEDOF

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF2(obj, globalElement, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Returns total number of dof in the FEDOF with opt filter

INTERFACE
MODULE FUNCTION obj_GetTotalDOF3(obj, globalElement, opt, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    CHARACTER(*), INTENT(IN) :: opt
    !! opt for Vertex, Edge, Face, Cell, and All
    !! opt = Vertex
    !! opt = Edge
    !! opt = Face
    !! opt = Cell
    !! opt = All
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if islocal true then globalElement is local element number
    INTEGER(I4B) :: ans
    !! Total number of dof in the FEDOF with opt filter
  END FUNCTION obj_GetTotalDOF3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Get the prefix for setting essential parameters

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-19
! summary: Display the content of FE DOF

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-19
! summary: Display cell order

INTERFACE
  MODULE SUBROUTINE obj_DisplayCellOrder(obj, msg, unitno, full)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: full
    !! if full is present and true then all data will be displayed.
  END SUBROUTINE obj_DisplayCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetCellOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the cell order

INTERFACE
  MODULE SUBROUTINE obj_SetCellOrder(obj, order, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! this is cell order
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! islocal denotes whether the order(:) is based on
    !! local element or global element number.
    !! local element means in order(ii) ii is the local
    !! element number, global element means in order(ii) ii is the
    !! global element number. Note that getting local element
    !! number is difficult for user, so it is better to use
    !! global element number.
  END SUBROUTINE obj_SetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetFaceOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the face order

INTERFACE
  MODULE SUBROUTINE obj_SetFaceOrder(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! this is cell order
  END SUBROUTINE obj_SetFaceOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetEdgeOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the Edge order

INTERFACE
  MODULE SUBROUTINE obj_SetEdgeOrder(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetEdgeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetNodeCoord@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-18
! summary: Set nodal coordinates for a global element

INTERFACE
  MODULE SUBROUTINE obj_SetNodeCoord(obj, globalElement, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! fedof object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_SetNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Get the mesh pointer

INTERFACE
  MODULE FUNCTION obj_GetMeshPointer(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    CLASS(AbstractMesh_), POINTER :: ans
  END FUNCTION obj_GetMeshPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetDomainPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-24
! summary: Get the domain pointer

INTERFACE
  MODULE FUNCTION obj_GetDomainPointer(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    CLASS(AbstractDomain_), POINTER :: ans
  END FUNCTION obj_GetDomainPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetBaseInterpolation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the base interpolation

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetBaseInterpolation
END INTERFACE

!----------------------------------------------------------------------------
!
!                                                      GetCellOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary:  Get the order of cell

INTERFACE
  MODULE SUBROUTINE obj_GetCellOrder(obj, cellOrder, tCellOrder, &
                                     globalElement, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! fedof object
    INTEGER(I4B), INTENT(INOUT) :: cellOrder(:)
    !! cell order
    INTEGER(I4B), INTENT(OUT) :: tCellOrder
    !! size of data written in cellOrder
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalElement is local element
  END SUBROUTINE obj_GetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetOrders@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  Get cellOrder, faceOrder, edgeOrder

INTERFACE
  MODULE SUBROUTINE obj_GetOrders(obj, cellOrder, faceOrder, edgeOrder, &
                                  cellOrient, faceOrient, edgeOrient, &
                                  tCellOrder, tFaceOrder, tEdgeOrder, &
                                  tCellOrient, tFaceOrient, tEdgeOrient, &
                                  globalElement, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    !! fedof object
    INTEGER(I4B), INTENT(INOUT) :: cellOrder(:)
    !! cell order
    INTEGER(I4B), INTENT(INOUT) :: faceOrder(:, :)
    !! number of rows in faceOrder is equal to 3
    !! number of columns in faceOrder is equal to total faces
    INTEGER(I4B), INTENT(INOUT) :: edgeOrder(:)
    !! size if equal to the total number of edges in element
    INTEGER(I4B), INTENT(INOUT) :: cellOrient(:)
    !! size is equal to 1
    INTEGER(I4B), INTENT(INOUT) :: faceOrient(:, :)
    !! face orientation flags
    !! number of rows is 3
    !! number of columns is tface
    INTEGER(I4B), INTENT(INOUT) :: edgeOrient(:)
    !! orientaion flag for edge
    !! size is equal to tedge
    INTEGER(I4B), INTENT(OUT) :: tCellOrder
    !! size of data written in cellOrder
    INTEGER(I4B), INTENT(OUT) :: tFaceOrder
    !! size of data written in faceorder
    INTEGER(I4B), INTENT(OUT) :: tEdgeOrder
    !! size of data written in edgeorder
    INTEGER(I4B), INTENT(OUT) :: tCellOrient
    !! size of data written in cellOrder
    INTEGER(I4B), INTENT(OUT) :: tFaceOrient(2)
    !! size of data written in faceorder
    INTEGER(I4B), INTENT(OUT) :: tEdgeOrient
    !! size of data written in edgeorder
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalElement is local element
  END SUBROUTINE obj_GetOrders
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetMaxTotalConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Get maximum size of connectivity

INTERFACE
  MODULE FUNCTION obj_GetMaxTotalConnectivity(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxTotalConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetMaxTotalQuadraturePoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Get maximum number of quadrature points in an element

INTERFACE
  MODULE FUNCTION obj_GetMaxTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetSparsity@SetSparsityMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-09
! summary: Set sparsity in CSRMatrix_ from AbstractDomain_

INTERFACE FEDOFSetSparsity

  MODULE SUBROUTINE obj_SetSparsity1(obj, mat)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE obj_SetSparsity1
END INTERFACE FEDOFSetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine Sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity2(obj, col_fedof, cellToCell, mat, &
                                     ivar, jvar)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    CLASS(FEDOF_), INTENT(INOUT) :: col_fedof
    !! Abstract mesh class
    INTEGER(I4B), INTENT(IN) :: cellToCell(:)
    !! cell To Cell connectivity between mesh of obj and col_fedof
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable in row
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable in column
  END SUBROUTINE obj_SetSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetSparsity@SetSparsityMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Set sparsity in [[CSRMatrix_]] from [[AbstractDomain_]]

INTERFACE FEDOFSetSparsity
  MODULE SUBROUTINE obj_SetSparsity3(fedofs, mat)
    CLASS(FEDOFPointer_), INTENT(INOUT) :: fedofs(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE obj_SetSparsity3
END INTERFACE FEDOFSetSparsity

!----------------------------------------------------------------------------
!                                                        GetQuadraturePoints
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-18
! summary:  Get quadrature points for a global element

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad, globalElement, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! fedof object
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! quadrature points
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetQuadraturePoints
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-18
! summary:  Get quadrature points for a global element on a local face

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                 globalElement, &
                                                 localFaceNumber, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! fedof object
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    !! quadrature points
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in the global element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-13
! summary:  Get local element shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, globalElement, elemsd, &
                                              quad, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    TYPE(ElemshapeData_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeDataH1Lagrange(obj, &
                                         globalElement, elemsd, quad, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetLocalElemShapeDataH1Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeDataH1Hierarchical(obj, &
                                         globalElement, elemsd, quad, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetLocalElemShapeDataH1Hierarchical
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-13
! summary:  Get global element shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, globalElement, elemsd, &
                                               xij, geoElemsd, islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! Abstract finite element
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! shape function data
    TYPE(ElemshapeData_), INTENT(INOUT) :: elemsd
    !! global element shape data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoElemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then the global element is a local element
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE FEDOFImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE FEDOFImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml, dom)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE FEDOFImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE FEDOFImportFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEDOF_Class
