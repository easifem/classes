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
!

MODULE OneDimFEDOF_Class
USE GlobalData, ONLY: DFP, I4B, LGT, INT8
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE BaseType, ONLY: CSRMatrix_, &
                    QuadraturePoint_, &
                    ElemshapeData_
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_

USE OneDimDomain_Class, ONLY: OneDimDomain_

USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE
PRIVATE

PUBLIC :: OneDimFEDOF_
PUBLIC :: OneDimFEDOFPointer_
PUBLIC :: OneDimFEDOFSetSparsity
PUBLIC :: SetOneDimFEDOFParam

CHARACTER(*), PARAMETER :: modName = "OneDimFEDOF_Class"
CHARACTER(*), PARAMETER :: myprefix = "OneDimFEDOF"
CHARACTER(*), PARAMETER :: DEFAULT_BASETYPE = "Monomial"
CHARACTER(*), PARAMETER :: DEFAULT_IPTYPE = "Equidistance"
REAL(DFP), PARAMETER :: DEFAULT_ALPHA = 0.0_DFP
REAL(DFP), PARAMETER :: DEFAULT_BETA = 0.0_DFP
REAL(DFP), PARAMETER :: DEFAULT_LAMBDA = 0.5_DFP
CHARACTER(*), PARAMETER :: essentialParam = "baseContinuity/baseInterpolation/orderFile/&
&ipType/basisType/alpha/beta/lambda/"

!----------------------------------------------------------------------------
!                                                                   OneDimFEDOF_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: OneDimFEDOF data type

TYPE :: OneDimFEDOF_
  PRIVATE
  LOGICAL(LGT) :: isinit = .FALSE.
  !! It is set to true when OneDimFEDOF is initiated
  INTEGER(I4B) :: tdof = 0
  !! Total number of degrees of freedom
  INTEGER(I4B) :: maxTotalConnectivity = 0
  !! maximum number of connectivity

  INTEGER(INT8) :: maxCellOrder = 0_INT8
  !! maximum value of cell order

  INTEGER(INT8), ALLOCATABLE :: cellOrder(:)
  !! Order of each cell
  !! the size of cellOrder is equal to the obj%tCells
  !! Get connectivity of an element
  !! Get entity number of an element
  !! Get the cell number of an element (this is global element number)
  !! convert it to the local element number
  !! use this local element number to get cell order from cellOrder

  INTEGER(I4B), ALLOCATABLE :: cellIA(:)
  !! sparsity for cell,
  !! the size of cellIA is equal to the total number of cells + 1
  !! The degrees of freedom of icell is stored in
  !! cellJA(cellIA(icell):cellIA(icell+1)-1)

  CLASS(AbstractOneDimFE_), POINTER :: fe => NULL()
  !! pointer to finite element object
  !! point, line, triangle, quadrangle, tetrahedron, hexahedron, prism,
  !! pyramid

  CLASS(OneDimDomain_), POINTER :: mesh => NULL()
  !! Pointer to domain

CONTAINS
  PRIVATE

  !CONSTRUCTOR:
  !@ConstructorMethods
  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate OneDimFEDOF by using homogeneous order
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate OneDimFEDOF by using inhomogeneous order
  PROCEDURE, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate OneDimFEDOF from ParameterList
  PROCEDURE, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate OneDimFEDOF from order vector defined for global elements
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3, &
    Initiate4
  !! Generic method for initiating OneDimFEDOF
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
  !! Returns true of the OneDimFEDOF is initiated

  !SET:
  !@SetMethods
  PROCEDURE, PASS(obj) :: SetCellOrder => obj_SetCellOrder
  !! Set the cell order, this is a private method
  PROCEDURE, PASS(obj) :: SetFaceOrder => obj_SetFaceOrder
  !! Set the face order, this is a private method
  PROCEDURE, PASS(obj) :: SetEdgeOrder => obj_SetEdgeOrder
  !! Set the edge order, this is a private method

  !IO:
  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the contents of OneDimFEDOF
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
  !! Get the case name of OneDimFEDOF, it returns baseContinuity+baseInterpolation

  PROCEDURE, PUBLIC, PASS(obj) :: GetVertexDOF => obj_GetVertexDOF
  !! Get vertex degrees of freedom

  PROCEDURE, PUBLIC, PASS(obj) :: GetCellDOF => obj_GetCellDOF
  !! Get cell degrees of freedom
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCellDOF => obj_GetTotalCellDOF
  !! Get total cell degrees of freedom

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalVertexDOF => obj_GetTotalVertexDOF
  !! Retuns the total number of vertex dof

  PROCEDURE, PASS(obj) :: GetTotalDOF1 => obj_GetTotalDOF1
  !! Retuns the total degrees of freedom in OneDimFEDOF
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

  PROCEDURE, PASS(obj) :: GetQuadraturePoints => obj_GetQuadraturePoints
  !! Get quadrature points for isotropic order

  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData

  !SET:
  !@SetSparsityMethods

  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_SetSparsity1
  !! Set sparsity in the CSRMatrix by using single OneDimFEDOF
  !! This is for non block matrix

  PROCEDURE, PASS(obj) :: SetSparsity2 => obj_SetSparsity2
  !! Set sparsity in the CSRMatrix by using single OneDimFEDOF
  !! This is for non block matrix

  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2

END TYPE OneDimFEDOF_

!----------------------------------------------------------------------------
!                                                             OneDimFEDOFPointer_
!----------------------------------------------------------------------------

TYPE :: OneDimFEDOFPointer_
  TYPE(OneDimFEDOF_), POINTER :: ptr => NULL()
END TYPE OneDimFEDOFPointer_

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine Check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           SetOneDimFEDOFParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Set the essential parameters for constructing the OneDimFEDOF

INTERFACE
  MODULE SUBROUTINE SetOneDimFEDOFParam(param, baseContinuity, baseInterpolation, &
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
  END SUBROUTINE SetOneDimFEDOFParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof
!
!# Introduction
! This method makes order0(1) from order and calls obj_Initiate2.

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, order, mesh, baseContinuity, &
          baseInterpolation, fetype, ipType, basisType, alpha, beta, lambda, &
                            quadratureType, quadratureOrder, quadratureNips, &
                            quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order
    !! homogeneous value of order
    CLASS(OneDimDomain_), TARGET, INTENT(IN) :: mesh
    !! cell mesh
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of basis (regularity)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis function used for interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar, (Vector)
    !! Read docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation type
    !! Read docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! type of basis function used for constructing the Lagrange polynomial
    !! Used when baseInterpolation is Lagrange
    !! Read docs of AbstractOneDimFE
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha parameter for jacobian polynomial
    !! Read docs of AbstractOneDimFE
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta parameter for jacobian polynomial
    !! Read docs of AbstractOneDimFE
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda parameter for Ultraspherical parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Ultraspherical
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Accuracy of quadrature rule
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(1)
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Ultraspherical parameter for quadrature
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, order, mesh, baseContinuity, &
          baseInterpolation, fetype, ipType, basisType, alpha, beta, lambda, &
  islocal, quadratureType, quadratureOrder, quadratureNips, quadratureAlpha, &
                                  quadratureBeta, quadratureLambda)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    !! Finite degree of freedom object
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! Inhomogeneous value of order
    !! This is order of each cell element
    !! see the note on islocal
    CLASS(OneDimDomain_), TARGET, INTENT(IN) :: mesh
    !! cell mesh
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of basis (regularity)
    !! Read the docs of AbstractOneDimFE
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis function used for interpolation
    !! Read the docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar (Vector)
    !! Read docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation type
    !! used when baseInterpolation is Lagrange
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! type of basis function used for
    !! constructing the Lagrange polynomial
    !! Used when baseInterpolation is Lagrange
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda parameter for Ultraspherical parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Ultraspherical
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! islocal denotes whether the order(:) is based on
    !! local element or global element number.
    !! local element means in order(ii) ii is the local
    !! element number, global element means in order(ii) ii is the
    !! global element number. Note that getting local element
    !! number is difficult for user, so it is better to use
    !! global element number.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Accuracy of quadrature rule
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(1)
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Ultraspherical parameter for quadrature
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, param, mesh)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    !! Fintie degree of freedom object
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter list
    CLASS(OneDimDomain_), TARGET, INTENT(IN) :: mesh
    !! mesh
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
  MODULE SUBROUTINE obj_Initiate4(obj, order, mesh, baseContinuity, &
          baseInterpolation, fetype, ipType, basisType, alpha, beta, lambda, &
           quadratureType, quadratureOrder, quadratureNips, quadratureAlpha, &
                                  quadratureBeta, quadratureLambda)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order(:, :)
    !! The number of columns in order is equal to total number of elements
    !! The number of rows in order is equal to 2
    !! The first row contains the global element number
    !! the second rows contains the order of that element
    CLASS(OneDimDomain_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of basis function
    !! Read the docs of AbstractOneDimFE
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation of basis
    !! Read the docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar (Vector)
    !! Read docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation type
    !! used when baseInterpolation is Lagrange
    !! Read the docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! type of basis function used for
    !! constructing the Lagrange polynomial
    !! Used when baseInterpolation is Lagrange
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta parameter for jacobian parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda parameter for Ultraspherical parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Ultraspherical
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Accuracy of quadrature rule
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(1)
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Ultraspherical parameter for quadrature
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj2
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                              IsInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary:  Returns true if the OneDimFEDOF is initiated

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetCaseName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-27
! summary:  Get the case name of OneDimFEDOF

INTERFACE
  MODULE FUNCTION obj_GetCaseName(obj) RESULT(ans)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    !! OneDimFEDOF object
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    !! OneDimFEDOF object
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
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    !! OneDimFEDOF object
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
!                                                      GetCellDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary:  Get cell degree of freedom

INTERFACE
  MODULE SUBROUTINE obj_GetCellDOF(obj, globalCell, ans, tsize, islocal)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalCell
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-29
! summary:  Returns total number of vertex dof of the entire OneDimFEDOF
! In other words, it returns the total number of vertex nodes in mesh

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexDOF(obj) RESULT(ans)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Returns total number of dof in the OneDimFEDOF

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF1(obj) RESULT(ans)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Returns total number of dof in the OneDimFEDOF

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF2(obj, globalElement, islocal) RESULT(ans)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
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
! summary: Returns total number of dof in the OneDimFEDOF with opt filter

INTERFACE
MODULE FUNCTION obj_GetTotalDOF3(obj, globalElement, opt, islocal) RESULT(ans)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
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
    !! Total number of dof in the OneDimFEDOF with opt filter
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
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetEdgeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Get the mesh pointer

INTERFACE
  MODULE FUNCTION obj_GetMeshPointer(obj) RESULT(ans)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    CLASS(OneDimDomain_), POINTER :: ans
  END FUNCTION obj_GetMeshPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetBaseInterpolation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the base interpolation

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
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
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    !! OneDimFEDOF object
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
  MODULE SUBROUTINE obj_GetOrders(obj, cellOrder, tCellOrder, globalElement, &
                                  islocal)
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    !! OneDimFEDOF object
    INTEGER(I4B), INTENT(INOUT) :: cellOrder(:)
    !! cell order
    INTEGER(I4B), INTENT(OUT) :: tCellOrder
    !! size of data written in cellOrder
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
    CLASS(OneDimFEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxTotalConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetSparsity@SetSparsityMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-09
! summary: Set sparsity in CSRMatrix_ from AbstractDomain_

INTERFACE OneDimFEDOFSetSparsity

  MODULE SUBROUTINE obj_SetSparsity1(obj, mat)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE obj_SetSparsity1
END INTERFACE OneDimFEDOFSetSparsity

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
  MODULE SUBROUTINE obj_SetSparsity2(obj, col_OneDimFEDOF, cellToCell, mat, &
                                     ivar, jvar)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    !! Abstract mesh class
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: col_OneDimFEDOF
    !! Abstract mesh class
    INTEGER(I4B), INTENT(IN) :: cellToCell(:)
    !! cell To Cell connectivity between mesh of obj and col_OneDimFEDOF
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

INTERFACE OneDimFEDOFSetSparsity
  MODULE SUBROUTINE obj_SetSparsity3(OneDimFEDOFs, mat)
    CLASS(OneDimFEDOFPointer_), INTENT(INOUT) :: OneDimFEDOFs(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE obj_SetSparsity3
END INTERFACE OneDimFEDOFSetSparsity

!----------------------------------------------------------------------------
!                                                        GetQuadraturePoints
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad, globalElement, &
                    quadratureType, order, nips, alpha, beta, lambda, islocal)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    !! OneDimFEDOF object
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! quadrature points
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Type of quadrature points
    !! Read the docs of AbstractOneDimFE and OneDimQuadratureOpt
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of integrand
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nips(1)
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_GetQuadraturePoints
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetLocalElemShapeData
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
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
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

INTERFACE OneDimFEDOFImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, mesh)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(OneDimDomain_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE OneDimFEDOFImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE OneDimFEDOFImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml, mesh)
    CLASS(OneDimFEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    CLASS(OneDimDomain_), OPTIONAL, INTENT(IN) :: mesh
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE OneDimFEDOFImportFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimFEDOF_Class
