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
!

MODULE LineH1HierarchicalFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractFE_Class, ONLY: AbstractFE_
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: QuadraturePoint_, ElemShapeData_

IMPLICIT NONE

PRIVATE

PUBLIC :: LineH1HierarchicalFE_
PUBLIC :: LineH1HierarchicalFEPointer_
PUBLIC :: LineH1HierarchicalFEPointer

PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "LineH1HierarchicalFE_Class"

!----------------------------------------------------------------------------
!                                                           LineH1HierarchicalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-09
! summary:  Scalar H1 Hierarchical Finite Element

TYPE, EXTENDS(AbstractFE_) :: LineH1HierarchicalFE_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get the Local element shape data
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Get the Global element shape data
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order of shape functions
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Get the quadrature points
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetQuadraturePoints => &
    obj_GetFacetQuadraturePoints
  !! Get the quadrature points on a local face of element
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set the order of accuracy for quadrature
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set the type of quadrature points
  PROCEDURE, PUBLIC, PASS(obj) :: GetInterpolationPoints => &
    obj_GetInterpolationPoints
  !! Get Interpolation points in cell element
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetTotalInterpolationPoints => obj_GetTotalInterpolationPoints
  !! Get total number of Interpolation points
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetDOFValueFromQuadrature => &
    obj_GetFacetDOFValueFromQuadrature
  !! Get the dof values of a function from its quadrature values on a facet
END TYPE LineH1HierarchicalFE_

!----------------------------------------------------------------------------
!                                                    LineH1HierarchicalFEPointer_
!----------------------------------------------------------------------------

TYPE :: LineH1HierarchicalFEPointer_
  CLASS(LineH1HierarchicalFE_), POINTER :: ptr => NULL()
END TYPE LineH1HierarchicalFEPointer_

!----------------------------------------------------------------------------
!                                             LineH1HierarchicalFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE LineH1HierarchicalFEPointer
  MODULE FUNCTION obj_LineH1HierarchicalFEPointer1() RESULT(ans)
    TYPE(LineH1HierarchicalFE_), POINTER :: ans
  END FUNCTION obj_LineH1HierarchicalFEPointer1
END INTERFACE LineH1HierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                          HierarchicalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE LineH1HierarchicalFEPointer
  MODULE FUNCTION obj_LineH1HierarchicalFEPointer2( &
   order, nsd, cellOrient, quadratureType, quadratureOrder, quadratureAlpha, &
    quadratureBeta, quadratureLambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Isotropic Order of finite element
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(3)
    !! Orientation of cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Order of quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha, quadratureBeta
    !! For jacobian polynomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! for ultraspherical polynomial
    TYPE(LineH1HierarchicalFE_), POINTER :: ans
    !! LineH1HierarchicalFE_ pointer
  END FUNCTION obj_LineH1HierarchicalFEPointer2
END INTERFACE LineH1HierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of LineH1HierarchicalFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(LineH1HierarchicalFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of LineH1HierarchicalFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(LineH1HierarchicalFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                               GetLocalElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  This routine set order in the already initiated AbstractFE_
!
!# Introduction
!
! This routine sets order in the already initiated AbstractFE_
! Make sure the object is initiated by calling correct constructor methods

INTERFACE
  MODULE SUBROUTINE obj_SetOrder( &
    obj, order, anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, &
    faceOrient, edgeOrient, tCell, tFace, tEdge, errCheck)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! aniso tropic order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order
    !! number of rows in faceOrder is 3
    !! number of columns in faceOrder is tfaceorder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order
    !! size of edgeorder is tedgeorder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! cell orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient
    !! number of rows in faceoriient is 3
    !! number of columns in faceorient is tfaceorient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! size of cellOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! number of columns in faceOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! size of edgeorder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get quadrature points

! obj_Initiate9(obj, elemType, domainName, order, quadratureType,&
! alpha, beta, lambda, xij)

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                  GetFacetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get quadrature points on a local face of element

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                 localFaceNumber)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    !! Quadrature points
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
  END SUBROUTINE obj_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the order for quadrature

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureOrder(obj, order, order1, order2, order3)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order3
  END SUBROUTINE obj_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuadratureType@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary:  Set the quadrature type

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureType( &
    obj, quadratureType, quadratureType1, quadratureType2, quadratureType3)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
  END SUBROUTINE obj_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetTotalInterpolationPoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Get total number of interpolation points

INTERFACE
  MODULE FUNCTION obj_GetTotalInterpolationPoints( &
    obj, order, ipType) RESULT(ans)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order of interpolation in x, y, and z directions
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! interpolation point type in x, y, and z directions
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetInterpolationPoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get Interpolation points

INTERFACE
  MODULE SUBROUTINE obj_GetInterpolationPoints( &
    obj, xij, ans, nrow, ncol, order, ipType, alpha, beta, lambda)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! Abstract finite elemenet
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of reference element
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nodal coordinates of interpolation points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! data written in xij
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! interpolation point type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:), beta(:), lambda(:)
    !! Jacobi and Ultraspherical parameters
  END SUBROUTINE obj_GetInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoelemsd)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get Interpolation points

INTERFACE
  MODULE SUBROUTINE obj_GetFacetDOFValueFromQuadrature( &
    obj, elemsd, facetElemsd, xij, localFaceNumber, func, ans, tsize, &
    massMat, ipiv)
    CLASS(LineH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! Abstract finite elemenet
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! element shape function defined inside the cell
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! shape function defined on the face of element
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of reference element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    REAL(DFP), INTENT(INOUT) :: func(:)
    !! user defined functions
    !! quadrature values of function
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! nodal coordinates of interpolation points
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! data written in xij
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! pivot indices for LU decomposition of mass matrix
  END SUBROUTINE obj_GetFacetDOFValueFromQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LineH1HierarchicalFE_Class
