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

MODULE QuadrangleH1FE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractFE_Class, ONLY: AbstractFE_
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: QuadraturePoint_, ElemShapeData_
USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE

PRIVATE

PUBLIC :: QuadrangleH1FE_
PUBLIC :: QuadrangleH1FEPointer_

CHARACTER(*), PARAMETER :: modName = "QuadrangleH1FE_Class"

!----------------------------------------------------------------------------
!                                                     QuadrangleH1FE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-09
! summary:  Scalar H1  Finite Element

TYPE, ABSTRACT, EXTENDS(AbstractFE_) :: QuadrangleH1FE_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Get the quadrature points
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetQuadraturePoints => &
    obj_GetFacetQuadraturePoints
  !! Get the quadrature points on a local face of element
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set the quadrature order
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set the quadrature type
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalInterpolationPoints => &
    obj_GetTotalInterpolationPoints
  !! Get total number of interpolation points
  PROCEDURE, PUBLIC, PASS(obj) :: GetInterpolationPoints => &
    obj_GetInterpolationPoints
  !! Get the interpolation points
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrientation => obj_SetOrientation
  !! Set the orientation of element
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetFacetDOFValueFromSTFunc => obj_GetFacetDOFValueFromSTFunc
  !! Get facet dof value from space-time user function

END TYPE QuadrangleH1FE_

!----------------------------------------------------------------------------
!                                                     QuadrangleH1FEPointer_
!----------------------------------------------------------------------------

TYPE :: QuadrangleH1FEPointer_
  CLASS(QuadrangleH1FE_), POINTER :: ptr => NULL()
END TYPE QuadrangleH1FEPointer_

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
    CLASS(QuadrangleH1FE_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetFacetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get quadrature points on a local face of element

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                 localFaceNumber)
    CLASS(QuadrangleH1FE_), INTENT(INOUT) :: obj
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
    CLASS(QuadrangleH1FE_), INTENT(INOUT) :: obj
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
    CLASS(QuadrangleH1FE_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
  END SUBROUTINE obj_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                           GetTotalInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Get total number of interpolation points

INTERFACE
  MODULE FUNCTION obj_GetTotalInterpolationPoints(obj, order, ipType) &
    RESULT(ans)
    CLASS(QuadrangleH1FE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order of interpolation in x, y, and z directions
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! interpolation point type in x, y, and z directions
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                 GetInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get Interpolation points

INTERFACE
  MODULE SUBROUTINE obj_GetInterpolationPoints( &
    obj, xij, ans, nrow, ncol, order, ipType, alpha, beta, lambda)
    CLASS(QuadrangleH1FE_), INTENT(INOUT) :: obj
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
!                                                   SetOrientation@SetMethods
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
  MODULE SUBROUTINE obj_SetOrientation( &
    obj, cellOrient, faceOrient, edgeOrient, tCell, tFace, tEdge, errCheck)
    CLASS(QuadrangleH1FE_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! cell orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient, number of rows in faceoriient is 3
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
  END SUBROUTINE obj_SetOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get Interpolation points
!
!# Introduction
!
! The user function should be scalar.
! It is a function of space and time.

INTERFACE
  MODULE SUBROUTINE obj_GetFacetDOFValueFromSTFunc( &
    obj, elemsd, facetElemsd, xij, times, localFaceNumber, func, ans, tsize, &
    massMat, ipiv, funcValue, onlyFaceBubble)
    CLASS(QuadrangleH1FE_), INTENT(INOUT) :: obj
    !! Abstract finite elemenet
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! element shape function defined inside the cell
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! shape function defined on the face of element
    REAL(DFP), INTENT(IN) :: xij(:, :), times
    !! Nodal coordinates of reference element and times
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! user defined functions
    !! quadrature values of function
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Nodal coordinates of interpolation points
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Data written in xij
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! pivot indices for LU decomposition of mass matrix
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! function values at quadrature points used inside
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyFaceBubble
    !! if true then we include only face bubble, that is,
    !! only include internal face bubble.
  END SUBROUTINE obj_GetFacetDOFValueFromSTFunc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE QuadrangleH1FE_Class
