! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

MODULE AbstractOneDimFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: ElemShapeData_, &
                    QuadraturePoint_

USE String_Class, ONLY: String

USE FPL, ONLY: ParameterList_

USE ExceptionHandler_Class, ONLY: e

USE OneDimBasisOpt_Class, ONLY: OneDimBasisOpt_

IMPLICIT NONE
PRIVATE

PUBLIC :: AbstractOneDimFE_
PUBLIC :: AbstractOneDimFEPointer_
PUBLIC :: SetAbstractOneDimFEParam
PUBLIC :: AbstractOneDimFEDeallocate
PUBLIC :: AbstractOneDimFEDisplay
PUBLIC :: AbstractOneDimFEInitiate
PUBLIC :: AbstractOneDimFECheckEssentialParam
PUBLIC :: DEALLOCATE

CHARACTER(*), PARAMETER :: modName = "AbstractOneDimFE_Class"
CHARACTER(*), PARAMETER :: AbstractOneDimFEEssentialParams = &
     "/order/fetype/ipType/refElemDomain/baseContinuity/baseInterpolation"// &
                           "/basisType/alpha/beta/lambda"

!----------------------------------------------------------------------------
!                                                        AbstractOneDimFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Abstract class for finite element is defined
!
!{!pages/docs-api/AbstractOneDimFE/AbstractOneDimFE_.md!}

TYPE, ABSTRACT :: AbstractOneDimFE_
  PRIVATE
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is set to true at the time of constructor

  TYPE(OneDimBasisOpt_) :: opt

  REAL(DFP), ALLOCATABLE :: coeff(:, :)
  !! coefficient necessary for lagrange interpolation

  REAL(DFP), ALLOCATABLE :: xij(:, :)
  !! interpolation points for lagrange polynomial
  !! coeff, and xij are needed internally for
  !! constructing the lagrange polynomial

CONTAINS

  PRIVATE

  ! CONSTRUCTOR:
  !@ConstructorMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate method from the parameter list
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate method from the parameters

  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Initiate by copy

  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Initiate by copy

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! This method checks the essential parameters in the parameter list
  !! It is called while initiating the object from the parameter list

  !IO:
  !@IOMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of a finite element
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: MdEncode => obj_MdEncode
  !! Display the contents
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ReactEncode => obj_ReactEncode
  !! Display the contents
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Deallocate the data stored in an instance

  ! SET:
  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Sets the parameters of finite element

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order and reallocate appropriate data in
  !! already initiated AbstractOneDimFE_

  !GET:
  ! @GetMethods
  PROCEDURE(obj_GetPrefix), DEFERRED, PUBLIC, PASS(obj) :: GetPrefix
  !! Get prefix

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get local element shape data for Discontinuous Galerkin

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Get global element shape data

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Sets the parameters of finite element

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetCaseName => &
    obj_GetCaseName
  !! Get case name for the finite element

  ! GET:
  ! @QuadratureMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints

END TYPE AbstractOneDimFE_

!----------------------------------------------------------------------------
!                                                   AbstractOneDimFEPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractOneDimFEPointer_
  CLASS(AbstractOneDimFE_), POINTER :: ptr => NULL()
END TYPE AbstractOneDimFEPointer_

!----------------------------------------------------------------------------
!                                    CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-08-11
! summary: This routine Check the essential parameters in param.

INTERFACE AbstractOneDimFECheckEssentialParam
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(AbstractOneDimFE_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE AbstractOneDimFECheckEssentialParam

!----------------------------------------------------------------------------
!                                SetAbstractOneDimFEParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-11
! summary:  Sets the parameters for initiating abstract finite element

INTERFACE
  MODULE SUBROUTINE SetAbstractOneDimFEParam(param, prefix, baseContinuity, &
   baseInterpolation, ipType, basisType, alpha, beta, lambda, order, fetype, &
           quadratureType, quadratureOrder, quadratureNips, quadratureAlpha, &
                                             quadratureBeta, quadratureLambda)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! ParameterList
    CHARACTER(*), INTENT(IN) :: prefix
    !! Prefix
    INTEGER(I4B), INTENT(IN) :: order
    !! Isotropic Order of finite element
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity or Conformity of basis function.
    !! This parameter is used to determine the nodal coordinates of
    !! reference element, when xij is not present.
    !! If xij is present then this parameter is ignored
    !! H1* (default), HDiv, HCurl, DG
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis function family used for interpolation.
    !! This parameter is used to determine the nodal coordinates of
    !! reference element, when xij is not present.
    !! If xij is present then this parameter is ignored
    !! LagrangeInterpolation, LagrangePolynomial
    !! SerendipityInterpolation, SerendipityPolynomial
    !! HierarchyInterpolation, HierarchyPolynomial
    !! OrthogonalInterpolation, OrthogonalPolynomial
    !! HermitInterpolation, HermitPolynomial
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial
    !! Default ipType is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis type: Legendre, Lobatto, Ultraspherical,
    !! Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar
    !! For HDiv and Hcurl it should be Vector
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
  END SUBROUTINE SetAbstractOneDimFEParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiates an instance of the finite element

INTERFACE AbstractOneDimFEInitiate
  MODULE SUBROUTINE obj_Initiate1(obj, param)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate1
END INTERFACE AbstractOneDimFEInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiates an instance of the finite element

INTERFACE AbstractOneDimFEInitiate
  MODULE SUBROUTINE obj_Initiate2(obj, baseContinuity, baseInterpolation, &
                      ipType, basisType, alpha, beta, lambda, order, fetype, &
                            quadratureType, quadratureOrder, quadratureNips, &
                            quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
    !! Finite element object
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity or Conformity of basis function.
    !! This parameter is used to determine the nodal coordinates of
    !! reference element, when xij is not present.
    !! If xij is present then this parameter is ignored
    !! H1* (default), HDiv, HCurl, DG
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis function family used for interpolation.
    !! This parameter is used to determine the nodal coordinates of
    !! reference element, when xij is not present.
    !! If xij is present then this parameter is ignored
    !! LagrangeInterpolation, LagrangePolynomial
    !! SerendipityInterpolation, SerendipityPolynomial
    !! HierarchyInterpolation, HierarchyPolynomial
    !! OrthogonalInterpolation, OrthogonalPolynomial
    !! HermitInterpolation, HermitPolynomial
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial
    !! Default ipType is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis type: Legendre, Lobatto, Ultraspherical,
    !! Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic Order of finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar
    !! For HDiv and Hcurl it should be Vector
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
END INTERFACE AbstractOneDimFEInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary: Initiates an instance of the finite element by copying

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
    CLASS(AbstractOneDimFE_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary: Deallocate the data

INTERFACE AbstractOneDimFEDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractOneDimFEDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE AbstractOneDimFEDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(AbstractOneDimFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE AbstractOneDimFEDeallocate

INTERFACE DEALLOCATE
  MODULE PROCEDURE Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-14
! summary: Display the content

INTERFACE AbstractOneDimFEDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno, notFull)
    CLASS(AbstractOneDimFE_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull
  END SUBROUTINE obj_Display
END INTERFACE AbstractOneDimFEDisplay

!----------------------------------------------------------------------------
!                                                          MdEncode@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the contents in mardown format

INTERFACE
  MODULE FUNCTION obj_MdEncode(obj) RESULT(ans)
    CLASS(AbstractOneDimFE_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_MdEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ReactEncode@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the reference elements in react components

INTERFACE
  MODULE FUNCTION obj_ReactEncode(obj) RESULT(ans)
    CLASS(AbstractOneDimFE_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Set the parameters

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, order, fetype, ipType, basisType, &
      alpha, beta, lambda, refElemDomain, baseContinuity, baseInterpolation, firstCall, &
           quadratureType, quadratureOrder, quadratureNips, quadratureAlpha, &
                                 quadratureBeta, quadratureLambda)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
    !! AbstractOneDimFE object
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! finite element type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation point type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis type in x, y, and z directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    !! String name of type of continuity used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
    !! String name of type of interpolation used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refElemDomain
    !! Domain of reference element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !!
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
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  This routine set order in the already initiated AbstractOneDimFE_

INTERFACE
  MODULE SUBROUTINE obj_SetOrder(obj, order)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), INTENT(IN) :: order
    !! order
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-11
! summary:  Get prefix

ABSTRACT INTERFACE
  FUNCTION obj_GetPrefix(obj) RESULT(ans)
    IMPORT :: AbstractOneDimFE_
    CLASS(AbstractOneDimFE_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Get the parameters

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, order, fetype, ipType, basisType, &
                         alpha, beta, lambda, refElemDomain, baseContinuity, &
                                 baseInterpolation, firstCall, isInitiated, &
                            quadratureType, quadratureOrder, quadratureNips, &
                            quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(AbstractOneDimFE_), INTENT(IN) :: obj
    !! Abstract one dimenstional finite element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: fetype
    !! finite element type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ipType
    !! interpolation point type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: basisType
    !! Basis type in x, y, and z directions
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda
    !! Ultraspherical parameter
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseContinuity
    !! String name of type of continuity used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseInterpolation
    !! String name of type of interpolation used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: refElemDomain
    !! Domain of reference element
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: firstCall
    !!
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated
    !! Returns isInitiated
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureOrder
    !! Accuracy of quadrature rule
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureNips(1)
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureAlpha
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureBeta
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureLambda
    !! Ultraspherical parameter for quadrature
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoelemsd)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoelemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                     GetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-19
! summary: Get the quadrature points for the finite element

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad, quadratureType, &
                                            order, alpha, beta, lambda)
    CLASS(AbstractOneDimFE_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Type of quadrature points
    !! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau, GaussLegendreRadauLeft
    !! GaussLegendreRadauRight ! GaussChebyshev
    !! GaussChebyshevLobatto ! GaussChebyshevRadau, GaussChebyshevRadauLeft
    !! GaussChebyshevRadauRight
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of integrand
    !! either the order or the nips should be present
    !! Both nips and order should not be present
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetCaseName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-21
! summary: Get case name for the finite element

INTERFACE
  MODULE FUNCTION obj_GetCaseName(obj) RESULT(ans)
    CLASS(AbstractOneDimFE_), INTENT(in) :: obj
    CHARACTER(len=:), ALLOCATABLE :: ans
  END FUNCTION obj_GetCaseName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractOneDimFE_Class
