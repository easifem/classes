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

MODULE OneDimBasisOpt_Class

USE GlobalData, ONLY: I4B, DFP, LGT

USE String_Class, ONLY: String

USE Display_Method, ONLY: ToString, Display

USE BaseType, ONLY: ipopt => TypeInterpolationOpt, &
                    polyopt => TypePolynomialOpt, &
                    QuadraturePoint_, &
                    TypeFEVariableOpt, &
                    elemNameOpt => TypeElemNameOpt

USE ParameterList, ONLY: ParameterList_
USE FPL_Method, ONLY: GetValue, CheckEssentialParam, Set
USE ExceptionHandler_Class, ONLY: e

USE StringUtility, ONLY: UpperCase
USE InputUtility, ONLY: Input

USE LineInterpolationUtility, ONLY: RefElemDomain_Line
USE ReferenceLine_Method, ONLY: RefCoord_Line

USE BaseInterpolation_Method, ONLY: BaseType_ToChar, &
                                    BaseInterpolation_ToChar

USE QuadraturePoint_Method, ONLY: QuadratureCopy => Copy, &
                                  QuadraturePointDisplay => Display, &
                                  QuadraturePointInitiate => Initiate

USE OneDimQuadratureOpt_Class, ONLY: OneDimQuadratureOpt_, &
                                     TypeOneDimQuadratureOpt, &
                                     SetOneDimQuadratureOptParam

IMPLICIT NONE

PRIVATE

#define quadOptPrefix "quadratureOpt"

CHARACTER(*), PARAMETER :: modName = "OneDimBasisOpt_Class"
CHARACTER(*), PARAMETER :: essentialParams = &
     "/order/fetype/ipType/refElemDomain/baseContinuity/baseInterpolation"// &
                           "/basisType/alpha/beta/lambda"

PUBLIC :: OneDimBasisOpt_, TypeOneDimBasisOpt
PUBLIC :: SetOneDimBasisOptParam

!----------------------------------------------------------------------------
!                                                             BasisOpt_Class
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary: This class contains data necessary for forming basis functions
! in one dimension

TYPE :: OneDimBasisOpt_
  PRIVATE
  LOGICAL(LGT) :: firstCall = .TRUE.
  !! flag to check if the shape functions are constructed from scratch or not

  INTEGER(I4B) :: tdof = 0
  !! total number of degrees of freedom
  !! this is equal to order + 1  in one dimension case

  INTEGER(I4B) :: order = 0
  !! order of the basis functions

  INTEGER(I4B) :: fetype = 0
  !! type of finite element
  !!  Scalar, Vector, Matrix

  INTEGER(I4B) :: ipType = ipopt%equidistance
  !! interpolation type, it is used incase baseInterpolation is Lagrange

  INTEGER(I4B) :: basisType = polyopt%monomial
  !! basis type, it is used in case baseInterpolation is Lagrange
  !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto
  !! Ultraspherical

  REAL(DFP) :: alpha = 0.0_DFP
  !! Jacobi polynomial parameter, x, y, z
  REAL(DFP) :: beta = 0.0_DFP
  !! Jacobi polynomial parametera, x, y, z
  REAL(DFP) :: lambda = 0.5_DFP
  !! Ultraspherical polynomial parameter, x, y, z

  CHARACTER(1) :: refelemDomain = "B"
  !! String name for reference element domain.
  !! It can take following values:
  !! - UNIT "U"
  !! - BIUNIT "B"

  CHARACTER(2) :: baseContinuity = "H1"
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG

  CHARACTER(4) :: baseInterpolation = "LAGR"
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation
  !! HierarchyInterpolation
  !! OrthogonalInterpolation
  !! HermitInterpolation
  !! SerendipityInterpolation

  REAL(DFP) :: refelemCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])
  !! coordinate of reference element

  CHARACTER(128) :: basisType_char = "MONOMIAL"
  !! basis type in string format

  CHARACTER(128) :: ipType_char = "EQUIDISTANCE"
  !! interpolation type in string format

  TYPE(OneDimQuadratureOpt_) :: quadOpt
  !! Quadrature options

CONTAINS

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate OneDimBasisOpt object from parameters

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate2 => &
    obj_Initiate2
  !! Initiate by arguments

  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! This method checks the essential parameters in the parameter list
  !! It is called while initiating the object from the parameter list

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Deallocate the object

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the contents of the object

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set the parameters of the object

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order of the basis functions

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseContinuity => &
    obj_GetBaseContinuity
  !! Get baseContinuity

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Get baseInterpolation

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetCaseName => &
    obj_GetCaseName
  !! Get the case name

END TYPE OneDimBasisOpt_

!----------------------------------------------------------------------------
!                                                              TypeBasisOpt
!----------------------------------------------------------------------------

TYPE(OneDimBasisOpt_), PARAMETER :: TypeOneDimBasisOpt = OneDimBasisOpt_()

!----------------------------------------------------------------------------
CONTAINS
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-16
! summary:  Set the basis type in parameter list

SUBROUTINE SetOneDimBasisOptParam(param, prefix, order, baseContinuity, &
          baseInterpolation, ipType, basisType, alpha, beta, lambda, feType, &
           quadratureType, quadratureOrder, quadratureNips, quadratureAlpha, &
                                  quadratureBeta, quadratureLambda)

  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  !! prefix used for setting values in parameter list
  INTEGER(I4B), INTENT(IN) :: order
  !! order of the basis functionsa
  CHARACTER(*), INTENT(IN) :: baseContinuity
  !! continuity of basis functions
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  !! interpolation of basis functions
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
  !! Interpolation type in case baseInterpolation is Lagrange
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
  !! basis type in case baseInterpolation is Lagrange
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
  !! Jacobi polynomial parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta
  !! Jacobi polynomial parametera
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
  !! Ultraspherical polynomial parameter
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
  !! Finite element type
  !! Default is Scalar
  !! For HDiv and Hcurl it should be Vector
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
  !! Quadrature type
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
  !! Quadrature order
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(1)
  !! Number of integration points
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
  !! Quadrature alpha parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
  !! Quadrature beta parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
  !! Quadrature lambda parameter

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType_Line()"
  INTEGER(I4B), PARAMETER :: default_feType = TypeFEVariableOpt%scalar
  INTEGER(I4B), PARAMETER :: default_ipType = ipopt%equidistance
  CHARACTER(:), ALLOCATABLE :: baseContinuity0, baseInterpolation0
  INTEGER(I4B) :: aint
  REAL(DFP) :: areal
  TYPE(String) :: refelemDomainName
  LOGICAL(LGT) :: isok, abool
  TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! setting order
  CALL Set(obj=param, prefix=prefix, key="order", &
           datatype=order, VALUE=order)

  ! setting baseContinuity
  baseContinuity0 = UpperCase(baseContinuity)
  CALL Set(obj=param, prefix=prefix, key="baseContinuity", &
           datatype=baseContinuity0, VALUE=baseContinuity0)

  ! setting baseInterpolation
  baseInterpolation0 = UpperCase(baseInterpolation)
  CALL Set(obj=param, prefix=prefix, key="baseInterpolation", &
           datatype=baseContinuity0, VALUE=baseInterpolation0)

  ! setting feType, default fetypee is scalar
  aint = Input(default=default_feType, option=feType)
  CALL Set(obj=param, prefix=prefix, key="feType", &
           datatype=aint, VALUE=aint)

  !  ipType should be present when baseInterpolation is Lagrange
  abool = baseInterpolation0(1:4) .EQ. "LAGR"
  IF (abool) THEN
    isok = PRESENT(ipType)
    CALL AssertError1(isok, myName, &
                  'In case of LAGRANGE polynomials ipType should be present.')
  END IF
  aint = Input(default=default_ipType, option=ipType)
  CALL Set(obj=param, prefix=prefix, key="ipType", datatype=aint, &
           VALUE=aint)

  ! setting refElemDomain
  refelemDomainName = RefElemDomain_Line(baseInterpol=baseInterpolation0, &
                                         baseContinuity=baseContinuity0)
  CALL Set(obj=param, prefix=prefix, key="refElemDomain", &
           datatype=refelemDomainName, VALUE=refelemDomainName)

  ! setting basisType
  isok = PRESENT(basisType)

  IF (isok) THEN
    aint = basisType
  ELSE
    SELECT CASE (baseInterpolation0(1:4))
    CASE ("LAGR")
      aint = polyopt%monomial
    CASE ("ORTH")
      aint = polyopt%legendre
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[INTERNAL ERROR] :: No case found for baseInterpolation0(1:4)='// &
                        baseInterpolation0(1:4))
    END SELECT
  END IF
  CALL Set(obj=param, prefix=prefix, key="basisType", datatype=aint, &
           VALUE=aint)

  ! default value of alpha is 0.0
  areal = 0.0_DFP; IF (PRESENT(alpha)) areal = alpha
  CALL Set(obj=param, prefix=prefix, key="alpha", datatype=areal, &
           VALUE=areal)

  ! default value of beta is 0.0
  areal = 0.0_DFP; IF (PRESENT(beta)) areal = beta
  CALL Set(obj=param, prefix=prefix, key="beta", datatype=areal, &
           VALUE=areal)

  ! default value of lambda is 0.5
  areal = 0.5_DFP; IF (PRESENT(lambda)) areal = lambda
  CALL Set(obj=param, prefix=prefix, key="lambda", datatype=areal, &
           VALUE=areal)

  baseContinuity0 = ""
  baseInterpolation0 = ""
  refelemDomainName = ""

  ! We need to make a sublist for quadOpt
  ! otherwise the keys will collide, quadOptPrefix is a macro
  ! defined at the top of this file
  sublist => NULL()
  sublist => param%NewSubList(key=quadOptPrefix)

  isok = ASSOCIATED(sublist)
  CALL AssertError1(isok, myName, &
                    'cannot get sublist by using prefix='//quadOptPrefix)

  CALL SetOneDimQuadratureOptParam(param=sublist, &
                                   prefix=prefix, &
                                   quadratureType=quadratureType, &
                                   order=quadratureOrder, &
                                   nips=quadratureNips, &
                                   alpha=quadratureAlpha, &
                                   beta=quadratureBeta, &
                                   lambda=quadratureLambda)

  sublist => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetOneDimBasisOptParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-16
! summary: Initiate OneDimBasisOpt object from parameters

SUBROUTINE obj_Initiate1(obj, param, prefix)
  CLASS(OneDimBasisOpt_), INTENT(inout) :: obj
  TYPE(ParameterList_), INTENT(in) :: param
  CHARACTER(*), INTENT(in) :: prefix

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "OneDimBasisOpt_Initiate()"
#endif

  TYPE(String) :: astr
  TYPE(ParameterList_), POINTER :: sublist
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL obj%DEALLOCATE()

  CALL obj%CheckEssentialParam(param=param, prefix=prefix)

  CALL GetValue(obj=param, prefix=prefix, key="order", &
                VALUE=obj%order)
  obj%tdof = obj%order + 1

  CALL GetValue(obj=param, prefix=prefix, key="feType", &
                VALUE=obj%feType)

  CALL GetValue(obj=param, prefix=prefix, key="ipType", &
                VALUE=obj%ipType)

  CALL GetValue(obj=param, prefix=prefix, key="basisType", &
                VALUE=obj%basisType)

  obj%basisType_char = BaseType_ToChar(obj%basisType, isUpper=.TRUE.)
  obj%ipType_char = BaseInterpolation_ToChar(obj%ipType, isUpper=.TRUE.)

  CALL GetValue(obj=param, prefix=prefix, key="alpha", &
                VALUE=obj%alpha)

  CALL GetValue(obj=param, prefix=prefix, key="beta", &
                VALUE=obj%beta)

  CALL GetValue(obj=param, prefix=prefix, key="lambda", &
                VALUE=obj%lambda)

  CALL GetValue(obj=param, prefix=prefix, key="refElemDomain", &
                VALUE=astr)
  obj%refelemDomain = astr%slice(1, 1)

  CALL GetValue(obj=param, prefix=prefix, key="baseContinuity", &
                VALUE=astr)
  obj%baseContinuity = astr%slice(1, 2)

  CALL GetValue(obj=param, prefix=prefix, key="baseInterpolation", &
                VALUE=astr)
  obj%baseInterpolation = astr%slice(1, 4)

  obj%refelemCoord(1:1, 1:2) = RefCoord_Line(obj%refelemDomain)

  ! now we will initiate quadOpt
  ! it is kept in a sublist, see the SetOneDimQuadratureOptParam
  ! method

  sublist => NULL()
  ierr = param%GetSubList(key=quadOptPrefix, sublist=sublist)

  isok = ASSOCIATED(sublist)
  CALL AssertError1(isok, myName, &
                    "cannot get sublist by using prefix="//quadOptPrefix)
  CALL obj%quadOpt%Initiate(param=sublist, prefix=prefix)

  sublist => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary:  Initiate OneDimBasisOpt by arguments

SUBROUTINE obj_Initiate2(obj, baseContinuity, baseInterpolation, &
                      ipType, basisType, alpha, beta, lambda, order, fetype, &
                         quadratureType, quadratureOrder, quadratureNips, &
                         quadratureAlpha, quadratureBeta, quadratureLambda)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
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
    !! Quadrature orders
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(1)
    !! Number of integration points
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Quadrature alpha parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Quadrature beta parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Quadrature lambda parameter

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myname = "obj_Initiate2()"
#endif

  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL obj%DEALLOCATE()

  CALL obj%SetParam( &
    baseContinuity=baseContinuity, &
    baseInterpolation=baseInterpolation, &
    ipType=ipType, &
    basisType=basisType, &
    alpha=alpha, &
    beta=beta, &
    lambda=lambda, &
    order=order, &
    fetype=fetype, &
    quadratureType=quadratureType, &
    quadratureOrder=quadratureOrder, &
    quadratureNips=quadratureNips, &
    quadratureAlpha=quadratureAlpha, &
    quadratureBeta=quadratureBeta, &
    quadratureLambda=quadratureLambda &
    )

  obj%tdof = obj%order + 1

  astr = RefElemDomain_Line(baseContinuity=baseContinuity, &
                            baseInterpol=baseInterpolation)
  obj%refelemDomain = astr%slice(1, 1)
  astr = ""
  obj%refelemCoord(1:1, 1:2) = RefCoord_Line(obj%refelemDomain)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE obj_Initiate2

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-08-11
! summary: This routine Check the essential parameters in param.

SUBROUTINE obj_CheckEssentialParam(obj, param, prefix)
  CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL CheckEssentialParam(obj=param, keys=essentialParams, &
                           prefix=prefix, myName=myName, modName=modName)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

SUBROUTINE obj_Deallocate(obj)
  CLASS(OneDimBasisOpt_), INTENT(inout) :: obj

  CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL obj%Copy(TypeOneDimBasisOpt)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Copy
!----------------------------------------------------------------------------

SUBROUTINE obj_Copy(obj, obj2)

  CLASS(OneDimBasisOpt_), INTENT(inout) :: obj
  CLASS(OneDimBasisOpt_), INTENT(in) :: obj2

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! Do not call obj%Deallocate(), here,
  ! because in obj%Deallocate() we are calling obj%Copy for
  ! resetting the object

  ! Copy the values from obj2 to obj
  obj%firstCall = obj2%firstCall
  obj%tdof = obj2%tdof
  obj%order = obj2%order
  obj%fetype = obj2%fetype
  obj%ipType = obj2%ipType
  obj%basisType = obj2%basisType
  obj%alpha = obj2%alpha
  obj%beta = obj2%beta
  obj%lambda = obj2%lambda
  obj%refelemDomain = obj2%refelemDomain
  obj%baseContinuity = obj2%baseContinuity
  obj%baseInterpolation = obj2%baseInterpolation
  obj%refelemCoord = obj2%refelemCoord
  obj%basisType_char = obj2%basisType_char
  obj%ipType_char = obj2%ipType_char

  CALL obj%quadOpt%Copy(obj2%quadOpt)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Copy

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary:  Display the contents of the object

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Display(msg=msg, unitno=unitno)
  CALL Display(obj%firstCall, msg="firstCall: ", unitno=unitno)
  CALL Display(obj%tdof, msg="tdof: ", unitno=unitno)
  CALL Display(obj%order, msg="order: ", unitno=unitno)
  CALL Display(obj%feType, msg="feType: ", unitno=unitno)
  CALL Display(obj%ipType, msg="ipType: ", unitno=unitno)
  CALL Display(obj%basisType, msg="basisType: ", unitno=unitno)
  CALL Display(obj%alpha, msg="alpha: ", unitno=unitno)
  CALL Display(obj%beta, msg="beta: ", unitno=unitno)
  CALL Display(obj%lambda, msg="lambda: ", unitno=unitno)
  CALL Display(obj%refElemDomain, msg="refElemDomain: ", unitno=unitno)
  CALL Display(obj%baseContinuity, msg="baseContinuity: ", unitno=unitno)
  CALL Display(obj%baseInterpolation, msg="baseInterpolation: ", &
               unitno=unitno)
  CALL Display(obj%refelemCoord, msg="refelemCoord: ", unitno=unitno)
  CALL Display(TRIM(obj%basisType_char), msg="basisType_char: ", &
               unitno=unitno)
  CALL Display(TRIM(obj%ipType_char), msg="ipType_char: ", &
               unitno=unitno)

  CALL obj%quadOpt%Display(msg="QuadratureOpt: ", unitno=unitno)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                                   SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary:  Set the parameters in the OneDimBasisOpt object

SUBROUTINE obj_SetParam(obj, order, fetype, ipType, basisType, alpha, &
             beta, lambda, refElemDomain, baseContinuity, baseInterpolation, &
                 firstCall, quadratureType, quadratureOrder, quadratureNips, &
                        quadratureAlpha, quadratureBeta, quadratureLambda)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
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
  !! Quadrature order
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(1)
  !! Number of integration points
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
  !! Quadrature alpha parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
  !! Quadrature beta parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
  !! Quadrature lambda parameter

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (PRESENT(order)) obj%order = order
  IF (PRESENT(feType)) obj%feType = feType
  IF (PRESENT(ipType)) obj%ipType = ipType
  IF (PRESENT(baseContinuity)) obj%baseContinuity = UpperCase(baseContinuity(1:2))
  IF (PRESENT(baseInterpolation)) obj%baseInterpolation = &
    UpperCase(baseInterpolation(1:4))
  IF (PRESENT(refElemDomain)) obj%refElemDomain = &
    UpperCase(refElemDomain(1:1))
  IF (PRESENT(basisType)) obj%basisType = basisType
  IF (PRESENT(alpha)) obj%alpha = alpha
  IF (PRESENT(beta)) obj%beta = beta
  IF (PRESENT(lambda)) obj%lambda = lambda
  IF (PRESENT(firstCall)) obj%firstCall = firstCall

  CALL obj%quadOpt%SetParam(quadratureType=quadratureType, &
                            order=quadratureOrder, &
                            nips=quadratureNips, &
                            alpha=quadratureAlpha, &
                            beta=quadratureBeta, &
                            lambda=quadratureLambda)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_SetParam

!----------------------------------------------------------------------------
!                                                              SetOrder
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: Set the order of the basis functions

SUBROUTINE obj_SetOrder(obj, order)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: order

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%order = order
  obj%tdof = order + 1
  obj%firstCall = .TRUE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_SetOrder

!----------------------------------------------------------------------------
!                                                                  GetParam
!----------------------------------------------------------------------------

SUBROUTINE obj_GetParam(obj, order, tdof, fetype, ipType, basisType, &
                        alpha, beta, lambda, refElemDomain, baseContinuity, &
                        baseInterpolation, firstCall, &
                        quadratureType, quadratureOrder, quadratureNips, &
                        quadratureAlpha, quadratureBeta, quadratureLambda)
  CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
  !! Abstract one dimenstional finite element
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order
  !! order of element (isotropic order)
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tdof
  !! total degrees of freedom
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
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureType
  !! Quadrature type
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureOrder
  !! Quadrature order
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureNips(1)
  !! Number of integration points
  REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureAlpha
  !! Quadrature alpha parameter
  REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureBeta
  !! Quadrature beta parameter
  REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureLambda
  !! Quadrature lambda parameter

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (PRESENT(order)) order = obj%order
  IF (PRESENT(tdof)) tdof = obj%tdof
  IF (PRESENT(feType)) feType = obj%feType
  IF (PRESENT(ipType)) ipType = obj%ipType
  IF (PRESENT(baseContinuity)) baseContinuity = obj%baseContinuity
  IF (PRESENT(baseInterpolation)) baseInterpolation = obj%baseInterpolation
  IF (PRESENT(refElemDomain)) refElemDomain = obj%refElemDomain
  IF (PRESENT(basisType)) basisType = obj%basisType
  IF (PRESENT(alpha)) alpha = obj%alpha
  IF (PRESENT(beta)) beta = obj%beta
  IF (PRESENT(lambda)) lambda = obj%lambda
  IF (PRESENT(firstCall)) firstCall = obj%firstCall

  CALL obj%quadOpt%GetParam(quadratureType=quadratureType, &
                            order=quadratureOrder, &
                            nips=quadratureNips, &
                            alpha=quadratureAlpha, &
                            beta=quadratureBeta, &
                            lambda=quadratureLambda)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_GetParam

!----------------------------------------------------------------------------
!                                                         GetBaseContinuity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-17
! summary: Get baseContinuity value from the object

FUNCTION obj_GetBaseContinuity(obj) RESULT(ans)
  CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    !! Object from which to get baseContinuity
  CHARACTER(2) :: ans
    !! The baseContinuity value

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetBaseContinuity()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ans = obj%baseContinuity

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION obj_GetBaseContinuity

!----------------------------------------------------------------------------
!                                                      GetBaseInterpolation
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-17
! summary: Get baseInterpolation value from the object

FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
  CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    !! Object from which to get baseInterpolation
  CHARACTER(4) :: ans
    !! The baseInterpolation value

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetBaseInterpolation()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ans = obj%baseInterpolation

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                       GetQuadraturePoints
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-19
! summary:  Get the quadratuere points

SUBROUTINE obj_GetQuadraturePoints(obj, quad, quadratureType, &
                                   order, alpha, beta, lambda)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    !! OneDimBasisOpt
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

  INTEGER(I4B) :: order0, quadratureType0
  REAL(DFP) :: alpha0, beta0, lambda0

  ! Let us update the quadrature points quadOpt
  CALL obj%quadOpt%SetParam(quadratureType=quadratureType, &
                           order=order, alpha=alpha, beta=beta, lambda=lambda)

  ! Let us get the quadrature points
  CALL obj%quadOpt%GetParam(quadratureType=quadratureType0, &
                       order=order0, alpha=alpha0, beta=beta0, lambda=lambda0)

  ! Internal variables
  CALL QuadraturePointInitiate(obj=quad, &
                               elemType=elemNameOpt%line, &
                               domainName=obj%refelemDomain, &
                               order=order0, &
                               quadratureType=quadratureType0, &
                               alpha=alpha0, &
                               beta=beta0, &
                               lambda=lambda0)

END SUBROUTINE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                             GetCaseName
!----------------------------------------------------------------------------

FUNCTION obj_GetCaseName(obj) RESULT(ans)
  CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
  CHARACTER(:), ALLOCATABLE :: ans
  ans = obj%baseContinuity//obj%baseInterpolation
END FUNCTION obj_GetCaseName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../../submodules/include/errors.F90"

END MODULE OneDimBasisOpt_Class
