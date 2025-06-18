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

MODULE OneDimQuadratureOpt_Class

USE GlobalData, ONLY: I4B, DFP
USE BaseType, ONLY: ipopt => TypeInterpolationOpt
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display, ToString
USE FPL, ONLY: ParameterList_
USE FPL_Method, ONLY: Set

USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar

USE InputUtility, ONLY: Input

IMPLICIT NONE

PRIVATE

PUBLIC :: OneDimQuadratureOpt_
PUBLIC :: TypeOneDimQuadratureOpt
PUBLIC :: SetOneDimQuadratureOptParam

CHARACTER(*), PARAMETER :: modName = "OneDimQuadratureOpt_Class"

!----------------------------------------------------------------------------
!                                                       OneDimQuadratureOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-18
! summary: Quadrature options in 1D

TYPE :: OneDimQuadratureOpt_
  INTEGER(I4B) :: quadratureType = ipopt%GaussLegendre
  !! quadrature type

  REAL(DFP) :: alpha = 0.0_DFP
  !! alpha parameter for Jacobi polynomials

  REAL(DFP) :: beta = 0.0_DFP
  !! beta parameter for Jacobi polynomials

  REAL(DFP) :: lambda = 0.5_DFP
  !! lambda parameter for Ultraspherical polynomials

  INTEGER(I4B) :: order = 0_I4B
  !! order of accuracy of the quadrature

  INTEGER(I4B) :: nips(1) = 0_I4B
  !! number of integration points

  CHARACTER(128) :: quadratureType_char = "GAUSSLEGENDRE"
  !! quadrature type

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy the options from another object

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of the object

  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set the parameters

END TYPE OneDimQuadratureOpt_

!----------------------------------------------------------------------------
!                                                    TypeOneDimQuadratureOpt
!----------------------------------------------------------------------------

TYPE(OneDimQuadratureOpt_) :: TypeOneDimQuadratureOpt = &
                              OneDimQuadratureOpt_()

!----------------------------------------------------------------------------
CONTAINS
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Copy the content from obj2 to obj

SUBROUTINE obj_Copy(obj, obj2)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  CLASS(OneDimQuadratureOpt_), INTENT(IN) :: obj2

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%quadratureType = obj2%quadratureType
  obj%alpha = obj2%alpha
  obj%beta = obj2%beta
  obj%lambda = obj2%lambda
  obj%order = obj2%order
  obj%nips = obj2%nips
  obj%quadratureType_char = obj2%quadratureType_char

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Copy

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Display the content of the object

SUBROUTINE obj_Display(obj, msg, unitNo)
  CLASS(OneDimQuadratureOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN), OPTIONAL :: msg
  !! Message to display
  INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
  !! Unit number for writing

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Display(msg, unitno=unitno)
  CALL Display(obj%quadratureType, "quadratureType: ", unitNo)
  CALL Display(TRIM(obj%quadratureType_char), "quadratureType_char: ", unitNo)
  CALL Display(obj%alpha, "alpha: ", unitNo)
  CALL Display(obj%beta, "beta: ", unitNo)
  CALL Display(obj%lambda, "lambda: ", unitNo)
  CALL Display(obj%order, "order: ", unitNo)
  CALL Display(obj%nips(1), "nips(1): ", unitNo)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                SetOneDimQuadratureOptParam
!----------------------------------------------------------------------------

SUBROUTINE SetOneDimQuadratureOptParam(param, prefix, quadratureType, &
                                       order, nips, alpha, beta, lambda)

  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN), OPTIONAL :: prefix
  INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
  INTEGER(I4B), INTENT(IN), OPTIONAL :: order
  INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(1)
  REAL(DFP), INTENT(IN), OPTIONAL :: alpha
  REAL(DFP), INTENT(IN), OPTIONAL :: beta
  REAL(DFP), INTENT(IN), OPTIONAL :: lambda

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetOneDimQuadratureOptParam()"
#endif
  INTEGER(I4B) :: aint, bint(1)
  REAL(DFP) :: areal

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! setting quadratureType
  aint = Input(default=TypeOneDimQuadratureOpt%quadratureType, &
               option=quadratureType)
  CALL Set(obj=param, prefix=prefix, key="quadratureType", &
           datatype=aint, VALUE=aint)

  ! setting order
  aint = Input(default=TypeOneDimQuadratureOpt%order, &
               option=order)
  CALL Set(obj=param, prefix=prefix, key="quadratureOrder", &
           datatype=aint, VALUE=aint)

  ! setting nips
  bint = Input(default=TypeOneDimQuadratureOpt%nips, &
               option=nips)
  CALL Set(obj=param, prefix=prefix, key="quadratureNips", &
           datatype=bint(1), VALUE=bint(1))

  ! setting alpha
  areal = Input(default=TypeOneDimQuadratureOpt%alpha, &
               option=alpha)
  CALL Set(obj=param, prefix=prefix, key="quadratureAlpha", &
           datatype=areal, value=areal)

  ! setting beta
  areal = Input(default=TypeOneDimQuadratureOpt%beta, &
               option=beta)
  CALL Set(obj=param, prefix=prefix, key="quadratureBeta", &
           datatype=areal, value=areal)

  ! setting lambda
  areal = Input(default=TypeOneDimQuadratureOpt%lambda, &
               option=lambda)
  CALL Set(obj=param, prefix=prefix, key="quadratureLambda", &
           datatype=areal, value=areal)


#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetOneDimQuadratureOptParam

!----------------------------------------------------------------------------
!                                                                SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-05
! summary: Sets the parameters for 1D quadrature options
!

SUBROUTINE obj_SetParam(obj, quadratureType, order, nips, alpha, beta, lambda)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
  INTEGER(I4B), INTENT(IN), OPTIONAL :: order
  INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(1)
  REAL(DFP), INTENT(IN), OPTIONAL :: alpha
  REAL(DFP), INTENT(IN), OPTIONAL :: beta
  REAL(DFP), INTENT(IN), OPTIONAL :: lambda

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! Set quadratureType if present
  IF (PRESENT(quadratureType)) THEN
    obj%quadratureType = quadratureType
    obj%quadratureType_char = QuadraturePoint_ToChar(obj%quadratureType, &
                                                     isUpper=.TRUE.)
  END IF

  ! Set order if present
  IF (PRESENT(order)) obj%order = order

  ! Set number of integration points if present
  IF (PRESENT(nips)) obj%nips = nips

  ! Set alpha parameter for Jacobi polynomials if present
  IF (PRESENT(alpha)) obj%alpha = alpha

  ! Set beta parameter for Jacobi polynomials if present
  IF (PRESENT(beta)) obj%beta = beta

  ! Set lambda parameter for Ultraspherical polynomials if present
  IF (PRESENT(lambda)) obj%lambda = lambda

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_SetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimQuadratureOpt_Class
