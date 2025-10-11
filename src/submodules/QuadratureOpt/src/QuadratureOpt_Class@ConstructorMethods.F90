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

SUBMODULE(QuadratureOpt_Class) ConstructorMethods
USE Display_Method, ONLY: Display, ToString
USE FPL_Method, ONLY: Set, GetValue
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar, &
                                  QuadraturePoint_ToInteger, &
                                  QuadraturePoint_Initiate => Initiate, &
                                  GetTotalQuadraturePoints, &
                                  InitiateFacetQuadrature
USE InputUtility, ONLY: Input
USE BaseType, ONLY: TypeElemNameOpt

USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_, &
                                    QuadratureNumber_Line

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Copy the content from obj2 to obj

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = obj2%isInit
obj%isHomogeneous = obj2%isHomogeneous
obj%isOrder = obj2%isOrder
obj%isNips = obj2%isNips
obj%topoType = obj2%topoType
obj%nsd = obj2%nsd
obj%xidim = obj2%xidim
obj%quadratureType = obj2%quadratureType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%refelemCoord = obj2%refelemCoord
obj%order = obj2%order
obj%nips = obj2%nips
obj%quadratureType_char = obj2%quadratureType_char
obj%refelemDomain = obj2%refelemDomain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
! Internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL GetValue(obj=param, prefix=prefix, key="isHomogeneous", &
              VALUE=obj%isHomogeneous)

IF (obj%isHomogeneous) THEN
  CALL InitiateFromParamHomogeneous(obj=obj, param=param, prefix=prefix)
ELSE
  CALL InitiateFromParamInHomogeneous(obj=obj, param=param, prefix=prefix)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                               InitiateFromParamHomogeneous
!----------------------------------------------------------------------------

SUBROUTINE InitiateFromParamHomogeneous(obj, param, prefix)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateFromParamHomogeneous()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(obj=param, prefix=prefix, key="isHomogeneous", &
                VALUE=obj%isHomogeneous)

  CALL GetValue(obj=param, prefix=prefix, key="topoType", VALUE=obj%topoType)

  CALL GetValue(obj=param, prefix=prefix, key="nsd", VALUE=obj%nsd)

  CALL GetValue(obj=param, prefix=prefix, key="quadratureType", &
                VALUE=obj%quadratureType(1))

  CALL GetValue(obj=param, prefix=prefix, key="alpha", VALUE=obj%alpha(1))

  CALL GetValue(obj=param, prefix=prefix, key="beta", VALUE=obj%beta(1))

  CALL GetValue(obj=param, prefix=prefix, key="lambda", VALUE=obj%lambda(1))

  CALL GetValue(obj=param, prefix=prefix, key="order", VALUE=obj%order(1))

  CALL GetValue(obj=param, prefix=prefix, key="nips", VALUE=obj%nips(1))

  CALL GetValue(obj=param, prefix=prefix, key="isNips", VALUE=obj%isNips)

  CALL GetValue(obj=param, prefix=prefix, key="isOrder", VALUE=obj%isOrder)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE InitiateFromParamHomogeneous

!----------------------------------------------------------------------------
!                                               InitiateFromParamHomogeneous
!----------------------------------------------------------------------------

SUBROUTINE InitiateFromParamInHomogeneous(obj, param, prefix)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateFromParamHomogenous()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(obj=param, prefix=prefix, key="isHomogeneous", &
                VALUE=obj%isHomogeneous)
  CALL GetValue(obj=param, prefix=prefix, key="isNips", VALUE=obj%isNips)
  CALL GetValue(obj=param, prefix=prefix, key="isOrder", VALUE=obj%isOrder)
  CALL GetValue(obj=param, prefix=prefix, key="topoType", VALUE=obj%topoType)
  CALL GetValue(obj=param, prefix=prefix, key="nsd", VALUE=obj%nsd)
  CALL GetValue(obj=param, prefix=prefix, key="quadratureType", &
                VALUE=obj%quadratureType)
  CALL GetValue(obj=param, prefix=prefix, key="alpha", VALUE=obj%alpha)
  CALL GetValue(obj=param, prefix=prefix, key="beta", VALUE=obj%beta)
  CALL GetValue(obj=param, prefix=prefix, key="lambda", VALUE=obj%lambda)
  CALL GetValue(obj=param, prefix=prefix, key="order", VALUE=obj%order)
  CALL GetValue(obj=param, prefix=prefix, key="nips", VALUE=obj%nips)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE InitiateFromParamInHomogeneous

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL obj%SetParam(isHomogeneous=isHomogeneous,quadratureType=quadratureType,&
                  order=order, isOrder=isOrder, nips=nips, isNips=isNips, &
                  alpha=alpha, beta=beta, lambda=lambda, topoType=topoType, &
                  nsd=nsd, xidim=xidim, refelemDomain=refelemDomain, &
                  refelemCoord=refelemCoord)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL obj%Copy(TypeQuadratureOpt)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
