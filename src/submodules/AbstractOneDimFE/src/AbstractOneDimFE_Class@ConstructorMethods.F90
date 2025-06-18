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

SUBMODULE(AbstractOneDimFE_Class) ConstructorMethods
USE Display_Method, ONLY: ToString

USE RefElementFactory, ONLY: RefElement_Pointer

USE OneDimBasisOpt_Class, ONLY: SetOneDimBasisOptParam

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
#endif

CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

prefix = obj%GetPrefix()

CALL obj%opt%CheckEssentialParam(param=param, prefix=prefix)

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                   SetAbstractOneDimFEParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractOneDimFEParam
CHARACTER(*), PARAMETER :: myName = "SetAbstractOneDimFEParam()"

LOGICAL(LGT) :: isok
TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

sublist => NULL()
sublist => param%NewSubList(key=prefix)

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'cannot get sublist by using prefix='//prefix)

CALL SetOneDimBasisOptParam(param=sublist, &
                            prefix=prefix, &
                            order=order, &
                            baseContinuity=baseContinuity, &
                            baseInterpolation=baseInterpolation, &
                            ipType=ipType, &
                            basisType=basisType, &
                            alpha=alpha, &
                            beta=beta, &
                            lambda=lambda, &
                            feType=feType, &
                            quadratureType=quadratureType, &
                            quadratureOrder=quadratureOrder, &
                            quadratureNips=quadratureNips, &
                            quadratureAlpha=quadratureAlpha, &
                            quadratureBeta=quadratureBeta, &
                            quadratureLambda=quadratureLambda)

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetAbstractOneDimFEParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"

TYPE(ParameterList_), POINTER :: sublist
CHARACTER(:), ALLOCATABLE :: prefix
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

prefix = obj%GetPrefix()
sublist => NULL()
ierr = param%GetSubList(key=prefix, sublist=sublist)

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  "cannot get sublist by using prefix="//prefix)

CALL obj%DEALLOCATE()
CALL obj%opt%Initiate(param=sublist, prefix=prefix)
obj%isInitiated = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%Initiate(baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      ipType=ipType, basisType=basisType, alpha=alpha, &
                      beta=beta, lambda=lambda, order=order, fetype=fetype, &
                      quadratureType=quadratureType, &
                      quadratureOrder=quadratureOrder, &
                      quadratureNips=quadratureNips, &
                      quadratureAlpha=quadratureAlpha, &
                      quadratureBeta=quadratureBeta, &
                      quadratureLambda=quadratureLambda)

obj%isInitiated = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

INTEGER(I4B) :: s(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInitiated = obj2%isInitiated
CALL obj%opt%Copy(obj2%opt)

IF (ALLOCATED(obj2%coeff)) THEN
  s = SHAPE(obj2%coeff)
  obj%coeff(1:s(1), 1:s(2)) = obj2%coeff(1:s(1), 1:s(2))
END IF

IF (ALLOCATED(obj2%xij)) THEN
  s = SHAPE(obj2%xij)
  obj%xij(1:s(1), 1:s(2)) = obj2%xij(1:s(1), 1:s(2))
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInitiated = .FALSE.
CALL obj%opt%DEALLOCATE()
IF (ALLOCATED(obj%coeff)) DEALLOCATE (obj%coeff)
IF (ALLOCATED(obj%xij)) DEALLOCATE (obj%xij)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Ptr_Vector()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
