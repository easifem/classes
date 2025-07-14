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

SUBMODULE(AbstractFE_Class) ParamConstructorMethods
USE BasisOpt_Class, ONLY: SetBasisOptParam
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%CheckEssentialParam(param=param)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                       SetAbstractFEParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractFEParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetAbstractFEParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SetBasisOptParam(param=param, prefix=prefix, nsd=nsd, &
                      elemType=elemType, baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, ipType=ipType, &
                 basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, &
                    order=order, anisoOrder=anisoOrder, edgeOrder=edgeOrder, &
                    faceOrder=faceOrder, cellOrder=cellOrder, fetype=fetype, &
                      dofType=dofType, transformType=transformType)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetAbstractFEParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL obj%opt%Initiate(param=param)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                             Error handling
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ParamConstructorMethods
