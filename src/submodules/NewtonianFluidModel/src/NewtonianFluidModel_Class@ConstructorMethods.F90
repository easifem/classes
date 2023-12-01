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
!

SUBMODULE(NewtonianFluidModel_Class) ConstructorMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                SetNewtonianFluidModelParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetNewtonianFluidModelParam
INTEGER(I4B) :: ierr
ierr = param%Set(key=myprefix//"/name", &
  & VALUE="NewtonianFluidModel")
ierr = param%Set(key=myprefix//"/dynamicViscosity",  &
  & VALUE=DynamicViscosity)
END PROCEDURE SetNewtonianFluidModelParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE nfm_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "nfm_CheckEssentialParam"

IF (.NOT. param%isPresent(key=myprefix//"/dynamicViscosity")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/dynamicViscosity should be present in param')
END IF

IF (.NOT. param%isPresent(key=myprefix//"/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/name should be present in param')
END IF

END PROCEDURE nfm_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE nfm_Initiate
CHARACTER(*), PARAMETER :: myName = "nfm_Initiate"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Initiate()')
#endif

CALL obj%CheckEssentialParam(param)
CALL obj%SetIsInitiated(.TRUE.)
CALL obj%SetName(myprefix)
ierr = param%Get(key=myprefix//"/dynamicViscosity", VALUE=obj%mu)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Initiate()')
#endif
END PROCEDURE nfm_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE nfm_Deallocate
obj%mu = 0.0
CALL AbstractMaterialModelDeallocate(obj)
END PROCEDURE nfm_Deallocate

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE nfm_Final
CALL obj%DEALLOCATE()
END PROCEDURE nfm_Final

END SUBMODULE ConstructorMethods
