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

SUBMODULE(NewtonianFluidModel_Class) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE nfm_GetDynamicViscosity
DynamicViscosity = obj%Mu
END PROCEDURE nfm_GetDynamicViscosity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE nfm_GetModelParameters
INTEGER(I4B) :: ierr
IF (obj%isInitiated()) THEN
  ierr = param%set(key=myprefix//"/name", &
    & VALUE="NewtonianFluidModel")
  ierr = param%set(key=myprefix//"/dynamicViscosity", VALUE=obj%Mu)
END IF
END PROCEDURE nfm_GetModelParameters

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE nfm_GetPrefix
ans = myprefix
END PROCEDURE nfm_GetPrefix

!----------------------------------------------------------------------------
!                                                                 GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE nfm_GetParam
IF (PRESENT(dynamicViscosity)) dynamicViscosity = obj%mu
END PROCEDURE nfm_GetParam

END SUBMODULE GetMethods
