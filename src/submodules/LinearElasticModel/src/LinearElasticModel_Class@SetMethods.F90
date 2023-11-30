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

SUBMODULE(LinearElasticModel_Class) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
IF (PRESENT(elasticityType)) obj%elasticityType = elasticityType
IF (PRESENT(nu)) obj%nu = nu
IF (PRESENT(G)) obj%G = G
IF (PRESENT(youngsModulus)) obj%E = youngsModulus
IF (PRESENT(lambda)) obj%lambda = lambda
IF (PRESENT(C)) obj%C = C
IF (PRESENT(invC)) obj%invC = invC
IF (PRESENT(stiffnessPower)) obj%stiffnessPower = stiffnessPower
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 SetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetData
CHARACTER(*), PARAMETER :: myName = "obj_SetData()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by subclass.')
END PROCEDURE obj_SetData

!----------------------------------------------------------------------------
!                                                                 UpdateData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateData
CHARACTER(*), PARAMETER :: myName = "obj_UpdateData()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by subclass.')
END PROCEDURE obj_UpdateData

END SUBMODULE SetMethods
