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

SUBMODULE(AbstractSolidMechanicsModel_Class) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_GetElasticParam
CHARACTER(*), PARAMETER :: myName = "lem_GetElasticParam()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
END PROCEDURE lem_GetElasticParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_GetC
CHARACTER(*), PARAMETER :: myName = "lem_GetC()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
C = 0.0_DFP
END PROCEDURE lem_GetC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_GetInvC
CHARACTER(*), PARAMETER :: myName = "lem_GetInvC()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
InvC = 0.0_DFP
END PROCEDURE lem_GetInvC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_GetElasticityType
CHARACTER(*), PARAMETER :: myName = "lem_GetElasticityType()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
ans = 0
END PROCEDURE lem_GetElasticityType

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_GetPrefix
CHARACTER(*), PARAMETER :: myName = "lem_GetPrefix()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
ans = ""
END PROCEDURE lem_GetPrefix

!----------------------------------------------------------------------------
!                                                             isPlaneStress
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_isPlaneStress
ans = obj%isPStress
END PROCEDURE lem_isPlaneStress

!----------------------------------------------------------------------------
!                                                            isPlaneStrain
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_isPlaneStrain
ans = obj%isPStrain
END PROCEDURE lem_isPlaneStrain

END SUBMODULE GetMethods
