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

MODULE PROCEDURE obj_GetElasticParam
CHARACTER(*), PARAMETER :: myName = "obj_GetElasticParam()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
END PROCEDURE obj_GetElasticParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetC
CHARACTER(*), PARAMETER :: myName = "obj_GetC()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
C = 0.0_DFP
END PROCEDURE obj_GetC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInvC
CHARACTER(*), PARAMETER :: myName = "obj_GetInvC()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
InvC = 0.0_DFP
END PROCEDURE obj_GetInvC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElasticityType
CHARACTER(*), PARAMETER :: myName = "obj_GetElasticityType()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
ans = 0
END PROCEDURE obj_GetElasticityType

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & 'AbstractSolidMechanicsModel_')
ans = ""
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                             isPlaneStress
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isPlaneStress
ans = obj%isPStress
END PROCEDURE obj_isPlaneStress

!----------------------------------------------------------------------------
!                                                            isPlaneStrain
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isPlaneStrain
ans = obj%isPStrain
END PROCEDURE obj_isPlaneStrain

END SUBMODULE GetMethods
