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

SUBMODULE(AbstractMaterialModel_Class) GetMethods
IMPLICIT NONE
CONTAINS

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
!                                                                   GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetName
ans = obj%name%chars()
END PROCEDURE obj_GetName

!----------------------------------------------------------------------------
!                                                               isInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isInitiated
ans = obj%isInit
END PROCEDURE obj_isInitiated

!----------------------------------------------------------------------------
!                                                               GetDataSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDataSize
CHARACTER(*), PARAMETER :: myName = "obj_GetDataSize()"
ans = 0
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine must be implemented by subclass.')
END PROCEDURE obj_GetDataSize

!----------------------------------------------------------------------------
!                                                                    GetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetData
CHARACTER(*), PARAMETER :: myName = "obj_GetData()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine must be implemented by subclass.')
END PROCEDURE obj_GetData

END SUBMODULE GetMethods
