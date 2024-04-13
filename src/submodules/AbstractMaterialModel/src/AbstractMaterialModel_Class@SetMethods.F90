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

SUBMODULE(AbstractMaterialModel_Class) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           SetIsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIsInitiated
obj%isInit = VALUE
END PROCEDURE obj_SetIsInitiated

!----------------------------------------------------------------------------
!                                                                   SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetName
obj%name = VALUE
END PROCEDURE obj_SetName

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
