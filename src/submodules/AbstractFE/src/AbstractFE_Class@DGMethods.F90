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

SUBMODULE(AbstractFE_Class) DGMethods
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 GetLocalElemShapeData_DG
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData_DG_Master
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemShapeData_DG()"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[WORK IN PROGRESS]')
!TODO: Implement obj_GetLocalElemShapeData_DG_Master
END PROCEDURE obj_GetLocalElemShapeData_DG_Master

!----------------------------------------------------------------------------
!                                                 GetGlobalElemShapeData_DG
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData_DG_Master
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData_DG()"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[WORK IN PROGRESS]')
!TODO: Implement obj_GetGlobalElemShapeData_DG_Master
END PROCEDURE obj_GetGlobalElemShapeData_DG_Master

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DGMethods
