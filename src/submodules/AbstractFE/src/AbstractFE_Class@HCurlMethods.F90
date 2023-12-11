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

SUBMODULE(AbstractFE_Class) HCurlMethods
! USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData_HCurl_Master
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemShapeData_HCurl()"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[WORK IN PROGRESS]')
!TODO: Implement get_GetLocalElemShapeData_HCurl_Master
END PROCEDURE obj_GetLocalElemShapeData_HCurl_Master

!----------------------------------------------------------------------------
!                                                    GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData_HCurl_Master
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData_HCurl()"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[WORK IN PROGRESS]')
!TODO: Implement get_GetGlobalElemShapeData_HCurl_Master
END PROCEDURE obj_GetGlobalElemShapeData_HCurl_Master

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE HCurlMethods
