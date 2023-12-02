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

SUBMODULE(AbstractKernel_Class) UpdateMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_Update
CHARACTER(*), PARAMETER :: myName = "ak_Update"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine is not available or has not been implemented')
END PROCEDURE ak_Update

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_UpdateIteration
CHARACTER(*), PARAMETER :: myName = "ak_UpdateIteration"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine is not available or has not been implemented')
END PROCEDURE ak_UpdateIteration

END SUBMODULE UpdateMethods
