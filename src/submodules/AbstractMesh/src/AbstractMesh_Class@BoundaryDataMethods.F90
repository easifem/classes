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

SUBMODULE(AbstractMesh_Class) BoundaryDataMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateBoundaryData
CHARACTER(*), PARAMETER :: myName = "obj_InitiateBoundaryData()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

! check
IF (obj%isBoundaryDataInitiated) THEN
  CALL e%RaiseInformation(modName//"::"//myName//" - "// &
    & "Boundary data information is already initiated.")
  RETURN
END IF

CALL obj%InitiateElementToElements()

obj%isBoundaryDataInitiated = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_InitiateBoundaryData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE BoundaryDataMethods
