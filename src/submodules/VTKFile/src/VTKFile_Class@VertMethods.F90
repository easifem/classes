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

SUBMODULE(VTKFile_Class) VertMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 WriteVerts
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteVerts
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteVerts()"
#endif

TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = "Verts"
CALL obj%WriteStartTag(name=astr)

astr = "connectivity"
CALL obj%WriteDataArray(name=astr, x=connectivity)

astr = "offsets"
CALL obj%WriteDataArray(name=astr, x=offsets)

astr = 'Verts'
CALL obj%WriteEndTag(name=astr)

astr = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteVerts

END SUBMODULE VertMethods
