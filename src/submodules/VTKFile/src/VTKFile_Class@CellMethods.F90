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

SUBMODULE(VTKFile_Class) CellMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                WriteCells
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteCells
  CALL obj%WriteStartTag(name=String('Cells'))
  CALL obj%WriteDataArray( name=String('connectivity'), x=connectivity )
  CALL obj%WriteDataArray( name=String('offsets'), x=offsets )
  CALL obj%WriteDataArray( name=String('types'), x=types )
  CALL obj%WriteEndTag(name=String('Cells'))
END PROCEDURE VTKFile_WriteCells

END SUBMODULE CellMethods