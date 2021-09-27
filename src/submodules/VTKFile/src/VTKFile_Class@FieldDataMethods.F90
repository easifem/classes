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

SUBMODULE( VTKFile_Class ) FieldDataMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            WriteFieldData
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteFieldData_1
  SELECT TYPE( x )
  TYPE IS( REAL( Real64 ) )
    CALL obj%WriteDataArray(name=name, x=[x], isTuples=.TRUE.)
  TYPE IS( REAL( Real32 ) )
    CALL obj%WriteDataArray(name=name, x=[x], isTuples=.TRUE.)
  TYPE IS( INTEGER( Int8 ) )
    CALL obj%WriteDataArray(name=name, x=[x], isTuples=.TRUE.)
  TYPE IS( INTEGER( Int16 ) )
    CALL obj%WriteDataArray(name=name, x=[x], isTuples=.TRUE.)
  TYPE IS( INTEGER( Int32 ) )
    CALL obj%WriteDataArray(name=name, x=[x], isTuples=.TRUE.)
#ifdef USE_Int64
  TYPE IS( INTEGER( Int64 ) )
    CALL obj%WriteDataArray(name=name, x=[x], isTuples=.TRUE.)
#endif
  END SELECT
END PROCEDURE VTKFile_WriteFieldData_1

!----------------------------------------------------------------------------
!                                                            WriteFieldData
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteFieldData_2
  TYPE( String ) :: act
  act = action%upper()
  SELECT CASE( TRIM(act%chars()) )
  CASE( 'OPEN' )
    CALL obj%WriteStartTag( name=String('FieldData') )
  CASE( 'CLOSE' )
    call obj%WriteEndTag( name=String('FieldData') )
  END SELECT
END PROCEDURE VTKFile_WriteFieldData_2

END SUBMODULE FieldDataMethods