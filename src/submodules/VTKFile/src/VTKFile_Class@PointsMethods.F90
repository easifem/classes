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

SUBMODULE(VTKFile_Class) PointsMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                WritePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePoints_1
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePoints_1"
  SELECT CASE( obj%DataStructureType )
  !> Rectilinear Grid
  CASE( VTK_RectilinearGrid )
    CALL obj%WriteStartTag(name=String('Coordinates'))
    CALL obj%WriteDataArray(name=String('X'), x=x)
    CALL obj%WriteDataArray(name=String('Y'), x=y)
    CALL obj%WriteDataArray(name=String('Z'), x=z)
    CALL obj%WriteEndTag(name=String('Coordinates'))
  !> Structured Grid
  CASE( VTK_StructuredGrid, VTK_UnstructuredGrid, VTK_PolyData )
    CALL obj%WriteStartTag(name=String('Points'))
    CALL obj%WriteDataArray(name=String('Points'), x=x, y=y, z=z)
    CALL obj%WriteEndTag(name=String('Points'))

  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unknown DataStructureType')
  END SELECT
END PROCEDURE VTKFile_WritePoints_1

!----------------------------------------------------------------------------
!                                                                WritePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePoints_2
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePoints_1"
  SELECT CASE( obj%DataStructureType )
  !> Rectilinear Grid
  CASE( VTK_RectilinearGrid )
    CALL obj%WriteStartTag(name=String('Coordinates'))
    CALL obj%WriteDataArray(name=String('X'), x=x)
    CALL obj%WriteDataArray(name=String('Y'), x=y)
    CALL obj%WriteDataArray(name=String('Z'), x=z)
    CALL obj%WriteEndTag(name=String('Coordinates'))
  !> Structured Grid
  CASE( VTK_StructuredGrid, VTK_UnstructuredGrid, VTK_PolyData )
    CALL obj%WriteStartTag(name=String('Points'))
    CALL obj%WriteDataArray(name=String('Points'), x=x, y=y, z=z)
    CALL obj%WriteEndTag(name=String('Points'))

  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unknown DataStructureType')
  END SELECT
END PROCEDURE VTKFile_WritePoints_2

!----------------------------------------------------------------------------
!                                                                WritePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePoints_3
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePoints_3"
  !> main program
  SELECT CASE( obj%DataStructureType )
  CASE( VTK_RectilinearGrid )
    CALL obj%WriteStartTag(name=String('Coordinates'))
    CALL obj%WriteDataArray(name=String('X'), x=x( 1, : ) )
    CALL obj%WriteDataArray(name=String('Y'), x=x( 2, : ) )
    CALL obj%WriteDataArray(name=String('Z'), x=x( 3, : ) )
    CALL obj%WriteEndTag(name=String('Coordinates'))
  CASE( VTK_StructuredGrid, VTK_UnstructuredGrid, VTK_PolyData )
    CALL obj%WriteStartTag(name=String('Points'))
    CALL obj%WriteDataArray(name=String('Points'), x=x)
    CALL obj%WriteEndTag(name=String('Points'))
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unknown DataStructureType')
  END SELECT
END PROCEDURE VTKFile_WritePoints_3

!----------------------------------------------------------------------------
!                                                                WritePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePoints_4
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePoints_4"
  !> main program
  SELECT CASE( obj%DataStructureType )
  CASE( VTK_RectilinearGrid )
    CALL obj%WriteStartTag(name=String('Coordinates'))
    CALL obj%WriteDataArray(name=String('X'), x=x( 1, : ) )
    CALL obj%WriteDataArray(name=String('Y'), x=x( 2, : ) )
    CALL obj%WriteDataArray(name=String('Z'), x=x( 3, : ) )
    CALL obj%WriteEndTag(name=String('Coordinates'))
  CASE( VTK_StructuredGrid, VTK_UnstructuredGrid, VTK_PolyData )
    CALL obj%WriteStartTag(name=String('Points'))
    CALL obj%WriteDataArray(name=String('Points'), x=x)
    CALL obj%WriteEndTag(name=String('Points'))
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unknown DataStructureType')
  END SELECT
END PROCEDURE VTKFile_WritePoints_4

!----------------------------------------------------------------------------
!                                                                WritePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePoints_5
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePoints_5"
  !> main program
  SELECT CASE( obj%DataStructureType )
  CASE( VTK_StructuredGrid, VTK_UnstructuredGrid, VTK_PolyData )
    CALL obj%WriteStartTag(name=String('Points'))
    CALL obj%WriteDataArray(name=String('Points'), x=x, y=y, z=z)
    CALL obj%WriteEndTag(name=String('Points'))
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unknown DataStructureType')
  END SELECT
END PROCEDURE VTKFile_WritePoints_5

!----------------------------------------------------------------------------
!                                                                WritePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePoints_6
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePoints_6"
  !> main program
  SELECT CASE( obj%DataStructureType )
  CASE( VTK_StructuredGrid, VTK_UnstructuredGrid, VTK_PolyData )
    CALL obj%WriteStartTag(name=String('Points'))
    CALL obj%WriteDataArray(name=String('Points'), x=x, y=y, z=z)
    CALL obj%WriteEndTag(name=String('Points'))
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unknown DataStructureType')
  END SELECT
END PROCEDURE VTKFile_WritePoints_6

!----------------------------------------------------------------------------
!                                                                WritePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePoints_7
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePoints_7"
  !> main program
  SELECT CASE( obj%DataStructureType )
  CASE( VTK_StructuredGrid, VTK_UnstructuredGrid, VTK_PolyData )
    CALL obj%WriteStartTag(name=String('Points'))
    CALL obj%WriteDataArray(name=String('Points'), x=x )
    CALL obj%WriteEndTag(name=String('Points'))
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unknown DataStructureType')
  END SELECT
END PROCEDURE VTKFile_WritePoints_7

!----------------------------------------------------------------------------
!                                                                WritePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePoints_8
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePoints_8"
  !> main program
  SELECT CASE( obj%DataStructureType )
  CASE( VTK_StructuredGrid, VTK_UnstructuredGrid, VTK_PolyData )
    CALL obj%WriteStartTag(name=String('Points'))
    CALL obj%WriteDataArray(name=String('Points'), x=x )
    CALL obj%WriteEndTag(name=String('Points'))
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unknown DataStructureType')
  END SELECT
END PROCEDURE VTKFile_WritePoints_8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE PointsMethods