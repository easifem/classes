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

SUBMODULE( VTKFile_Class ) PieceMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePiece_1
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePiece_1"
  TYPE( String ) :: names( 2 ), values( 2 )
  INTEGER( I4B ) :: n
  !> main
  SELECT CASE( obj%DataStructureType )
  CASE( PARALLEL_VTK_RectilinearGrid, PARALLEL_VTK_StructuredGrid )
    IF( .NOT. PRESENT( srcFileName ) ) THEN
      CALL e%raiseError(modName//'::'//myName// &
      & ' - Source file name should be present!')
    END IF
    n = 2
    names( 1 ) = 'Extent'
    values( 1 ) = '"' // TRIM(str(n=extent(1))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(2))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(3))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(4))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(5))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(6))) // '"'
    names( 2 ) = 'Source'
    values( 2 ) = '"' // TRIM(ADJUSTL(srcFileName)) // '"'
  CASE( PARALLEL_VTK_UnstructuredGrid, PARALLEL_VTK_PolyData )
    IF( .NOT. PRESENT( srcFileName ) ) THEN
      CALL e%raiseError(modName//'::'//myName// &
      & ' - Source file name should be present!')
    END IF
    n = 1
    names( 1 ) = 'Source'
    values( 1 ) = '"' // TRIM(ADJUSTL(srcFileName)) // '"'
  CASE( VTK_RectilinearGrid, VTK_StructuredGrid, VTK_ImageData )
    n = 1
    names( 1 ) = 'Extent'
    values( 1 ) = '"' // TRIM(str(n=extent(1))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(2))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(3))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(4))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(5))) // CHAR_SPACE &
                    & // TRIM(str(n=extent(6))) // '"'
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName// &
      & ' - Unknown DataStructureType')
  END SELECT
  CALL obj%WriteStartTag(name=String('Piece'), attrNames=names(1:n), &
    & attrValues=values(1:n) )
END PROCEDURE VTKFile_WritePiece_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePiece_2
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePiece_2"
  TYPE( String ) :: names( 2 ), values( 2 )
  INTEGER( I4B ) :: n
  !> main
  SELECT CASE( obj%DataStructureType )
  CASE( VTK_UnstructuredGrid )
    n = 2
    names( 1 ) = 'NumberOfPoints'
    names( 2 ) = 'NumberOfCells'
    values( 1 ) = '"' // TRIM( str(nPoints) ) // '"'
    values( 2 ) = '"' // TRIM( str(nCells) ) // '"'
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName// &
      & ' - Unknown DataStructureType')
  END SELECT
  CALL obj%WriteStartTag(name=String('Piece'), attrNames=names(1:n), &
    & attrValues=values(1:n) )
END PROCEDURE VTKFile_WritePiece_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePiece_3
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_WritePiece_3"
  TYPE( String ) :: names( 5 ), values( 5 )
  INTEGER( I4B ) :: n
  !> main
  SELECT CASE( obj%DataStructureType )
  CASE( VTK_PolyData )
    n = 5
    names( 1 ) = 'NumberOfPoints'
    names( 2 ) = 'NumberOfVerts'
    names( 3 ) = 'NumberOfLines'
    names( 4 ) = 'NumberOfStrips'
    names( 5 ) = 'NumberOfPolys'
    values( 1 ) = '"' // TRIM( str(nPoints) ) // '"'
    values( 2 ) = '"' // TRIM( str(nVerts) ) // '"'
    values( 3 ) = '"' // TRIM( str(nLines) ) // '"'
    values( 4 ) = '"' // TRIM( str(nStrips) ) // '"'
    values( 5 ) = '"' // TRIM( str(nPolys) ) // '"'
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName// &
      & ' - Unknown DataStructureType')
  END SELECT
  CALL obj%WriteStartTag(name=String('Piece'), attrNames=names(1:n), &
    & attrValues=values(1:n) )
END PROCEDURE VTKFile_WritePiece_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePiece_4
  CALL obj%WriteEndTag(name=String('Piece'))
END PROCEDURE VTKFile_WritePiece_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE PieceMethods