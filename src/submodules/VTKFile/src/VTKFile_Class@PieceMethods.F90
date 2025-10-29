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

SUBMODULE(VTKFile_Class) PieceMethods
USE penf, ONLY: str

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePiece_1
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = "VTKFile_WritePiece_1()"
LOGICAL(LGT) :: isok
#endif

TYPE(String) :: names(2), values(2), astr
INTEGER(I4B) :: n, extent0(6), tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

extent0 = 0
tsize = SIZE(extent)
extent0(1:tsize) = extent(:)

SELECT CASE (obj%DataStructureType)

CASE (PARALLEL_VTK_RECTILINEARGRID, PARALLEL_VTK_STRUCTUREDGRID)

#ifdef DEBUG_VER
  isok = PRESENT(srcFileName)
  CALL AssertError1(isok, myName, &
     'Source file name should be present for Parallel &
     &RectilinearGrid/StructuredGrid')
#endif

  n = 2
  names(1) = 'Extent'
  values(1) = '"'//str(n=extent0(1))//CHAR_SPACE &
              //str(n=extent0(2))//CHAR_SPACE &
              //str(n=extent0(3))//CHAR_SPACE &
              //str(n=extent0(4))//CHAR_SPACE &
              //str(n=extent0(5))//CHAR_SPACE &
              //str(n=extent0(6))//'"'

  names(2) = 'Source'

  values(2) = '"'//TRIM(ADJUSTL(srcFileName))//'"'

CASE (PARALLEL_VTK_UnstructuredGrid, PARALLEL_VTK_PolyData)

#ifdef DEBUG_VER
  isok = PRESENT(srcFileName)
  CALL AssertError1(isok, myName, &
     'Source file name should be present for Parallel &
     &UnstructuredGrid/PolyData')
#endif

  n = 1
  names(1) = 'Source'
  values(1) = '"'//TRIM(ADJUSTL(srcFileName))//'"'

CASE (VTK_RectilinearGrid, VTK_StructuredGrid, VTK_ImageData)

  n = 1
  names(1) = 'Extent'
  values(1) = '"' &
              //str(n=extent0(1))//CHAR_SPACE &
              //str(n=extent0(2))//CHAR_SPACE &
              //str(n=extent0(3))//CHAR_SPACE &
              //str(n=extent0(4))//CHAR_SPACE &
              //str(n=extent0(5))//CHAR_SPACE &
              //str(n=extent0(6))//'"'

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for DataStructureType')
#endif
END SELECT

astr = String('Piece')
CALL obj%WriteStartTag(name=astr, attrNames=names(1:n), &
                       attrValues=values(1:n))

astr = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WritePiece_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePiece_2
CHARACTER(LEN=*), PARAMETER :: myName = "VTKFile_WritePiece_2"
TYPE(String) :: names(2), values(2)
INTEGER(I4B) :: n
!> main
SELECT CASE (obj%DataStructureType)
CASE (VTK_UnstructuredGrid)
  n = 2
  names(1) = 'NumberOfPoints'
  names(2) = 'NumberOfCells'
  values(1) = '"'//str(nPoints)//'"'
  values(2) = '"'//str(nCells)//'"'
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
                    ' - Unknown DataStructureType')
END SELECT
CALL obj%WriteStartTag(name=String('Piece'), attrNames=names(1:n), &
                       attrValues=values(1:n))
END PROCEDURE VTKFile_WritePiece_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePiece_3
CHARACTER(LEN=*), PARAMETER :: myName = "VTKFile_WritePiece_3"
TYPE(String) :: names(5), values(5)
INTEGER(I4B) :: n
!> main
SELECT CASE (obj%DataStructureType)
CASE (VTK_PolyData)
  n = 5
  names(1) = 'NumberOfPoints'
  names(2) = 'NumberOfVerts'
  names(3) = 'NumberOfLines'
  names(4) = 'NumberOfStrips'
  names(5) = 'NumberOfPolys'
  values(1) = '"'//str(nPoints)//'"'
  values(2) = '"'//str(nVerts)//'"'
  values(3) = '"'//str(nLines)//'"'
  values(4) = '"'//str(nStrips)//'"'
  values(5) = '"'//str(nPolys)//'"'
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
                    ' - Unknown DataStructureType')
END SELECT

CALL obj%WriteStartTag(name=String('Piece'), attrNames=names(1:n), &
                       attrValues=values(1:n))
END PROCEDURE VTKFile_WritePiece_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WritePiece_4
CALL obj%WriteEndTag(name=String('Piece'))
END PROCEDURE VTKFile_WritePiece_4

!----------------------------------------------------------------------------
! Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE PieceMethods

