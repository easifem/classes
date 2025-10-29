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

SUBMODULE(VTKFile_Class) DataArrayMethods
USE penf, ONLY: str
USE InputUtility, ONLY: Input

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                              WriteDataArrayLocationTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArrayLocationTag
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArrayLocationTag()"
#endif

TYPE(String) :: location0, action0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

location0 = location%upper()
action0 = action%upper()

SELECT CASE (location0%chars())
CASE ('CELL')
  location0 = 'CellData'
CASE ('NODE')
  location0 = 'PointData'
END SELECT

SELECT CASE (obj%DataStructureType)
CASE (PARALLEL_VTK_RectilinearGrid, PARALLEL_VTK_StructuredGrid, &
      PARALLEL_VTK_UnstructuredGrid)
  location0 = 'P'//location0
END SELECT

SELECT CASE (action0%chars())
CASE ('OPEN')
  CALL obj%WriteStartTag(name=location0)
CASE ('CLOSE')
  CALL obj%WriteEndTag(name=location0)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArrayLocationTag

!----------------------------------------------------------------------------
!                                                        WriteDataArrrayTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArrayTag
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArrayTag()"
#endif

LOGICAL(LGT) :: abool
TYPE(String) :: names(5), values(5), astr
INTEGER(I4B) :: n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

names(1) = 'type'
values(1) = '"'//dataType//'"'
names(2) = 'NumberOfComponents'
values(2) = '"'//str(numberOfComponents, .TRUE.)//'"'
names(3) = 'Name'
values(3) = '"'//name//'"'
names(4) = 'format'
values(4) = '"'//TRIM(dataFormatName(obj%dataFormat))//'"'

abool = INPUT(default=.FALSE., option=isTuples)
IF (abool) names(2) = 'NumberOfTuples'

n = 4
abool = INPUT(Default=.FALSE., option=isOffset)
IF (abool) THEN
  n = 5
  names(5) = "offset"
  values(5) = '"'//str(obj%offset, .TRUE.)//'"'
  astr = 'DataArray'
  CALL obj%WriteSelfClosingTag( &
    name=astr, attrNames=names(1:n), attrValues=values(1:n))
ELSE
  astr = 'DataArray'
  CALL obj%WriteTag( &
    name=astr, attrNames=names(1:n), attrValues=Values(1:n), &
    content=content)
END IF

astr = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArrayTag

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Real32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank1_Real32()"
#endif

TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float32')
isOffset = .FALSE.
nByte = SIZE(x, 1) * BYReal32
noc = INPUT(default=1, option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank1_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Real64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank1_Real64()"
#endif

TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float64')
isOffset = .FALSE.
nByte = SIZE(x, 1) * BYReal64
noc = INPUT(default=1, option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank1_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank1_Int8()"
#endif

TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int8')
isOffset = .FALSE.
nByte = SIZE(x, 1) * BYInt8
noc = INPUT(default=1, option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank1_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank1_Int16()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int16')
isOffset = .FALSE.
nByte = SIZE(x, 1) * BYInt16
noc = INPUT(default=1, option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank1_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank1_Int32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int32')
isOffset = .FALSE.
nByte = SIZE(x, 1) * BYInt32
noc = INPUT(default=1, option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank1_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank1_Int64()"
#endif

TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int64')
isOffset = .FALSE.
nByte = SIZE(x, 1) * BYInt64
noc = INPUT(default=1, option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank1_Int64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Real32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank2_Real32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float32')
isOffset = .FALSE.
nByte = SIZE(x) * BYReal32
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank2_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Real64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank2_Real64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float64')
isOffset = .FALSE.
nByte = SIZE(x) * BYReal64
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank2_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Int8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank2_Int8()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int8')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt8
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank2_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Int16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank2_Int16()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int16')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt16
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank2_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Int32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank2_Int32()"
#endif

TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int32')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt32
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank2_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Int64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank2_Int64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int64')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt64
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE VTKFile_WriteDataArray_Rank2_Int64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Real32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank3_Real32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float32')
isOffset = .FALSE.
nByte = SIZE(x) * BYReal32
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE VTKFile_WriteDataArray_Rank3_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Real64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank3_Real64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float64')
isOffset = .FALSE.
nByte = SIZE(x) * BYReal64
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank3_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Int8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank3_Int8()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int8')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt8
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank3_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Int16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank3_Int16()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int16')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt16
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank3_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Int32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank3_Int32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int32')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt32
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank3_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Int64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank3_Int64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int64')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt64
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank3_Int64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Real32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank4_Real32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float32')
isOffset = .FALSE.
nByte = SIZE(x) * BYReal32
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank4_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Real64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank4_Real64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Float64')
isOffset = .FALSE.
nByte = SIZE(x) * BYReal64
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank4_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Int8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank4_Int8()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int8')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt8
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank4_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Int16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank4_Int16()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int16')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt16
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_Rank4_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Int32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank4_Int32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int32')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt32
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE VTKFile_WriteDataArray_Rank4_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Int64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_Rank4_Int64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int64')
isOffset = .FALSE.
nByte = SIZE(x) * BYInt64
noc = INPUT(default=SIZE(x, 1), option=numberOfComponents)
#include "./include/VTKFile_WriteDataArray_Rank1234.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE VTKFile_WriteDataArray_Rank4_Int64

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Real32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName= "VTKFile_WriteDataArray_XYZ_Rank1_Real32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float32')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal32
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Real32

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Real64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName= "VTKFile_WriteDataArray_XYZ_Rank1_Real64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Float64')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal64
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Real64

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank1_Int8()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int8')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt8
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int8

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank1_Int16()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int16')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt16
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int16

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank1_Int32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dataType = String('Int32')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt32
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int32

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank1_Int64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int64')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt64
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Real32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName= "VTKFile_WriteDataArray_XYZ_Rank2_Real32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Float32')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal32
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Real64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName= "VTKFile_WriteDataArray_XYZ_Rank2_Real64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Float64')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal64
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank2_Int8()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int8')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt8
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank2_Int16()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int16')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt16
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank2_Int32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int32')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt32
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank2_Int64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int64')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt64
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Real32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName= "VTKFile_WriteDataArray_XYZ_Rank3_Real32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Float32')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal32
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Real64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName= "VTKFile_WriteDataArray_XYZ_Rank3_Real64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Float64')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal64
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank3_Int8()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int8')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt8
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank3_Int16()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int16')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt16
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank3_Int32()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int32')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt32
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataArray_XYZ_Rank3_Int64()"
#endif
TYPE(String) :: dataType, content
INTEGER(I4B) :: noc, nByte
LOGICAL(LGT) :: isOffset
!> main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif
dataType = String('Int64')
isOffset = .FALSE.
nByte = (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt64
noc = 3
#include "./include/VTKFile_WriteDataArray_XYZ.F90"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

END SUBMODULE DataArrayMethods
