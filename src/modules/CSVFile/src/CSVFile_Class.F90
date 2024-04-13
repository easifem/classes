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

MODULE CSVFile_Class
USE GlobalData
USE String_Class
USE BaSetype
USE FortranFile_Class
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: hash = "#"
CHARACTER(*), PARAMETER :: comma = ","
CHARACTER(*), PARAMETER :: quote = '"'
CHARACTER(*), PARAMETER :: modName = 'CSVFile_Class'
INTEGER(I4B), PARAMETER :: maxStrLen = 256
INTEGER(I4B), PARAMETER, PUBLIC :: csv_type_string = 1
!! a character string cell
INTEGER(I4B), PARAMETER, PUBLIC :: csv_type_real = 2
!! a `real(wp)` cell
INTEGER(I4B), PARAMETER, PUBLIC :: csv_type_integer = 3
!! an `integer(ip)` cell
INTEGER(I4B), PARAMETER, PUBLIC :: csv_type_logical = 4
!! a logical cell

!----------------------------------------------------------------------------
!                                                                CSVFile_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2023
! summary: The main class for reading and writing CSV files.
!
!# Introduction
!
!@note
! A CSV file is assumed to contain the same number
! of columns in each row. It may optionally contain
! a header row.
!@endnote

TYPE, EXTENDS(TxtFile_) :: CSVFile_
  PRIVATE
  INTEGER(I4B) :: nrows = 0
    !! Number of rows
  INTEGER(I4B) :: ncols = 0
    !! number of columns
  INTEGER(I4B) :: chunk_size = MAX_CHUNK_SIZE
    !! for expanding vectors
  INTEGER(I4B) :: icol = 0
    !! Last column written in the current row
  LOGICAL(LGT) :: isQuotedStrings = .TRUE.
    !! Should we surround strings with quotation
  LOGICAL(LGT) :: isQuotedData = .FALSE.
    !! Should we surround all data with quotation
  CHARACTER(1) :: TrueChar = "T"
    !! Boolean true will be saved as "T" wo quotation
  CHARACTER(1) :: FalseChar = "F"
    !! Boolean false will be saved as "F" wo quotation
  TYPE(String), ALLOCATABLE :: header(:), DATA(:, :)
    !! header is csv file header
    !! data is the csv data
    !! size(header) = size(data, 2)
  INTEGER(I4B) :: headerIndx = 0
    !! header row number
  INTEGER(I4B), ALLOCATABLE :: skipRows(:)
    !! row numbers to skip
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => txt_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => txt_Deallocate
  FINAL :: txt_final

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetCSVFileProperties => &
    & txt_SetCSVFileProperties
  PROCEDURE, PUBLIC, PASS(obj) :: SetSkipRows => &
    & txt_SetSkipRows
  PROCEDURE, PUBLIC, PASS(obj) :: SetHeaderIndx => &
    & txt_SetHeaderIndx

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Getnrows => txt_Getnrows
  PROCEDURE, PUBLIC, PASS(obj) :: Getncols => txt_Getncols
  PROCEDURE, PUBLIC, PASS(obj) :: GetChunkSize => txt_GetChunkSize
  PROCEDURE, PUBLIC, PASS(obj) :: GetDataTypes => txt_GetDataTypes
  PROCEDURE, PUBLIC, PASS(obj) :: GetValue => txt_GetValue
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn => txt_GetColumn
  PROCEDURE, PUBLIC, PASS(obj) :: GetRealColumn => txt_GetRealColumn
  PROCEDURE, PUBLIC, PASS(obj) :: GetRealVectorColumn => &
    & txt_GetRealVectorColumn
  PROCEDURE, PUBLIC, PASS(obj) :: GetIntColumn => txt_GetIntColumn
  PROCEDURE, PUBLIC, PASS(obj) :: GetIntVectorColumn => &
    & txt_GetIntVectorColumn
  PROCEDURE, PUBLIC, PASS(obj) :: GetStringColumn => txt_GetStringColumn
  PROCEDURE, PUBLIC, PASS(obj) :: GetLogicalColumn => txt_GetLogicalColumn
  !!
  GENERIC, PUBLIC :: Get => GetValue, GetRealColumn, &
    & GetIntColumn, GetStringColumn, GetLogicalColumn, &
    & GetIntVectorColumn, GetRealVectorColumn

  ! IO:
  ! @ReadMethods
  PROCEDURE, PUBLIC, PASS(obj) :: csvFileRead => txt_CSVFileRead
  GENERIC, PUBLIC :: READ => csvFileRead
END TYPE CSVFile_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: CSVFile_
TYPE(CSVFile_), PUBLIC, PARAMETER :: TypeCSVFile = CSVFile_()

!----------------------------------------------------------------------------
!                                                      CSVFilePointer_
!----------------------------------------------------------------------------

TYPE :: CSVFilePointer_
  CLASS(CSVFile_), POINTER :: ptr => NULL()
END TYPE CSVFilePointer_

PUBLIC :: CSVFilePointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Initiate the txt file

INTERFACE
  MODULE SUBROUTINE txt_initiate(obj, filename, unit, status, access, form, &
    & position, action, pad, recl, comment, separator, delimiter)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unit
    !! User specified unit number, it should  not be `stdout, stdin, stderr`
    CHARACTER(*), OPTIONAL, INTENT(IN) :: status
    !! OLD, NEW, SCRATCH, REPLACE, UNKNOWN
    !! If UNKNOWN then we use REPLACE
    !! Default is REPLACE
    CHARACTER(*), OPTIONAL, INTENT(IN) :: access
    !! DIRECT, SEQUENTIAL, STREAM
    !! Default is SEQUENTIAL
    CHARACTER(*), OPTIONAL, INTENT(IN) :: form
    !! FORMATTED, UNFORMATTED
    !! Default is FORMATTED
    CHARACTER(*), OPTIONAL, INTENT(IN) :: position
    !! REWIND, APPEND, ASIS
    !! Default is ASIS
    CHARACTER(*), OPTIONAL, INTENT(IN) :: action
    !! READ, WRITE, READWRITE
    CHARACTER(*), OPTIONAL, INTENT(IN) :: pad
    !! YES, NO
    !! Default is YES
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: recl
    CHARACTER(*), OPTIONAL, INTENT(IN) :: comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: separator
    CHARACTER(*), OPTIONAL, INTENT(IN) :: delimiter
  END SUBROUTINE txt_initiate
END INTERFACE

INTERFACE CSVFileInitiate
  MODULE PROCEDURE txt_initiate
END INTERFACE CSVFileInitiate

PUBLIC :: CSVFileInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Deallocate the data

INTERFACE
  MODULE SUBROUTINE txt_Deallocate(obj, Delete)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: Delete
  END SUBROUTINE txt_Deallocate
END INTERFACE

INTERFACE CSVFileDeallocate
  MODULE PROCEDURE txt_Deallocate
END INTERFACE CSVFileDeallocate

PUBLIC :: CSVFileDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_final(obj)
    TYPE(CSVFile_), INTENT(INOUT) :: obj
  END SUBROUTINE txt_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getnrow@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION txt_getnrows(obj) RESULT(ans)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION txt_getnrows
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getncol@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION txt_getncols(obj) RESULT(ans)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION txt_getncols
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getChunkSize@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION txt_getChunkSize(obj) RESULT(ans)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION txt_getChunkSize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE txt_getDataTypes(obj, dataType)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: dataType(:)
  END SUBROUTINE txt_getDataTypes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_getValue(obj, irow, icol, val)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B), INTENT(IN) :: icol
    CLASS(*), INTENT(OUT) :: val
  END SUBROUTINE txt_getValue
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_getColumn(obj, icol, val)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: icol
    CLASS(*), INTENT(INOUT) :: val(:)
  END SUBROUTINE txt_getColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_getRealColumn(obj, icol, val)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: icol
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:)
  END SUBROUTINE txt_getRealColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_getRealVectorColumn(obj, icol, val)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: icol
    TYPE(RealVector_), INTENT(INOUT) :: val
  END SUBROUTINE txt_getRealVectorColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_getIntColumn(obj, icol, val)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: icol
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: val(:)
  END SUBROUTINE txt_getIntColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_getIntVectorColumn(obj, icol, val)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: icol
    TYPE(IntVector_), INTENT(INOUT) :: val
  END SUBROUTINE txt_getIntVectorColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_getStringColumn(obj, icol, val)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: icol
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: val(:)
  END SUBROUTINE txt_getStringColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_getLogicalColumn(obj, icol, val)
    CLASS(CSVFile_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: icol
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: val(:)
  END SUBROUTINE txt_getLogicalColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_setCSVFileProperties(obj, isQuotedStrings, &
    & isQuotedData, TrueChar, FalseChar, chunk_size, echostat, echounit)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isQuotedStrings
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isQuotedData
    CHARACTER(1), OPTIONAL, INTENT(IN) :: TrueChar
    CHARACTER(1), OPTIONAL, INTENT(IN) :: FalseChar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: chunk_size
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: echostat
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: echounit
  END SUBROUTINE txt_setCSVFileProperties
END INTERFACE

!----------------------------------------------------------------------------
!                                                   setSkipRows@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_setSkipRows(obj, indx)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
  END SUBROUTINE txt_setSkipRows
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setHeaderIndx@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_setHeaderIndx(obj, indx)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE txt_setHeaderIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Read@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary: This subroutine reads the `CSVFile_` file

INTERFACE
  MODULE SUBROUTINE txt_CSVFileRead(obj, iostat)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: iostat
  END SUBROUTINE txt_CSVFileRead
END INTERFACE
!----------------------------------------------------------------------------
!                                                     WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_write_Blank(obj)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    !! YES or NO
  END SUBROUTINE txt_write_Blank
END INTERFACE

!----------------------------------------------------------------------------
!                                                     WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_write_Line(obj, val, iostat, iomsg, advance)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(*), OPTIONAL, INTENT(IN) :: advance
    !! YES or NO
  END SUBROUTINE txt_write_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                     WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_write_Lines(obj, val, iostat, iomsg, advance)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(*), OPTIONAL, INTENT(IN) :: advance
    !! YES, NO
  END SUBROUTINE txt_write_Lines
END INTERFACE

!----------------------------------------------------------------------------
!                                                     WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_write_Char(obj, val, iostat, iomsg, &
    &  advance)
    CLASS(CSVFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(*), OPTIONAL, INTENT(IN) :: advance
    !! YES, NO
  END SUBROUTINE txt_write_Char
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write an integer

#define __SUBROUTINE_NAME__ txt_write_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a real value

#define __SUBROUTINE_NAME__ txt_write_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                         Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write an integer vector

#define __SUBROUTINE_NAME__ txt_write_vec_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_vec_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_vec_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_vec_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                         write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a intvector

#define __SUBROUTINE_NAME__ txt_write_IntVector
#define __DATA_TYPE__ TYPE( IntVector_ )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a vector of intvector

#define __SUBROUTINE_NAME__ txt_write_vec_IntVector
#define __DATA_TYPE__ TYPE( IntVector_ )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a real value vector

#define __SUBROUTINE_NAME__ txt_write_vec_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_vec_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                         write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a RealVector

#define __SUBROUTINE_NAME__ txt_write_RealVector
#define __DATA_TYPE__ TYPE( RealVector_ )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a vector of RealVector

#define __SUBROUTINE_NAME__ txt_write_vec_RealVector
#define __DATA_TYPE__ TYPE( RealVector_ )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                         Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write an integer matrix

#define __SUBROUTINE_NAME__ txt_write_mat_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_mat_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_mat_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_mat_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a real value vector

#define __SUBROUTINE_NAME__ txt_write_mat_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_mat_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION GetDataType(astr) RESULT(dataType)
    TYPE(String), INTENT(IN) :: astr
    INTEGER(I4B) :: dataType
  END FUNCTION GetDataType
END INTERFACE

END MODULE CSVFile_Class
