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
USE BaseType
USE FortranFile_Class
USE TxtFile_Class
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE

CHARACTER(LEN=*), PARAMETER :: hash="#"
CHARACTER(LEN=*), PARAMETER :: comma=","
CHARACTER(LEN=*), PARAMETER :: quote='"'
CHARACTER(LEN=*), PARAMETER :: modName = 'CSVFile_Class'
INTEGER(I4B), PARAMETER :: maxStrLen = 256
TYPE(ExceptionHandler_) :: e
INTEGER( I4B ), PARAMETER, PUBLIC :: csv_type_string  = 1
!! a character string cell
INTEGER( I4B ), PARAMETER, PUBLIC :: csv_type_real  = 2
!! a `real(wp)` cell
INTEGER( I4B ), PARAMETER, PUBLIC :: csv_type_integer = 3
!! an `integer(ip)` cell
INTEGER( I4B ), PARAMETER, PUBLIC :: csv_type_logical = 4
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
  INTEGER( I4B ) :: nrows=0
    !! Number of rows
  INTEGER( I4B ) :: ncols=0
    !! number of columns
  INTEGER( I4B ) :: chunk_size = MAX_CHUNK_SIZE
    !! for expanding vectors
  INTEGER( I4B ) :: icol = 0
    !! Last column written in the current row
  LOGICAL( LGT ) :: isQuotedStrings = .TRUE.
    !! Should we surround strings with quotation
  LOGICAL( LGT ) :: isQuotedData = .FALSE.
    !! Should we surround all data with quotation
  CHARACTER( LEN = 1 ) :: TrueChar = "T"
    !! Boolean true will be saved as "T" wo quotation
  CHARACTER( LEN = 1 ) :: FalseChar = "F"
    !! Boolean false will be saved as "F" wo quotation
  TYPE(String), ALLOCATABLE :: header( : ), data(:,:)
    !! header is csv file header
    !! data is the csv data
    !! size(header) = size(data, 2)
  INTEGER( I4B ) :: headerIndx = 0
    !! header row number
  INTEGER( I4B ), ALLOCATABLE :: skipRows( : )
    !! row numbers to skip
  CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: addSurrogate => txt_addSurrogate
  PROCEDURE, PUBLIC, PASS(obj) :: initiate => txt_initiate
  PROCEDURE, PUBLIC, PASS(obj) :: deallocate => txt_Deallocate
  FINAL :: txt_final
  !!
  !! @SetMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: setCSVFileProperties => &
    & txt_setCSVFileProperties
  PROCEDURE, PUBLIC, PASS( obj ) :: setSkipRows => &
    & txt_setSkipRows
  PROCEDURE, PUBLIC, PASS( obj ) :: setHeaderIndx => &
    & txt_setHeaderIndx
  !!
  !! @GetMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: getnrows => txt_getnrows
  PROCEDURE, PUBLIC, PASS( obj ) :: getncols => txt_getncols
  PROCEDURE, PUBLIC, PASS( obj ) :: getChunkSize => txt_getChunkSize
  PROCEDURE, PUBLIC, PASS( obj ) :: getDataTypes => txt_getDataTypes
  PROCEDURE, PUBLIC, PASS( obj ) :: getValue => txt_getValue
  PROCEDURE, PUBLIC, PASS( obj ) :: getColumn => txt_getColumn
  PROCEDURE, PUBLIC, PASS( obj ) :: getRealColumn => txt_getRealColumn
  PROCEDURE, PUBLIC, PASS( obj ) :: getRealVectorColumn => &
    & txt_getRealVectorColumn
  PROCEDURE, PUBLIC, PASS( obj ) :: getIntColumn => txt_getIntColumn
  PROCEDURE, PUBLIC, PASS( obj ) :: getIntVectorColumn => &
    & txt_getIntVectorColumn
  PROCEDURE, PUBLIC, PASS( obj ) :: getStringColumn => txt_getStringColumn
  PROCEDURE, PUBLIC, PASS( obj ) :: getLogicalColumn => txt_getLogicalColumn
  !!
  GENERIC, PUBLIC :: get => getValue, getRealColumn, &
    & getIntColumn, getStringColumn, getLogicalColumn, &
    & getIntVectorColumn, getRealVectorColumn
  !!
  !! @ReadMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: csvFileRead => txt_CSVFileRead
  GENERIC, PUBLIC :: read => csvFileRead
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
  CLASS( CSVFile_ ), POINTER :: ptr => NULL()
END TYPE CSVFilePointer_

PUBLIC :: CSVFilePointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./ConstructorMethods.inc"
#include "./SetMethods.inc"
#include "./GetMethods.inc"
#include "./ReadMethods.inc"
#include "./WriteMethods.inc"
#include "./AuxilaryMethods.inc"

END MODULE CSVFile_Class