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
!> authors: Vikas Sharma, Ph. D.
! date: 2 May 2021
! summary: module for I/O defines the derived type for a Fortran File object.

MODULE TxtFile_Class
USE GlobalData
USE String_Class
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE FortranFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = 'TxtFile_Class'
INTEGER(I4B), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!                                                                 TxtFile_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July, 2022
! summary: TxtFile is extension of FortranFile
!
!# Introduction
!
! TxtFile is an extension of the FortranFile.
! It stores data in ASCII format.

TYPE, EXTENDS(FortranFile_) :: TxtFile_
  PRIVATE
  LOGICAL(LGT) :: echostat = .FALSE.
  INTEGER(I4B) :: echounit = -1
  !
CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: initiate => txt_initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => txt_Deallocate
  FINAL :: txt_final
  !!
  !! @EnquireMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: isValidRecord => txt_isValidRecord
  !!
  !! @SetMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: setEchoStat => txt_setEchoStat
  PROCEDURE, PUBLIC, PASS(obj) :: setEchoUnit => txt_setEchoUnit
  !!
  !! @GetMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: getEchoStat => txt_getEchoStat
  PROCEDURE, PUBLIC, PASS(obj) :: getEchoUnit => txt_getEchoUnit
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalRecords => txt_getTotalRecords
  !!
  !! @ReadMethods
  !!
  !! read strings and chars
  PROCEDURE, PUBLIC, PASS(obj) :: readLine => txt_read_Line
  PROCEDURE, PUBLIC, PASS(obj) :: readLines => txt_read_Lines
  PROCEDURE, PASS(obj) :: readChar => txt_read_Char
  !! scalars
  PROCEDURE, PASS(obj) :: readInt8 => txt_read_Int8
  PROCEDURE, PASS(obj) :: readInt16 => txt_read_Int16
  PROCEDURE, PASS(obj) :: readInt32 => txt_read_Int32
  PROCEDURE, PASS(obj) :: readInt64 => txt_read_Int64
  PROCEDURE, PASS(obj) :: readReal32 => txt_read_Real32
  PROCEDURE, PASS(obj) :: readReal64 => txt_read_Real64
  !! vectors
  PROCEDURE, PASS(obj) :: readVecInt8 => txt_read_vec_Int8
  PROCEDURE, PASS(obj) :: readVecInt16 => txt_read_vec_Int16
  PROCEDURE, PASS(obj) :: readVecInt32 => txt_read_vec_Int32
  PROCEDURE, PASS(obj) :: readVecInt64 => txt_read_vec_Int64
  PROCEDURE, PASS(obj) :: readIntVector => txt_read_IntVector
  PROCEDURE, PASS(obj) :: readVecIntVector => txt_read_vec_IntVector
  PROCEDURE, PASS(obj) :: readVecReal32 => txt_read_vec_Real32
  PROCEDURE, PASS(obj) :: readVecReal64 => txt_read_vec_Real64
  PROCEDURE, PASS(obj) :: readRealVector => txt_read_RealVector
  PROCEDURE, PASS(obj) :: readVecRealVector => txt_read_vec_RealVector
  !! matrix
  PROCEDURE, PASS(obj) :: readMatReal32 => txt_read_Mat_Real32
  PROCEDURE, PASS(obj) :: readMatReal64 => txt_read_Mat_Real64
  PROCEDURE, PASS(obj) :: readMatInt8 => txt_read_Mat_Int8
  PROCEDURE, PASS(obj) :: readMatInt16 => txt_read_Mat_Int16
  PROCEDURE, PASS(obj) :: readMatInt32 => txt_read_Mat_Int32
  PROCEDURE, PASS(obj) :: readMatInt64 => txt_read_Mat_Int64
  !! generic
  GENERIC, PUBLIC :: read => &
    & readLine, readLines, readChar, &
    & readInt8, readInt16, readInt32, readInt64, &
    & readReal32, readReal64, &
    & readVecInt8, readVecInt16, readVecInt32, readVecInt64, &
    & readIntVector, readVecIntVector, &
    & readVecReal32, readVecReal64, &
    & readRealVector, readVecRealVector, &
    & readMatInt8, readMatInt16, readMatInt32, readMatInt64, &
    & readMatReal32, readMatReal64
  !!
  !! @WriteMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: convertMarkdownToSource => &
    & txt_convertMarkDownToSource
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: writeBlank => txt_write_Blank
  PROCEDURE, PUBLIC, PASS(obj) :: nextRow => txt_write_Blank
  PROCEDURE, PUBLIC, PASS(obj) :: writeLine => txt_write_Line
  PROCEDURE, PUBLIC, PASS(obj) :: writeLines => txt_write_Lines
  PROCEDURE, PASS(obj) :: writeChar => txt_write_Char
  !! scalars
  PROCEDURE, PASS(obj) :: writeInt8 => txt_write_Int8
  PROCEDURE, PASS(obj) :: writeInt16 => txt_write_Int16
  PROCEDURE, PASS(obj) :: writeInt32 => txt_write_Int32
  PROCEDURE, PASS(obj) :: writeInt64 => txt_write_Int64
  PROCEDURE, PASS(obj) :: writeReal32 => txt_write_Real32
  PROCEDURE, PASS(obj) :: writeReal64 => txt_write_Real64
  !! vectors
  PROCEDURE, PASS(obj) :: writeVecInt8 => txt_write_vec_Int8
  PROCEDURE, PASS(obj) :: writeVecInt16 => txt_write_vec_Int16
  PROCEDURE, PASS(obj) :: writeVecInt32 => txt_write_vec_Int32
  PROCEDURE, PASS(obj) :: writeVecInt64 => txt_write_vec_Int64
  PROCEDURE, PASS(obj) :: writeIntVector => txt_write_IntVector
  PROCEDURE, PASS(obj) :: writeVecIntVector => txt_write_vec_IntVector
  PROCEDURE, PASS(obj) :: writeVecReal32 => txt_write_vec_Real32
  PROCEDURE, PASS(obj) :: writeVecReal64 => txt_write_vec_Real64
  PROCEDURE, PASS(obj) :: writeRealVector => txt_write_RealVector
  PROCEDURE, PASS(obj) :: writeVecRealVector => txt_write_vec_RealVector
  !! matrix
  PROCEDURE, PASS(obj) :: writeMatReal32 => txt_write_Mat_Real32
  PROCEDURE, PASS(obj) :: writeMatReal64 => txt_write_Mat_Real64
  PROCEDURE, PASS(obj) :: writeMatInt8 => txt_write_Mat_Int8
  PROCEDURE, PASS(obj) :: writeMatInt16 => txt_write_Mat_Int16
  PROCEDURE, PASS(obj) :: writeMatInt32 => txt_write_Mat_Int32
  PROCEDURE, PASS(obj) :: writeMatInt64 => txt_write_Mat_Int64
  !! generic
  GENERIC, PUBLIC :: write => &
    & writeBlank, &
    & writeLine, writeLines, writeChar, &
    & writeInt8, writeInt16, writeInt32, writeInt64, &
    & writeVecInt8, writeVecInt16, writeVecInt32, writeVecInt64, &
    & writeMatInt8, writeMatInt16, writeMatInt32, writeMatInt64, &
    & writeIntVector, writeVecIntVector, &
    & writeReal32, writeReal64, &
    & writeVecReal32, writeVecReal64, &
    & writeRealVector, writeVecRealVector, &
    & writeMatReal32, writeMatReal64
END TYPE TxtFile_

PUBLIC :: TxtFile_

TYPE(TxtFile_), PUBLIC, PARAMETER :: TypeTxtFile = TxtFile_()

!----------------------------------------------------------------------------
!                                                             TxtFilePointer
!----------------------------------------------------------------------------

TYPE :: TxtFilePointer_
  CLASS(TxtFile_), POINTER :: ptr => NULL()
END TYPE

PUBLIC :: TxtFilePointer_

#include "./ConstructorMethods.inc"
#include "./EnquireMethods.inc"
#include "./SetMethods.inc"
#include "./GetMethods.inc"
#include "./ReadMethods.inc"
#include "./WriteMethods.inc"

END MODULE TxtFile_Class
