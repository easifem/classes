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
CHARACTER(*), PARAMETER :: modName = 'TxtFile_Class'
INTEGER(I4B), PARAMETER :: maxStrLen = 256
PUBLIC :: TxtFilePointer_
PUBLIC :: TxtFile_
PUBLIC :: TypeTxtFile
PUBLIC :: TxtFileInitiate
PUBLIC :: TxtFileDeallocate
PUBLIC :: TxtFileWrite

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

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => txt_initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => txt_Deallocate
  FINAL :: txt_final

  ! GET:
  ! @EnquireMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsValidRecord => txt_IsValidRecord

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetEchoStat => txt_SetEchoStat
  PROCEDURE, PUBLIC, PASS(obj) :: SetEchoUnit => txt_SetEchoUnit

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetEchoStat => txt_GetEchoStat
  PROCEDURE, PUBLIC, PASS(obj) :: GetEchoUnit => txt_GetEchoUnit
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalRecords => txt_GetTotalRecords
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalData => txt_GetTotalData

  ! IO:
  ! @ReadMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ReadLine => txt_Read_Line
  !! Read strings and chars
  PROCEDURE, PUBLIC, PASS(obj) :: ReadLines => txt_Read_Lines
  !! Read strings and chars
  PROCEDURE, PASS(obj) :: ReadChar => txt_Read_Char
  !! scalars
  PROCEDURE, PASS(obj) :: ReadInt8 => txt_Read_Int8
  PROCEDURE, PASS(obj) :: ReadInt16 => txt_Read_Int16
  PROCEDURE, PASS(obj) :: ReadInt32 => txt_Read_Int32
  PROCEDURE, PASS(obj) :: ReadInt64 => txt_Read_Int64
  PROCEDURE, PASS(obj) :: ReadReal32 => txt_Read_Real32
  PROCEDURE, PASS(obj) :: ReadReal64 => txt_Read_Real64
  PROCEDURE, PASS(obj) :: ReadBoolean => txt_Read_boolean
  !! vectors
  PROCEDURE, PASS(obj) :: ReadVecInt8 => txt_Read_vec_Int8
  PROCEDURE, PASS(obj) :: ReadVecInt16 => txt_Read_vec_Int16
  PROCEDURE, PASS(obj) :: ReadVecInt32 => txt_Read_vec_Int32
  PROCEDURE, PASS(obj) :: ReadVecInt64 => txt_Read_vec_Int64
  PROCEDURE, PASS(obj) :: ReadIntVector => txt_Read_IntVector
  PROCEDURE, PASS(obj) :: ReadVecIntVector => txt_Read_vec_IntVector
  PROCEDURE, PASS(obj) :: ReadVecReal32 => txt_Read_vec_Real32
  PROCEDURE, PASS(obj) :: ReadVecReal64 => txt_Read_vec_Real64
  PROCEDURE, PASS(obj) :: ReadRealVector => txt_Read_RealVector
  PROCEDURE, PASS(obj) :: ReadVecRealVector => txt_Read_vec_RealVector
  PROCEDURE, PASS(obj) :: ReadVecBoolean => txt_Read_vec_boolean
  !! matrix
  PROCEDURE, PASS(obj) :: ReadMatReal32 => txt_Read_Mat_Real32
  PROCEDURE, PASS(obj) :: ReadMatReal64 => txt_Read_Mat_Real64
  PROCEDURE, PASS(obj) :: ReadMatInt8 => txt_Read_Mat_Int8
  PROCEDURE, PASS(obj) :: ReadMatInt16 => txt_Read_Mat_Int16
  PROCEDURE, PASS(obj) :: ReadMatInt32 => txt_Read_Mat_Int32
  PROCEDURE, PASS(obj) :: ReadMatInt64 => txt_Read_Mat_Int64
  !! generic
  GENERIC, PUBLIC :: READ => ReadLine, ReadLines, ReadChar, ReadBoolean, &
    ReadInt8, ReadInt16, ReadInt32, ReadInt64, ReadReal32, ReadReal64, &
    ReadVecInt8, ReadVecInt16, ReadVecInt32, ReadVecInt64, &
    ReadIntVector, ReadVecIntVector, ReadVecBoolean, &
    ReadVecReal32, ReadVecReal64, ReadRealVector, ReadVecRealVector, &
    ReadMatInt8, ReadMatInt16, ReadMatInt32, ReadMatInt64, &
    ReadMatReal32, ReadMatReal64

  ! IO:
  ! @WriteMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ConvertMarkdownToSource => &
    & txt_ConvertMarkDownToSource
  PROCEDURE, PUBLIC, PASS(obj) :: WriteBlank => txt_Write_Blank
  PROCEDURE, PUBLIC, PASS(obj) :: nextRow => txt_Write_Blank
  PROCEDURE, PUBLIC, PASS(obj) :: WriteLine => txt_Write_Line
  PROCEDURE, PUBLIC, PASS(obj) :: WriteLines => txt_Write_Lines
  PROCEDURE, PASS(obj) :: WriteChar => txt_Write_Char
  !! scalars
  PROCEDURE, PASS(obj) :: WriteInt8 => txt_Write_Int8
  PROCEDURE, PASS(obj) :: WriteInt16 => txt_Write_Int16
  PROCEDURE, PASS(obj) :: WriteInt32 => txt_Write_Int32
  PROCEDURE, PASS(obj) :: WriteInt64 => txt_Write_Int64
  PROCEDURE, PASS(obj) :: WriteReal32 => txt_Write_Real32
  PROCEDURE, PASS(obj) :: WriteReal64 => txt_Write_Real64
  !! vectors
  PROCEDURE, PASS(obj) :: WriteVecInt8 => txt_Write_vec_Int8
  PROCEDURE, PASS(obj) :: WriteVecInt16 => txt_Write_vec_Int16
  PROCEDURE, PASS(obj) :: WriteVecInt32 => txt_Write_vec_Int32
  PROCEDURE, PASS(obj) :: WriteVecInt64 => txt_Write_vec_Int64
  PROCEDURE, PASS(obj) :: WriteIntVector => txt_Write_IntVector
  PROCEDURE, PASS(obj) :: WriteVecIntVector => txt_Write_vec_IntVector
  PROCEDURE, PASS(obj) :: WriteVecReal32 => txt_Write_vec_Real32
  PROCEDURE, PASS(obj) :: WriteVecReal64 => txt_Write_vec_Real64
  PROCEDURE, PASS(obj) :: WriteRealVector => txt_Write_RealVector
  PROCEDURE, PASS(obj) :: WriteVecRealVector => txt_Write_vec_RealVector
  !! matrix
  PROCEDURE, PASS(obj) :: WriteMatReal32 => txt_Write_Mat_Real32
  PROCEDURE, PASS(obj) :: WriteMatReal64 => txt_Write_Mat_Real64
  PROCEDURE, PASS(obj) :: WriteMatInt8 => txt_Write_Mat_Int8
  PROCEDURE, PASS(obj) :: WriteMatInt16 => txt_Write_Mat_Int16
  PROCEDURE, PASS(obj) :: WriteMatInt32 => txt_Write_Mat_Int32
  PROCEDURE, PASS(obj) :: WriteMatInt64 => txt_Write_Mat_Int64
  !! generic
  GENERIC, PUBLIC :: WRITE => &
    & WriteBlank, &
    & WriteLine, WriteLines, WriteChar, &
    & WriteInt8, WriteInt16, WriteInt32, WriteInt64, &
    & WriteVecInt8, WriteVecInt16, WriteVecInt32, WriteVecInt64, &
    & WriteMatInt8, WriteMatInt16, WriteMatInt32, WriteMatInt64, &
    & WriteIntVector, WriteVecIntVector, &
    & WriteReal32, WriteReal64, &
    & WriteVecReal32, WriteVecReal64, &
    & WriteRealVector, WriteVecRealVector, &
    & WriteMatReal32, WriteMatReal64
END TYPE TxtFile_

TYPE(TxtFile_), PARAMETER :: TypeTxtFile = TxtFile_()

!----------------------------------------------------------------------------
!                                                             TxtFilePointer
!----------------------------------------------------------------------------

TYPE :: TxtFilePointer_
  CLASS(TxtFile_), POINTER :: ptr => NULL()
END TYPE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Initiate the txt file

INTERFACE TxtFileInitiate
  MODULE SUBROUTINE txt_Initiate(obj, filename, unit, status, access, form, &
    & position, action, pad, recl, comment, separator, delimiter)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
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
  END SUBROUTINE txt_Initiate
END INTERFACE TxtFileInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Deallocate the data

INTERFACE TxtFileDeallocate
  MODULE SUBROUTINE txt_Deallocate(obj, Delete)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: Delete
  END SUBROUTINE txt_Deallocate
END INTERFACE TxtFileDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE txt_Final(obj)
    TYPE(TxtFile_), INTENT(INOUT) :: obj
  END SUBROUTINE txt_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                              isValidRecord@EnquireMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION txt_IsValidRecord(obj, aline, ignoreComment, ignoreBlank, &
    & commentSymbol) RESULT(Ans)
    CLASS(TxtFile_), INTENT(IN) :: obj
    TYPE(String), INTENT(IN) :: aline
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(1), OPTIONAL, INTENT(IN) :: commentSymbol
    LOGICAL(LGT) :: ans
  END FUNCTION txt_IsValidRecord
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetEchoStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Set the echo status

INTERFACE
  MODULE SUBROUTINE txt_SetEchoStat(obj, bool)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: bool
  END SUBROUTINE txt_SetEchoStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetEchoUnit@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Set the echo units

INTERFACE
  MODULE SUBROUTINE txt_SetEchoUnit(obj, unitno)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: unitno
  END SUBROUTINE txt_SetEchoUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEchoUnit@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Get the echo units

INTERFACE
  MODULE PURE FUNCTION txt_GetEchoUnit(obj) RESULT(ans)
    CLASS(TxtFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION txt_GetEchoUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                getTotalRecords@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-09
! update: 2021-11-09
! summary: Returns the total number of records in a file
!
!
!# Introduction
!
! This function returns the total number of records in a file
! If `ignoreComment=.TRUE.`, then the comments are ignored
! If `ignoreComment` is true, then `commentSymbol` should be given

INTERFACE
  MODULE FUNCTION txt_GetTotalRecords(obj, ignoreComment, ignoreBlank, &
                                      commentSymbol) RESULT(Ans)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(1), OPTIONAL, INTENT(IN) :: commentSymbol
    INTEGER(I4B) :: ans
  END FUNCTION txt_GetTotalRecords
END INTERFACE

!----------------------------------------------------------------------------
!                                                getTotalRecords@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2025-05-29
! summary: Returns the total number of records in a file
!
!# Introduction
!
! This function returns the total number of cell data in x and y
! If `ignoreComment=.TRUE.`, then the comments are ignored
! If `ignoreComment` is true, then `commentSymbol` should be given
! If `separator` is given, then the data is separated by the separator
! Default value of `separator` is a space

INTERFACE
  MODULE FUNCTION txt_GetTotalData(obj, ignoreComment, ignoreBlank, &
                                      commentSymbol, separator) RESULT(Ans)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
    INTEGER(I4B) :: ans
  END FUNCTION txt_GetTotalData
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEchoStat@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Get the echo status

INTERFACE
  MODULE PURE FUNCTION txt_GetEchoStat(obj) RESULT(ans)
    CLASS(TxtFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION txt_GetEchoStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ReadLine@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Read a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_Read_Line(obj, val, iostat, iomsg, &
  & ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(OUT) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ReadLine@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Read a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_Read_Lines(obj, val, iostat, iomsg, &
  & ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Lines
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ReadLine@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Read a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_Read_Char(obj, val, iostat, iomsg, &
  & ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(OUT) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Char
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2025-05-29
! summary: Read a scalar boolean value

INTERFACE
  MODULE SUBROUTINE txt_Read_boolean(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(INOUT) :: val
    INTEGER(I4B), INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_boolean
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-07-19
! summary: Read a scalar integer

INTERFACE
  MODULE SUBROUTINE txt_Read_Int8(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), INTENT(INOUT) :: val
    INTEGER(I4B), INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-07-19
! summary: Read a scalar integer

INTERFACE
  MODULE SUBROUTINE txt_Read_Int16(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), INTENT(INOUT) :: val
    INTEGER(I4B), INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-07-19
! summary: Read a scalar integer

INTERFACE
  MODULE SUBROUTINE txt_Read_Int32(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), INTENT(INOUT) :: val
    INTEGER(I4B), INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-07-19
! summary: Read a scalar integer

INTERFACE
  MODULE SUBROUTINE txt_Read_Int64(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), INTENT(INOUT) :: val
    INTEGER(I4B), INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-07-19
! summary: Read a scalar real number

INTERFACE
  MODULE SUBROUTINE txt_Read_Real32(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(INOUT) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-07-19
! summary: Read a scalar real number

INTERFACE
  MODULE SUBROUTINE txt_Read_Real64(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(INOUT) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-07-19
! summary: Read an instance of IntVector

INTERFACE
  MODULE SUBROUTINE txt_Read_IntVector(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(IntVector_), INTENT(INOUT) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_IntVector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-07-19
! summary: Read an instance of RealVector

INTERFACE
  MODULE SUBROUTINE txt_Read_RealVector(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(RealVector_), INTENT(INOUT) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_RealVector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2025-05-29
! summary: Read a vector of booleans

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_boolean(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_boolean
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read an integer vector

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_Int8(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a vector of integers

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_Int16(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read an integer vector

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_Int32(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a vector of integers

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_Int64(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a vector of real numbers

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_Real32(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a vector of real numbers

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_Real64(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read an integer vector

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_IntVector(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(IntVector_), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_IntVector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a vector of realVectors

INTERFACE
  MODULE SUBROUTINE txt_Read_vec_RealVector(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(RealVector_), ALLOCATABLE, INTENT(INOUT) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_vec_RealVector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a matrix of integers

INTERFACE
  MODULE SUBROUTINE txt_Read_mat_Int8(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_mat_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a matrix of integers

INTERFACE
  MODULE SUBROUTINE txt_Read_mat_Int16(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_mat_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a matrix of integers

INTERFACE
  MODULE SUBROUTINE txt_Read_mat_Int32(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_mat_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a matrix of integers

INTERFACE
  MODULE SUBROUTINE txt_Read_mat_Int64(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_mat_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a matrix of real numbers

INTERFACE
  MODULE SUBROUTINE txt_Read_mat_Real32(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_mat_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a matrix of real numbers

INTERFACE
  MODULE SUBROUTINE txt_Read_mat_Real64(obj, val, iostat, iomsg, &
                         ignoreComment, ignoreBlank, commentSymbol, separator)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
  END SUBROUTINE txt_Read_mat_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                       ConvertMarkdownToSource@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-07
! update: 2021-11-07
! summary: Reads a markdown file and converts it into the source file

INTERFACE
  MODULE SUBROUTINE txt_convertMarkdownToSource(obj, outfile, lang)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(TxtFile_), INTENT(INOUT) :: outfile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: lang
  END SUBROUTINE txt_convertMarkdownToSource
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_Write_Blank(obj)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    !! YES or NO
  END SUBROUTINE txt_Write_Blank
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE TxtFileWrite
  MODULE SUBROUTINE txt_Write_Line(obj, val, iostat, iomsg, advance)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(*), OPTIONAL, INTENT(IN) :: advance
    !! YES or NO
  END SUBROUTINE txt_Write_Line
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                       WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE TxtFileWrite
  MODULE SUBROUTINE txt_Write_Lines(obj, val, iostat, iomsg, advance)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(*), OPTIONAL, INTENT(IN) :: advance
    !! YES, NO
  END SUBROUTINE txt_Write_Lines
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                       WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE TxtFileWrite
  MODULE SUBROUTINE txt_Write_Char(obj, val, iostat, iomsg, &
    &  advance)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(*), OPTIONAL, INTENT(IN) :: advance
    !! YES, NO
  END SUBROUTINE txt_Write_Char
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                           Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write an integer

INTERFACE TxtFileWrite
  MODULE SUBROUTINE txt_Write_Int8(obj, val, iostat, iomsg, &
                                   advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! COL or ROW, default is COL
  END SUBROUTINE txt_Write_Int8

  MODULE SUBROUTINE txt_Write_Int16(obj, val, iostat, iomsg, &
                                    advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! COL or ROW, default is COL
  END SUBROUTINE txt_Write_Int16

  MODULE SUBROUTINE txt_Write_Int32(obj, val, iostat, iomsg, &
                                    advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! COL or ROW, default is COL
  END SUBROUTINE txt_Write_Int32

  MODULE SUBROUTINE txt_Write_Int64(obj, val, iostat, iomsg, &
                                    advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! COL or ROW, default is COL
  END SUBROUTINE txt_Write_Int64

  MODULE SUBROUTINE txt_Write_IntVector(obj, val, iostat, iomsg, &
                                        advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(IntVector_), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! COL or ROW, default is COL
  END SUBROUTINE txt_Write_IntVector

  MODULE SUBROUTINE txt_Write_Real32(obj, val, iostat, iomsg, &
                                     advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! COL or ROW, default is COL
  END SUBROUTINE txt_Write_Real32

  MODULE SUBROUTINE txt_Write_Real64(obj, val, iostat, iomsg, &
                                     advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! COL or ROW, default is COL
  END SUBROUTINE txt_Write_Real64

  MODULE SUBROUTINE txt_Write_RealVector(obj, val, iostat, iomsg, &
                                         advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(RealVector_), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! COL or ROW, default is COL
  END SUBROUTINE txt_Write_RealVector

  MODULE SUBROUTINE txt_Write_vec_Int8(obj, val, iostat, iomsg, &
                                       advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! ROW: Write vector as a row
    !! COL, TRANSPOSE: Write vector as column
    !! default is COL
  END SUBROUTINE txt_Write_vec_Int8

  MODULE SUBROUTINE txt_Write_vec_Int16(obj, val, iostat, iomsg, &
                                        advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! ROW: Write vector as a row
    !! COL, TRANSPOSE: Write vector as column
    !! default is COL
  END SUBROUTINE txt_Write_vec_Int16

  MODULE SUBROUTINE txt_Write_vec_Int32(obj, val, iostat, iomsg, &
                                        advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! ROW: Write vector as a row
    !! COL, TRANSPOSE: Write vector as column
    !! default is COL
  END SUBROUTINE txt_Write_vec_Int32

  MODULE SUBROUTINE txt_Write_vec_Int64(obj, val, iostat, iomsg, &
                                        advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! ROW: Write vector as a row
    !! COL, TRANSPOSE: Write vector as column
    !! default is COL
  END SUBROUTINE txt_Write_vec_Int64

  MODULE SUBROUTINE txt_Write_vec_Real32(obj, val, iostat, iomsg, &
                                         advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! ROW: Write vector as a row
    !! COL, TRANSPOSE: Write vector as column
    !! default is COL
  END SUBROUTINE txt_Write_vec_Real32

  MODULE SUBROUTINE txt_Write_vec_Real64(obj, val, iostat, iomsg, &
                                         advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! ROW: Write vector as a row
    !! COL, TRANSPOSE: Write vector as column
    !! default is COL
  END SUBROUTINE txt_Write_vec_Real64

  MODULE SUBROUTINE txt_Write_vec_IntVector(obj, val, iostat, iomsg, &
                                            advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(IntVector_), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! ROW: Write vector as a row
    !! COL, TRANSPOSE: Write vector as column
    !! default is COL
  END SUBROUTINE txt_Write_vec_IntVector

  MODULE SUBROUTINE txt_Write_vec_RealVector(obj, val, iostat, iomsg, &
                                             advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(RealVector_), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! ROW: Write vector as a row
    !! COL, TRANSPOSE: Write vector as column
    !! default is COL
  END SUBROUTINE txt_Write_vec_RealVector

  MODULE SUBROUTINE txt_Write_mat_Int8(obj, val, iostat, iomsg, &
                                       advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), INTENT(IN) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! "ROW" write lines rowwise,
    !! that is row-1 is line-1, row-2 is line-2, etc.
    !! "COL" or "TRANSPOSE" write lines columnwise.
  END SUBROUTINE txt_Write_mat_Int8

  MODULE SUBROUTINE txt_Write_mat_Int16(obj, val, iostat, iomsg, &
                                        advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), INTENT(IN) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! "ROW" write lines rowwise,
    !! that is row-1 is line-1, row-2 is line-2, etc.
    !! "COL" or "TRANSPOSE" write lines columnwise.
  END SUBROUTINE txt_Write_mat_Int16

  MODULE SUBROUTINE txt_Write_mat_Int32(obj, val, iostat, iomsg, &
                                        advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), INTENT(IN) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! "ROW" write lines rowwise,
    !! that is row-1 is line-1, row-2 is line-2, etc.
    !! "COL" or "TRANSPOSE" write lines columnwise.
  END SUBROUTINE txt_Write_mat_Int32

  MODULE SUBROUTINE txt_Write_mat_Int64(obj, val, iostat, iomsg, &
                                        advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), INTENT(IN) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! "ROW" write lines rowwise,
    !! that is row-1 is line-1, row-2 is line-2, etc.
    !! "COL" or "TRANSPOSE" write lines columnwise.
  END SUBROUTINE txt_Write_mat_Int64

  MODULE SUBROUTINE txt_Write_mat_Real32(obj, val, iostat, iomsg, &
                                         advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! "ROW" write lines rowwise,
    !! that is row-1 is line-1, row-2 is line-2, etc.
    !! "COL" or "TRANSPOSE" write lines columnwise.
  END SUBROUTINE txt_Write_mat_Real32

  MODULE SUBROUTINE txt_Write_mat_Real64(obj, val, iostat, iomsg, &
                                         advance, orient)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: val(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: orient
    !! "ROW" write lines rowwise,
    !! that is row-1 is line-1, row-2 is line-2, etc.
    !! "COL" or "TRANSPOSE" write lines columnwise.
  END SUBROUTINE txt_Write_mat_Real64

END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TxtFile_Class
