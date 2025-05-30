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

SUBMODULE(TomlUtility) GetMethods
USE ReallocateUtility, ONLY: Reallocate

USE Display_Method, ONLY: ToString, Display

USE tomlf, ONLY: toml_error, &
                 toml_load, &
                 toml_parser_config, &
                 toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_context, &
                 toml_terminal, &
                 toml_load, &
                 toml_array, &
                 toml_stat

USE CSVFile_Class, ONLY: CSVFile_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_string
CHARACTER(:), ALLOCATABLE :: temp_char
LOGICAL(LGT) :: isok

VALUE = default_value

CALL toml_get(table, key, temp_char, origin=origin, stat=stat)
isok = ALLOCATED(temp_char)

IF (isok) THEN
  VALUE = temp_char
  DEALLOCATE (temp_char)
  IF (PRESENT(isFound)) isFound = .TRUE.
  RETURN
END IF

IF (PRESENT(isFound)) isFound = .FALSE.
END PROCEDURE GetValue_string

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_bool
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_bool

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int8
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_int8

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int16
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_int16

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int32
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_int32

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int64
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_int64

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real32
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_real32

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real64
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_real64

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_bool_r1
LOGICAL(LGT) :: temp
LOGICAL(LGT), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_bool_r1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_bool_r1_static
LOGICAL(LGT) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_bool_r1_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int8_r1
INTEGER(INT8) :: temp
INTEGER(INT8), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_int8_r1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int8_r1_static
INTEGER(INT8) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_int8_r1_static

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int16_r1
INTEGER(INT16) :: temp
INTEGER(INT16), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_int16_r1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int16_r1_static
INTEGER(INT16) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_int16_r1_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int32_r1
INTEGER(INT32) :: temp
INTEGER(INT32), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_int32_r1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int32_r1_static
INTEGER(INT32) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_int32_r1_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int64_r1
INTEGER(INT64) :: temp
INTEGER(INT64), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_int64_r1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int64_r1_static
INTEGER(INT64) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_int64_r1_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real32_r1
REAL(REAL32) :: temp
REAL(REAL32), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_real32_r1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real32_r1_static
REAL(REAL32) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_real32_r1_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real64_r1
REAL(REAL64) :: temp
REAL(REAL64), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_real64_r1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real64_r1_static
REAL(REAL64) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_real64_r1_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int8_r2
INTEGER(INT8) :: temp
INTEGER(INT8), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_int8_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int8_r2_static
INTEGER(INT8) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_int8_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int16_r2
INTEGER(INT16) :: temp
INTEGER(INT16), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_int16_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int16_r2_static
INTEGER(INT16) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_int16_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int32_r2
INTEGER(INT32) :: temp
INTEGER(INT32), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_int32_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int32_r2_static
INTEGER(INT32) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_int32_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int64_r2
INTEGER(INT64) :: temp
INTEGER(INT64), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_int64_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_int64_r2_static
INTEGER(INT64) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_int64_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real32_r2
REAL(REAL32) :: temp
REAL(REAL32), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_real32_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real32_r2_static
REAL(REAL32) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_real32_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real64_r2
REAL(REAL64) :: temp
REAL(REAL64), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_real64_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_real64_r2_static
REAL(REAL64) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_real64_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_from_file
CHARACTER(*), PARAMETER :: myName = "GetValue_from_file()"
LOGICAL(LGT) :: isNotOpen, isNotRead
LOGICAL(LGT), PARAMETER :: color = .TRUE.
INTEGER(I4B), PARAMETER :: detail = 1
TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

terminal = toml_terminal(color)
isNotOpen = .NOT. afile%IsOpen()
isNotRead = .NOT. afile%IsRead()

IF (isNotRead .OR. isNotOpen) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: The file is not open or does not have '// &
                    'the access to read!')
END IF

CALL toml_load(table, afile%GetUnitNo(), context=context, error=error, &
             config=toml_parser_config(color=terminal, context_detail=detail))

IF (ALLOCATED(error)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[INTERNAL ERROR] :: Some error occured while parsing toml file'// &
                    ' with following message: '//CHAR_LF//error%message)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE GetValue_from_file

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_from_filename
CHARACTER(*), PARAMETER :: myName = "GetValue_from_filename()"
LOGICAL(LGT), PARAMETER :: color = .TRUE.
INTEGER(I4B), PARAMETER :: detail = 1
TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

terminal = toml_terminal(color)
CALL toml_load(table,  &
  & filename,  &
  & context=context,  &
  & config=toml_parser_config(color=terminal, context_detail=detail), &
  & error=error  &
  & )

IF (ALLOCATED(error)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Some error occured while parsing toml file'//  &
    & ' with following message: '//CHAR_LF//error%message)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE GetValue_from_filename

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_from_file_master
CHARACTER(*), PARAMETER :: myName = "GetValue_from_file_master"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (PRESENT(afile)) THEN
  CALL GetValue_from_file(table=table, afile=afile)
ELSEIF (PRESENT(filename)) THEN
  CALL GetValue_from_filename(table=table, filename=filename)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: either filename or afile should be present!')
  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE GetValue_from_file_master

!----------------------------------------------------------------------------
!                                                            TomlArrayLength
!----------------------------------------------------------------------------

MODULE PROCEDURE ArrayLength
TYPE(toml_array), POINTER :: array

! try to read from the array
array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat,  &
  & requested=.FALSE.)

ans = 0
IF (ASSOCIATED(array)) THEN
  ans = toml_len(array)
END IF

array => NULL()

END PROCEDURE ArrayLength

END SUBMODULE GetMethods
