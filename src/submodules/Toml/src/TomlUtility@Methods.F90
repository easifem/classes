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

SUBMODULE(TomlUtility) Methods
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

USE String_Class, ONLY: StringReallocate => Reallocate

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

MODULE PROCEDURE GetValue_string_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_string_r1()"
#endif

TYPE(toml_array), POINTER :: array
INTEGER(I4B) :: tsize, stat0, ii
LOGICAL(LGT) :: isFound0, isok
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isFound0 = .FALSE.
IF (PRESENT(isScalar)) isScalar = .FALSE.

!----------------------------------------------------------------------------
! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
!----------------------------------------------------------------------------

array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat0, &
              requested=.FALSE.)

isok = ASSOCIATED(array)

IF (isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Found toml array for key: '//key)
#endif

  tsize = toml_len(array)
  CALL StringReallocate(VALUE, tsize)
  isFound0 = .TRUE.

  DO ii = 1, tsize
    CALL toml_get(array, ii, astr)
    VALUE(ii) = astr
  END DO

  astr = ""

  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isFound)) isFound = isFound0
  NULLIFY (array)
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

!----------------------------------------------------------------------------
! READ a scalar value from toml
! In this case length of the vector is 1, this value is given in toml file
!----------------------------------------------------------------------------

CALL toml_get(table, key, astr, origin=origin, stat=stat0)

IF (stat0 .EQ. toml_stat%success) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Found scalar value for key: '//key)
#endif

  CALL StringReallocate(VALUE, 1)
  VALUE(1) = astr
  astr = ""
  isFound0 = .TRUE.
  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isScalar)) isScalar = .TRUE.
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isFound0 = .FALSE.
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(stat)) stat = stat0

!----------------------------------------------------------------------------
! TODO: READ from a txt file or csv file
!----------------------------------------------------------------------------

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE GetValue_string_r1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_string_r1_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_string_r1_static()"
#endif

TYPE(toml_array), POINTER :: array
INTEGER(I4B) :: stat0, ii
LOGICAL(LGT) :: isFound0, isok
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isFound0 = .FALSE.
tsize = 0
IF (PRESENT(isScalar)) isScalar = .FALSE.

!----------------------------------------------------------------------------
! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
!----------------------------------------------------------------------------

array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat0, &
              requested=.FALSE.)

isok = ASSOCIATED(array)

IF (isok) THEN
  tsize = toml_len(array)
  isFound0 = .TRUE.

  DO ii = 1, tsize
    CALL toml_get(array, ii, astr)
    VALUE(ii) = astr
  END DO

  astr = ""

  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isFound)) isFound = isFound0
  NULLIFY (array)
  RETURN
END IF

!----------------------------------------------------------------------------
! READ a scalar value from toml
! In this case length of the vector is 1, this value is given in toml file
!----------------------------------------------------------------------------

CALL toml_get(table, key, astr, origin=origin, stat=stat0)

IF (stat0 .EQ. toml_stat%success) THEN
  ! CALL StringReallocate(VALUE, 1)
  tsize = 1
  VALUE(1) = astr
  astr = ""
  isFound0 = .TRUE.
  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isScalar)) isScalar = .TRUE.
  RETURN
END IF

isFound0 = .FALSE.
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(stat)) stat = stat0

!----------------------------------------------------------------------------
! TODO: READ from a txt file or csv file
!----------------------------------------------------------------------------

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE GetValue_string_r1_static

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
#endif

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
#endif

END PROCEDURE GetValue_from_file

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_from_filename
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_from_filename()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT), PARAMETER :: color = .TRUE.
INTEGER(I4B), PARAMETER :: detail = 1

TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

terminal = toml_terminal(color)
CALL toml_load(table, filename, context=context, error=error, &
               config=toml_parser_config(color=terminal, &
                                         context_detail=detail))

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(error)
CALL AssertError1(isok, myName, &
       'Some error occured while parsing toml file with following message: ' &
                  //CHAR_LF//error%message)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_from_filename

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_from_file_master
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_from_file_master"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(afile)
IF (isok) CALL GetValue_from_file(table=table, afile=afile)

isok = PRESENT(filename)
IF (isok) CALL GetValue_from_filename(table=table, filename=filename)

#ifdef DEBUG_VER
isok = PRESENT(afile) .OR. PRESENT(filename)
CALL AssertError1(isok, myName, &
                  'either filename or afile should be present!')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_from_file_master

!----------------------------------------------------------------------------
!                                                            TomlArrayLength
!----------------------------------------------------------------------------

MODULE PROCEDURE ArrayLength
TYPE(toml_array), POINTER :: array
LOGICAL(LGT) :: isok

! try to read from the array
array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat, &
              requested=.FALSE.)

ans = 0
isok = ASSOCIATED(array)
IF (isok) ans = toml_len(array)

array => NULL()

END PROCEDURE ArrayLength

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
