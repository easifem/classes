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

SUBMODULE(TomlUtility) StringMethods
USE Display_Method, ONLY: ToString, Display
USE String_Class, ONLY: StringReallocate => Reallocate
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
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_String
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_String()"
#endif
CHARACTER(:), ALLOCATABLE :: temp_char
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

VALUE = default_value

CALL toml_get(table, key, temp_char, origin=origin, stat=stat)
isok = ALLOCATED(temp_char)

IF (isok) VALUE = temp_char
IF (PRESENT(isFound)) isFound = isok
temp_char = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_String

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_String_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_String_r1()"
#endif

TYPE(toml_array), POINTER :: array
INTEGER(I4B) :: tsize, stat0, ii
LOGICAL(LGT) :: isFound0, isok
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isFound0 = math%no
IF (PRESENT(isScalar)) isScalar = math%no

!----------------------------------------------------------------------------
! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
!----------------------------------------------------------------------------

array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat0, &
              requested=math%no)

isok = ASSOCIATED(array)

IF (isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Found toml array for key: '//key)
#endif

  tsize = toml_len(array)
  CALL StringReallocate(VALUE, tsize)
  isFound0 = math%yes

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
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Found scalar value for key: '//key)
#endif

  CALL StringReallocate(VALUE, 1)
  VALUE(1) = astr
  astr = ""
  isFound0 = math%yes
  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isScalar)) isScalar = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isFound0 = math%no
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(stat)) stat = stat0

!----------------------------------------------------------------------------
! TODO: READ from a txt file or csv file
!----------------------------------------------------------------------------

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE GetValue_String_r1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_String_r1_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_String_r1_static()"
#endif

TYPE(toml_array), POINTER :: array
INTEGER(I4B) :: stat0, ii
LOGICAL(LGT) :: isFound0, isok
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isFound0 = math%no
tsize = 0
IF (PRESENT(isScalar)) isScalar = math%no

!----------------------------------------------------------------------------
! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
!----------------------------------------------------------------------------

array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat0, &
              requested=math%no)

isok = ASSOCIATED(array)

IF (isok) THEN
  tsize = toml_len(array)
  isFound0 = math%yes

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
  ! CALL StringReallocate(VALUE, 1)
  tsize = 1
  VALUE(1) = astr
  astr = ""
  isFound0 = math%yes
  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isScalar)) isScalar = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isFound0 = math%no
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(stat)) stat = stat0

!----------------------------------------------------------------------------
! TODO: READ from a txt file or csv file
!----------------------------------------------------------------------------

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_String_r1_static

!----------------------------------------------------------------------------
!                                                          GetValue_String_r2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_String_r2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_String_r2()"
#endif

TYPE(toml_array), POINTER :: array, row_array
INTEGER(I4B) :: ncol, nrow, stat0, ii, temp_ncol, jj
TYPE(CSVFile_) :: acsvfile
TYPE(TxtFile_) :: atxtfile
LOGICAL(LGT) :: isFound0, isok
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isFound0 = math%no

!----------------------------------------------------------------------------
! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
!----------------------------------------------------------------------------

array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat0, &
              requested=math%no)

isok = ASSOCIATED(array)

IF (isok) THEN
  nrow = toml_len(array)

  DO ii = 1, nrow
    row_array => NULL()
    CALL toml_get(array, ii, row_array)

#ifdef DEBUG_VER
    isok = ASSOCIATED(row_array)
    CALL AssertError1( &
      isok, myName, &
      'While reading 2D array, it is seems the '//ToString(ii)// &
      'th row is empty (cannot read it).')
#endif

    temp_ncol = toml_len(row_array)

    IF (ii .EQ. 1) THEN
      ncol = temp_ncol
      CALL StringReallocate(VALUE, nrow, ncol)
      isFound0 = math%yes

    ELSE

#ifdef DEBUG_VER
      CALL AssertError2( &
      temp_ncol, ncol, myName, &
      'Staggered matrix is not allowed, a=length of current row, &
       &b=length of first row')
#endif

    END IF

    DO jj = 1, ncol
      CALL toml_get(row_array, jj, astr)
      VALUE(ii, jj) = astr
    END DO
  END DO

  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  NULLIFY (array, row_array)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

!----------------------------------------------------------------------------
! READ a scalar value from toml
! In this case shape of matrix is (1,1), this value is given in toml file
!----------------------------------------------------------------------------

CALL toml_get(table, key, astr, origin=origin, stat=stat0)

IF (stat0 .EQ. toml_stat%success) THEN
  CALL StringReallocate(VALUE, 1, 1)

  VALUE(1, 1) = astr
  isFound0 = math%yes

  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

!----------------------------------------------------------------------------
! TODO: READ from a txt file or csv file
!----------------------------------------------------------------------------

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE GetValue_String_r2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE StringMethods

