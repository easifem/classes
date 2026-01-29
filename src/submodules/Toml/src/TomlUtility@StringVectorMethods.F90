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

SUBMODULE(TomlUtility) StringVectorMethods
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

MODULE PROCEDURE GetValue_String_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_String_r1()"
#endif

INTEGER(I4B) :: stat0, origin0
LOGICAL(LGT) :: isFound0, isScalar0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isFound0 = math%no
isScalar0 = math%no
IF (PRESENT(origin)) origin0 = origin
IF (PRESENT(stat)) stat0 = stat

! the following call get value from the toml array
IF (.NOT. isFound0) &
  CALL GetVectorValue1(table, key, VALUE, origin0, stat0, isFound0, isScalar0)

! if the above routine failed then we try to read the
! single value by using the following call
IF (.NOT. isFound0) &
  CALL GetVectorValue2(table, key, VALUE, origin0, stat0, isFound0, isScalar0)

IF (PRESENT(origin)) origin = origin0
IF (PRESENT(stat)) stat = stat0
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(isScalar)) isScalar0 = isScalar0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_String_r1

!----------------------------------------------------------------------------
!                                                                  GetValue1
!----------------------------------------------------------------------------

! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
SUBROUTINE GetVectorValue1(table, key, VALUE, origin, stat, isFound, &
                           isScalar)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  TYPE(String), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValue1()"
#endif

  TYPE(toml_array), POINTER :: array
  INTEGER(I4B) :: tsize, ii
  LOGICAL(LGT) :: isok
  CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isScalar = math%no

  array => NULL()
  CALL toml_get(table, key, array, origin=origin, stat=stat, &
                requested=math%no)

  isok = ASSOCIATED(array)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! The following code will be executed when array is associated.
  tsize = toml_len(array)
  CALL StringReallocate(VALUE, tsize)
  isFound = math%yes

  DO ii = 1, tsize
    CALL toml_get(array, ii, astr)
    VALUE(ii) = astr
  END DO
  astr = ""

  NULLIFY (array)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValue1

!----------------------------------------------------------------------------
!                                                                  GetValue2
!----------------------------------------------------------------------------

SUBROUTINE GetVectorValue2(table, key, VALUE, origin, stat, isFound, &
                           isScalar)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  TYPE(String), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar

  ! define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValue2()"
#endif
  CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, key, astr, origin=origin, stat=stat)

  isFound = stat .EQ. toml_stat%success
  isScalar = math%no

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  CALL StringReallocate(VALUE, 1)
  VALUE(1) = astr
  astr = ""

  isFound = math%yes
  isScalar = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValue2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE StringVectorMethods

