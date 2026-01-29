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

SUBMODULE(TomlUtility) StringStaticMatrixMethods
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
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_String_r2_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_String_r2_static()"
#endif

INTEGER(I4B) :: origin0, stat0
LOGICAL(LGT) :: isFound0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(origin)) origin0 = origin
IF (PRESENT(stat)) stat0 = stat
isFound0 = math%no

! the following call get value from the toml array
IF (.NOT. isFound0) &
  CALL GetMatrixValue1(table, key, VALUE, origin0, stat0, isFound0, &
                       nrow, ncol)

! if the above routine failed then we try to read the
! single value by using the following call
IF (.NOT. isFound0) &
  CALL GetMatrixValue2(table, key, VALUE, origin0, stat0, isFound0, nrow, &
                       ncol)

IF (PRESENT(origin)) origin = origin0
IF (PRESENT(stat)) stat = stat0
IF (PRESENT(isFound)) isFound = isFound0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_String_r2_static

!----------------------------------------------------------------------------
!                                                            GetMatrixValue1
!----------------------------------------------------------------------------

! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
SUBROUTINE GetMatrixValue1(table, key, VALUE, origin, stat, isFound, nrow, &
                           ncol)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  TYPE(String), INTENT(INOUT) :: VALUE(:, :)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValue1()"
  INTEGER(I4B) :: maxnrow, maxncol
#endif

  TYPE(toml_array), POINTER :: array, row_array
  INTEGER(I4B) :: ii, jj
  LOGICAL(LGT) :: isok
  CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  nrow = 0; ncol = 0

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
  nrow = toml_len(array)

#ifdef DEBUG_VER
  maxnrow = SIZE(VALUE, 1)
  CALL AssertError3(nrow, maxnrow, myName, &
                    "a=nrow, b=nrow in value")

  maxncol = SIZE(VALUE, 2)
#endif

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

    ncol = toml_len(row_array)

#ifdef DEBUG_VER
    CALL AssertError3(ncol, maxncol, myName, &
                      "a=ncol, b=ncol in value")
#endif

    DO jj = 1, ncol
      CALL toml_get(row_array, jj, astr)
      VALUE(ii, jj) = astr
    END DO

  END DO

  isFound = math%yes

  NULLIFY (array, row_array)

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValue1

!----------------------------------------------------------------------------
!                                                            GetMatrixValue2
!----------------------------------------------------------------------------

! READ a scalar value from toml
! In this case shape of matrix is (1,1), this value is given in toml file
SUBROUTINE GetMatrixValue2(table, key, VALUE, origin, stat, isFound, nrow, &
                           ncol)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  TYPE(String), INTENT(INOUT) :: VALUE(:, :)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValue2()"
#endif
  CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, key, astr, origin=origin, stat=stat)

  isFound = stat .EQ. toml_stat%success
  nrow = 0; ncol = 0

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! The following code is executed when isok is true
  VALUE(1, 1) = astr
  nrow = 1; ncol = 1
  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValue2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE StringStaticMatrixMethods

