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

SUBMODULE(TomlUtility) StringScalarMethods
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_stat

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_String
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_String()"
#endif
CHARACTER(:), ALLOCATABLE :: astr
LOGICAL(LGT) :: isFound0
INTEGER(I4B) :: stat0, origin0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(origin)) origin0 = origin
IF (PRESENT(stat)) stat0 = stat

VALUE = default_value

CALL toml_get(table, key, astr, origin=origin0, stat=stat0)
isFound0 = ALLOCATED(astr) .AND. (stat0 .EQ. toml_stat%success)

IF (isFound0) VALUE = astr
astr = ""

IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(origin)) origin = origin0
IF (PRESENT(stat)) stat = stat0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_String

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE StringScalarMethods

