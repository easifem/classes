! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(AbstractStfemAlgorithm2_Class) TomlMethods
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value
USE BaseType, ONLY: math => TypeMathOpt
USE String_Class, ONLY: String
USE StringUtility, ONLY: UpperCase
USE TomlUtility, ONLY: GetValue

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "AbstractStfemAlgorithm2_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

TYPE(String) :: astr
LOGICAL(LGT) :: isok
INTEGER(I4B) :: stat, origin

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

! we are not reading the name here
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         'Reading name...')
! #endif

! CALL GetValue(table=table, key="name", &
!               VALUE=astr, default_value=default_value, &
!               origin=origin, stat=stat, isFound=isok)

! read the scalingOpt
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading scalingOpt...')
#endif

CALL GetValue(table=table, key="scalingOpt", &
              VALUE=astr, default_value="I", &
              origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              'scalingOpt not found in toml table, it should be "I, K, or M"')
END IF
#endif

obj%scalingOpt = UpperCase(astr%slice(1, 1))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
LOGICAL(LGT) :: isok
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=math%no, &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading the toml file :: &
                  &cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node)

node => NULL()
DEALLOCATE (table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
