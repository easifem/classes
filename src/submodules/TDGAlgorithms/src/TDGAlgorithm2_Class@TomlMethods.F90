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

SUBMODULE(TDGAlgorithm2_Class) TomlMethods
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value
USE String_Class, ONLY: String
USE StringUtility, ONLY: UpperCase

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE LCVMethodImportFromToml(obj, table, astr, origin, stat)
  CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: astr
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "LCVMethodImportFromToml()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  REAL(DFP) :: alpha
  REAL(DFP), PARAMETER :: defaultAlpha = 1.0_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=.FALSE., stat=stat)

  alpha = defaultAlpha

  isok = ASSOCIATED(node)
  IF (isok) THEN
    CALL GetValue(table=node, key="alpha", VALUE=alpha, &
                  default_value=defaultAlpha, origin=origin, stat=stat)
  END IF

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE LCVMethodImportFromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: found
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL GetValue(table=table, key="methodName", VALUE=astr, &
              default_value="UV", origin=origin, stat=stat, &
              isfound=found)

obj%name = UpperCase(astr%slice(1, 1))

SELECT CASE (obj%name)

CASE ("U") !UV
  obj%alpha = 1.0_DFP
CASE ("V") !V
  obj%alpha = 0.0_DFP
CASE ("L") !LCV
  CALL LCVMethodImportFromToml(obj=obj, table=table, astr=astr, &
                               origin=origin, stat=stat)
CASE DEFAULT
  obj%alpha = 1.0_DFP
END SELECT

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
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
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
