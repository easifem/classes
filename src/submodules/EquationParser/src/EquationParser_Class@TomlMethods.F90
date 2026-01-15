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

SUBMODULE(EquationParser_Class) TomlMethods
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE String_Class, ONLY: String
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE TomlUtility, ONLY: GetValue
USE TomlUtility, ONLY: GetValue_
USE tomlf, ONLY: toml_get => get_value

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1"
#endif

CHARACTER(TypeUserFunctionOpt%maxVarLen), ALLOCATABLE :: varNames(:)
CHARACTER(TypeUserFunctionOpt%maxlen) :: funcStr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Calling ImportVarNamesFromToml() ...')
#endif

CALL ImportVarNamesFromToml(varNames=varNames, table=table)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Calling ImportFuncStrFromToml() ...')
#endif

CALL ImportFuncStrFromToml(funcStr=funcStr, table=table)

CALL obj%Initiate(funcStr=funcStr, var=varNames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                           ImportFromToml
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
        'Following error occured while reading toml file :: cannot find ['// &
                  tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node)

NULLIFY (node)
DEALLOCATE (table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportVarNamesFromToml(varNames, table)
  CHARACTER(TypeUserFunctionOpt%maxVarLen), ALLOCATABLE, INTENT(INOUT) :: &
    varNames(:)
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportVarNamesFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, tsize, ii
  LOGICAL(LGT) :: isok, isScalar
  CHARACTER(*), PARAMETER :: key = "vars"
  TYPE(String), ALLOCATABLE :: strs(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading vars from toml ...')
#endif

  CALL GetValue(table=table, key=key, VALUE=strs, stat=stat, origin=origin, &
                isFound=isok, isScalar=isScalar)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    "Cannot find key "//key//" in the toml table")
#endif

  IF (isScalar) THEN
    tsize = 1
  ELSE
    tsize = SIZE(strs)
  END IF

  ALLOCATE (varNames(tsize))

  DO ii = 1, tsize
    varNames(ii) = strs(ii)%Chars()
    strs(ii) = ""
  END DO

  DEALLOCATE (strs)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportVarNamesFromToml

!----------------------------------------------------------------------------
!                                                       ImportFuncStrFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportFuncStrFromToml(funcStr, table)
  CHARACTER(*), INTENT(INOUT) :: funcStr
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportFuncStrFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  TYPE(String) :: strs
  CHARACTER(*), PARAMETER :: key = "expression"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading expression from toml ...')
#endif

  CALL GetValue(table=table, key=key, VALUE=strs, stat=stat, origin=origin, &
                isFound=isok, default_value="NA")

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    "Cannot find key "//key//" in the toml table")
#endif

  funcStr = strs%Chars()
  strs = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportFuncStrFromToml

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
