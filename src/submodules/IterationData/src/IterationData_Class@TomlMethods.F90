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

SUBMODULE(IterationData_Class) TomlMethods
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value
USE ConvergenceOptUtility, ONLY: ConvergenceType_ToInt
USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToInt
USE ConvergenceOptUtility, ONLY: NormType_ToInt
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "IterationData_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok
CHARACTER(:), ALLOCATABLE :: key
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = math%yes

key = "storeHistory"
CALL GetValue(table=table, key=key, VALUE=obj%storeHistory, &
              default_value=math%no, &
              isFound=isok, origin=origin, stat=stat)

key = "name"
CALL GetValue(table=table, key=key, VALUE=obj%name, default_value="NONE", &
              isFound=isok, origin=origin, stat=stat)

#ifdef DEBUG_VER
CALL AssertError1(isok, myName, &
                  "name key not found in toml table.")
#endif

key = "maxIter"
CALL GetValue(table=table, key=key, VALUE=obj%maxIter, &
              default_value=TypeIterationData%maxIter, &
              isFound=isok, origin=origin, stat=stat)

IF (obj%storeHistory) THEN
  ALLOCATE (obj%residualHistory(0:obj%maxIter))
  ALLOCATE (obj%solutionHistory(0:obj%maxIter))
END IF

key = "residualRelativeTolerance"
CALL GetValue(table=table, key=key, VALUE=obj%residualRelTolerance, &
              default_value=TypeIterationData%residualRelTolerance, &
              isFound=isok, origin=origin, stat=stat)

key = "residualAbsoluteTolerance"
CALL GetValue(table=table, key=key, VALUE=obj%residualAbsTolerance, &
              default_value=TypeIterationData%residualAbsTolerance, &
              isFound=isok, origin=origin, stat=stat)

key = "solutionRelativeTolerance"
CALL GetValue(table=table, key=key, VALUE=obj%solutionRelTolerance, &
              default_value=TypeIterationData%solutionRelTolerance, &
              isFound=isok, origin=origin, stat=stat)

key = "solutionAbsoluteTolerance"
CALL GetValue(table=table, key=key, VALUE=obj%solutionAbsTolerance, &
              default_value=TypeIterationData%solutionAbsTolerance, &
              isFound=isok, origin=origin, stat=stat)

key = "convergenceType"
CALL GetValue(table=table, key=key, VALUE=astr, &
              default_value="NONE", isFound=isok, &
              origin=origin, stat=stat)
obj%convergenceType = ConvergenceType_ToInt(astr%Chars())

key = "convergenceIn"
CALL GetValue(table=table, key=key, VALUE=astr, &
              default_value="NONE", isFound=isok, &
              origin=origin, stat=stat)
obj%convergenceIn = ConvergenceIn_ToInt(astr%Chars())

key = "normType"
CALL GetValue(table=table, key=key, VALUE=astr, &
              default_value="NONE", isFound=isok, &
              origin=origin, stat=stat)
obj%normType = NormType_ToInt(astr%Chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

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
                  'following error occured while reading '// &
                  'the toml file :: cannot find ['//tomlName// &
                  "] table in config.")
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
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
