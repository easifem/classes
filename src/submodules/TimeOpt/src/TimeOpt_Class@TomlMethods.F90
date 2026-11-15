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

SUBMODULE(TimeOpt_Class) TomlMethods
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "TimeOpt_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary: Import TimeOpt from toml table
!
!# Introduction
! The toml table should have following contents:
!
!```toml
! [timeOpt]
! timeDependency = "Transient"
! # "Static", "Steady", "Pseudostatic", "Transient", "Dynamic"
! totalTimeStep = 1
! dt = 0.0
! startTime = 0.0
! endTime = 0.0
!```

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isFound, isTotalTimeSteps, isStartTime, isEndTime, isDt
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isInit) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Object is already initialized. Nothing to do here.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading timeDependency...')
#endif

CALL GetValue(table=table, key="timeDependency", &
              VALUE=astr, default_value=TypeTimeOpt%default_char, &
              origin=origin, stat=stat, isFound=isFound)

obj%timeDependency = obj%ToNumber(astr%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading totalTimeSteps...')
#endif

CALL GetValue(table=table, key="totalTimeSteps", &
              VALUE=obj%totalTimeSteps, &
              default_value=TypeTimeOpt%totalTimeSteps, &
              origin=origin, stat=stat, isFound=isTotalTimeSteps)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading dt...')
#endif

CALL GetValue(table=table, key="dt", &
              VALUE=obj%dt, &
              default_value=TypeTimeOpt%dt, &
              origin=origin, stat=stat, isFound=isDt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading startTime...')
#endif

CALL GetValue(table=table, key="startTime", &
              VALUE=obj%startTime, &
              default_value=TypeTimeOpt%startTime, &
              origin=origin, stat=stat, isFound=isStartTime)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading endTime...')
#endif

CALL GetValue(table=table, key="endTime", &
              VALUE=obj%endTime, &
              default_value=TypeTimeOpt%endTime, &
              origin=origin, stat=stat, isFound=isEndTime)

IF (isStartTime .AND. isEndTime .AND. isTotalTimeSteps) THEN
  obj%dt = (obj%endTime - obj%startTime) / obj%totalTimeSteps

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. isDt, myName, &
               "startTime, endTime, totalTimeSteps are given, &
                & no need for dt")
#endif
END IF

IF (isStartTime .AND. isEndTime .AND. isDt) THEN
  obj%totalTimeSteps = CEILING((obj%endTime - obj%startTime) / obj%dt)

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. isTotalTimeSteps, myName, &
               "startTime, endTime, dt are given, &
                & no need for totalTimeSteps")
#endif
END IF

IF (isStartTime .AND. isTotalTimeSteps .AND. isDt) THEN
  obj%endTime = obj%startTime + obj%dt * obj%totalTimeSteps

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. isEndTime, myName, &
               "startTime, dt, totalTimeSteps are given, &
                & no need for endTime")
#endif
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
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
                  'the toml file :: cannot find ['// &
                  tomlName//"] table in config.")
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
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
