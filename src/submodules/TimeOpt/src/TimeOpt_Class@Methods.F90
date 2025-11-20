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

SUBMODULE(TimeOpt_Class) Methods
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE StringUtility, ONLY: Uppercase
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ToString
CHARACTER(*), PARAMETER :: myName = "obj_ToString()"

SELECT CASE (obj%timeDependency)
CASE (TypeTimeOpt%static)
  ans = "STATIC"
CASE (TypeTimeOpt%pseudostatic)
  ans = "PSEUDOSTATIC"
CASE (TypeTimeOpt%transient)
  ans = "TRANSIENT"
CASE default
  ans = ""
  CALL e%RaiseError(modName//'::'//myName//' - '// &
          'No case found for timeDependency = '//Tostring(obj%timeDependency))
END SELECT

END PROCEDURE obj_ToString

!----------------------------------------------------------------------------
!                                                                   ToNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ToNumber
TYPE(String) :: astr

! main code
astr = Uppercase(name)
ans = TypeTimeOpt%default
SELECT CASE (astr%chars())
CASE ("STATIC", "STEADY")
  ans = TypeTimeOpt%steady
CASE ("TRANSIENT", "DYNAMIC")
  ans = TypeTimeOpt%dynamic
CASE ("PSEUDOSTATIC")
  ans = TypeTimeOpt%pseudostatic
END SELECT
astr = ""
END PROCEDURE obj_ToNumber

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL Display(msg, unitno)
CALL Display(obj%static, 'static: ', unitno=unitno)
CALL Display(obj%steady, 'steady: ', unitno=unitno)
CALL Display(obj%pseudostatic, 'pseudostatic: ', unitno=unitno)
CALL Display(obj%transient, 'transient: ', unitno=unitno)
CALL Display(obj%dynamic, 'dynamic: ', unitno=unitno)
CALL Display(obj%default, 'default: ', unitno=unitno)
CALL Display(obj%default_char, 'default_char: ', unitno=unitno)
CALL Display(obj%timeDependency, 'timeDependency: ', unitno=unitno)
CALL Display(obj%totalTimeSteps, 'totalTimeSteps: ', unitno=unitno)
CALL Display(obj%currentTimeStep, 'currentTimeStep: ', unitno=unitno)
CALL Display(obj%currentTime, 'currentTime: ', unitno=unitno)
CALL Display(obj%dt, 'dt: ', unitno=unitno)
CALL Display(obj%startTime, 'startTime: ', unitno=unitno)
CALL Display(obj%endTime, 'endTime: ', unitno=unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
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
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isFound
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%isInit
CALL AssertError1(isok, myName, &
                  'TimeOpt is already initialized, deallocate it first.')
#endif

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
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading dt...')
#endif

CALL GetValue(table=table, key="dt", &
              VALUE=obj%dt, &
              default_value=TypeTimeOpt%dt, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading startTime...')
#endif

CALL GetValue(table=table, key="startTime", &
              VALUE=obj%startTime, &
              default_value=TypeTimeOpt%startTime, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading endTime...')
#endif

CALL GetValue(table=table, key="endTime", &
              VALUE=obj%endTime, &
              default_value=TypeTimeOpt%endTime, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
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
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
! Internal variables
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(timeDependency)) obj%timeDependency = timeDependency
IF (PRESENT(totalTimeSteps)) obj%totalTimeSteps = totalTimeSteps
IF (PRESENT(currentTimeStep)) obj%currentTimeStep = currentTimeStep
IF (PRESENT(currentTime)) obj%currentTime = currentTime
IF (PRESENT(dt)) obj%dt = dt
IF (PRESENT(startTime)) obj%startTime = startTime
IF (PRESENT(endTime)) obj%endTime = endTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                             GetTimeStepSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeStepSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeStepSize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%dt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTimeStepSize

!----------------------------------------------------------------------------
!                                                             GetCurrentTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCurrentTime
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCurrentTime()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%currentTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCurrentTime

!----------------------------------------------------------------------------
!                                                          GetCurrentTimeStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCurrentTimeStep
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCurrentTimeStep()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%currentTimeStep

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCurrentTimeStep

!----------------------------------------------------------------------------
!                                                          GetTotalTimeSteps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalTimeSteps
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalTimeSteps()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%totalTimeSteps

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalTimeSteps

!----------------------------------------------------------------------------
!                                                              UpdateTimeStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateTimeStep
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_UpdateTimeStep()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%currentTimeStep = obj%currentTimeStep + 1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_UpdateTimeStep

!----------------------------------------------------------------------------
!                                                          UpdateCurrentTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateCurrentTime
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_UpdateCurrentTime()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%currentTime = obj%currentTime + obj%dt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_UpdateCurrentTime

!----------------------------------------------------------------------------
!                                                                 GetEndTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEndTime
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetEndTime()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%endTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetEndTime

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = TypeTimeOpt%isInit
obj%static = TypeTimeOpt%static
obj%steady = TypeTimeOpt%steady
obj%pseudostatic = TypeTimeOpt%pseudostatic
obj%transient = TypeTimeOpt%transient
obj%dynamic = TypeTimeOpt%dynamic
obj%default = TypeTimeOpt%default
obj%timeDependency = TypeTimeOpt%timeDependency
obj%default_char = TypeTimeOpt%default_char
obj%totalTimeSteps = TypeTimeOpt%totalTimeSteps
obj%currentTimeStep = TypeTimeOpt%currentTimeStep
obj%currentTime = TypeTimeOpt%currentTime
obj%dt = TypeTimeOpt%dt
obj%startTime = TypeTimeOpt%startTime
obj%endTime = TypeTimeOpt%endTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsInitiated()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%isInit

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
