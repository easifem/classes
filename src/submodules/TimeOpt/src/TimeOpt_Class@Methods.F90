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
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE StringUtility, ONLY: Uppercase
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "TimeOpt_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ToString
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ToString()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = ""

SELECT CASE (obj%timeDependency)
CASE (TypeTimeOpt%static)
  ans = "STATIC"
CASE (TypeTimeOpt%pseudostatic)
  ans = "PSEUDOSTATIC"
CASE (TypeTimeOpt%transient)
  ans = "TRANSIENT"
CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for timeDependency")
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ToString

!----------------------------------------------------------------------------
!                                                                   ToNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ToNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ToNumber()"
#endif
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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
CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for given name")
#endif
END SELECT
astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ToNumber

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                   SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

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
!                                                               GetStartTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStartTime
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetStartTime()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%StartTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[Start] ')
#endif
END PROCEDURE obj_GetStartTime

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
