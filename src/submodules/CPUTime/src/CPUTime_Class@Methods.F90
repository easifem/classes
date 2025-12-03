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

SUBMODULE(CPUTime_Class) Methods
USE Display_Method, ONLY: ToString
USE Display_Method, ONLY: Display
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                              SetStartTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetStartTime
CALL CPU_TIME(obj%t1)
!$ obj%wt1 = omp_get_wtime()
END PROCEDURE obj_SetStartTime

!----------------------------------------------------------------------------
!                                                                SetEndTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetEndTime
CALL CPU_TIME(obj%t2)
!$ obj%wt2 = omp_get_wtime()
END PROCEDURE obj_SetEndTime

!----------------------------------------------------------------------------
!                                                                  GetTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTime
ans = obj%t2 - obj%t1
END PROCEDURE obj_GetTime

!----------------------------------------------------------------------------
!                                                                  GetWTime
!----------------------------------------------------------------------------

!$ MODULE PROCEDURE obj_GetWTime
!$ ans = obj%wt2 - obj%wt1
!$ END PROCEDURE obj_GetWTime

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL Display(msg, unitno=unitno)
CALL Display(obj%t1, "t1: ", unitno=unitno)
CALL Display(obj%t2, "t2: ", unitno=unitno)
CALL Display(obj%t2 - obj%t1, "t: ", unitno=unitno)
!$ CALL Display(obj%wt1, "wt1: ", unitno=unitno)
!$ CALL Display(obj%wt2, "wt2: ", unitno=unitno)
!$ CALL Display(obj%wt2 - obj%wt1, "wt: ", unitno=unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                     GetStringForKernelLog
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStringForKernelLog
! internal variables
REAL(DFP) :: t, wt
t = obj%GetTime()
!$ wt = obj%GetWTime()
ans = ToString(currentTimeStep)//","//ToString(currentTime)//"," &
      //'"'//methodName//'"'//","//ToString(t)
!$ ans = ans//","//tostring(wt)
END PROCEDURE obj_GetStringForKernelLog

END SUBMODULE Methods
