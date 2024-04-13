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

MODULE CPUTime_Class
USE GlobalData, ONLY: DFP, I4B
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
!$ USE OMP_LIB
IMPLICIT NONE
PRIVATE

PUBLIC :: CPUTime_
PUBLIC :: CPUTimePointer_
CHARACTER(*), PARAMETER :: modName = "CPUTime_Class"
CHARACTER(*), PARAMETER :: myprefix = "CPUTime"

!----------------------------------------------------------------------------
!                                                                 CPUTime_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-25
! summary:  Class for cpu time

TYPE :: CPUTime_
  REAL(DFP) :: t1
  REAL(DFP) :: t2
!$ REAL(DFP) :: wt1
!$ REAL(DFP) :: wt2
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: SetStartTime => obj_SetStartTime
  PROCEDURE, PUBLIC, PASS(obj) :: SetEndTime => obj_SetEndTime
  PROCEDURE, PUBLIC, PASS(obj) :: GetTime => obj_GetTime
!$ PROCEDURE, PUBLIC, PASS(obj) :: GetWTime => obj_GetWTime
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: GetStringForKernelLog =>  &
    & obj_GetStringForKernelLog
END TYPE CPUTime_

!----------------------------------------------------------------------------
!                                                             CPUTimePointer_
!----------------------------------------------------------------------------

TYPE :: CPUTimePointer_
  CLASS(CPUTime_), POINTER :: ptr => NULL()
END TYPE CPUTimePointer_

!----------------------------------------------------------------------------
!                                                               TypeCPUTime
!----------------------------------------------------------------------------

! TYPE(CPUTime_) :: TypeCPUTime
! !$OMP THREADPRIVATE( TypeCPUTime )

!----------------------------------------------------------------------------
!                                                         Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-25
! summary:  Initiate the time

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(CPUTime_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetStartTime@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetStartTime(obj)
    CLASS(CPUTime_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetStartTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetEndTime@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetEndTime(obj)
    CLASS(CPUTime_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetEndTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetTime@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetTime(obj) RESULT(ans)
    CLASS(CPUTime_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                             getWTime@Methods
!----------------------------------------------------------------------------

!$ INTERFACE
!$ MODULE FUNCTION obj_GetWTime(obj) RESULT(ans)
!$  CLASS(CPUTime_), INTENT(IN) :: obj
!$  REAL(DFP) :: ans
!$ END FUNCTION obj_GetWTime
!$ END INTERFACE

!----------------------------------------------------------------------------
!                                                               Display
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(CPUTime_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetStringForKernelLog
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetStringForKernelLog(obj, currentTimeStep,  &
    & currentTime, methodName) RESULT(ans)
    CLASS(CPUTime_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: currentTimeStep
    REAL(DFP), INTENT(IN) :: currentTime
    CHARACTER(*), INTENT(IN) :: methodName
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetStringForKernelLog
END INTERFACE

END MODULE CPUTime_Class
