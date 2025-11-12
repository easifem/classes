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

MODULE TimeOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE String_Class, ONLY: String
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: TimeOpt_, TypeTimeOpt
CHARACTER(*), PARAMETER :: modName = "TimeOpt_Class"

!----------------------------------------------------------------------------
!                                                                   TimeOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary: This class contains options related to time discretization

TYPE :: TimeOpt_
  INTEGER(I4B) :: static = 0
  !! PDE defines a Static problem

  INTEGER(I4B) :: steady = 0
  !! PDE defines a Static problem

  INTEGER(I4B) :: pseudostatic = 1
  !! PDE defines a Static problem

  INTEGER(I4B) :: transient = 2
  !! PDE defines a Transient problem

  INTEGER(I4B) :: dynamic = 2
  !! PDE defines a Transient problem

  INTEGER(I4B) :: default = 2
  !! Default time dependency

  INTEGER(I4B) :: timeDependency = 2
  !! time dependency of the problem
  !! it can be set to one of the following
  !! static, steady, pseudostatic, transient, dynamic

  CHARACTER(9) :: default_char = "TRANSIENT"
  !! Default time dependency

  INTEGER(I4B) :: totalTimeSteps = 1
  !! Total number of time steps

  INTEGER(I4B) :: currentTimeStep = 1
  !! Current time step

  REAL(DFP) :: currentTime = 0.0
  !! Current time

  REAL(DFP) :: dt = 0.0
  !! Time step

  REAL(DFP) :: startTime = 0.0
  !! Start time

  REAL(DFP) :: endTime = 0.0
  !! End time

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => obj_ToNumber
  !! Convert time option name to number
  PROCEDURE, PUBLIC, PASS(obj) :: ToString => obj_ToString
  !! Convert timeDependency to string
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the object information
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! import timeOpt from toml table
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! import timeOpt from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Import from toml
  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeStepSize => obj_GetTimeStepSize
  !! Get time step size
  PROCEDURE, PUBLIC, PASS(obj) :: GetCurrentTime => obj_GetCurrentTime
  !! Get current time
  PROCEDURE, PUBLIC, PASS(obj) :: GetCurrentTimeStep => &
    obj_GetCurrentTimeStep
  !! Get current time step
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalTimeSteps => obj_GetTotalTimeSteps
  !! Get current time
  PROCEDURE, PUBLIC, PASS(obj) :: UpdateTimeStep => obj_UpdateTimeStep
  !! Update time step
  PROCEDURE, PUBLIC, PASS(obj) :: UpdateCurrentTime => obj_UpdateCurrentTime
  !! Update current time
  PROCEDURE, PUBLIC, PASS(obj) :: GetEndTime => obj_GetEndTime
  !! Get final time of the simulation
END TYPE TimeOpt_

!----------------------------------------------------------------------------
!                                                             TimeOpt
!----------------------------------------------------------------------------

TYPE(TimeOpt_), PARAMETER :: TypeTimeOpt = TimeOpt_()

!----------------------------------------------------------------------------
!                                                            ToString@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Convert timeDependency to string

INTERFACE
  MODULE FUNCTION obj_ToString(obj) RESULT(ans)
    CLASS(TimeOpt_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_ToString
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   ToNumber
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_ToNumber(obj, name) RESULT(ans)
    CLASS(TimeOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_ToNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(TimeOpt_), INTENT(in) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

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

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(TimeOpt_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Import TimeOpt from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml)
    CLASS(TimeOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Set parameters for timeOpt

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, timeDependency, totalTimeSteps, &
                                 currentTimeStep, currentTime, dt, &
                                 startTime, endTime)
    CLASS(TimeOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: timeDependency
    INTEGER(I4B), INTENT(IN), OPTIONAL :: totalTimeSteps
    INTEGER(I4B), INTENT(IN), OPTIONAL :: currentTimeStep
    REAL(DFP), INTENT(IN), OPTIONAL :: currentTime
    REAL(DFP), INTENT(IN), OPTIONAL :: dt
    REAL(DFP), INTENT(IN), OPTIONAL :: startTime
    REAL(DFP), INTENT(IN), OPTIONAL :: endTime
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetTimeStep
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-09
! summary: Get time steo size

INTERFACE
  MODULE FUNCTION obj_GetTimeStepSize(obj) RESULT(ans)
    CLASS(TimeOpt_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetTimeStepSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetCurrentTime
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-09
! summary: Get current time

INTERFACE
  MODULE FUNCTION obj_GetCurrentTime(obj) RESULT(ans)
    CLASS(TimeOpt_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetCurrentTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetCurrentTime
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-09
! summary: Get current time

INTERFACE
  MODULE FUNCTION obj_GetCurrentTimeStep(obj) RESULT(ans)
    CLASS(TimeOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetCurrentTimeStep
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetCurrentTime
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-09
! summary: Get current time

INTERFACE
  MODULE FUNCTION obj_GetTotalTimeSteps(obj) RESULT(ans)
    CLASS(TimeOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalTimeSteps
END INTERFACE

!----------------------------------------------------------------------------
!                                                              UpdateTimeStep
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-09
! summary: Update time step

INTERFACE
  MODULE SUBROUTINE obj_UpdateTimeStep(obj)
    CLASS(TimeOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_UpdateTimeStep
END INTERFACE

!----------------------------------------------------------------------------
!                                                              UpdateTimeStep
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-09
! summary: Update time step

INTERFACE
  MODULE SUBROUTINE obj_UpdateCurrentTime(obj)
    CLASS(TimeOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_UpdateCurrentTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  GetEndTime
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-09
! summary: Get the final time of the simulation

INTERFACE
  MODULE FUNCTION obj_GetEndTime(obj) RESULT(ans)
    CLASS(TimeOpt_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetEndTime
END INTERFACE

END MODULE TimeOpt_Class
