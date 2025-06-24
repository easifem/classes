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
USE StringUtility, ONLY: Uppercase
USE Display_Method, ONLY: Display

IMPLICIT NONE

PRIVATE

PUBLIC :: TimeOpt_, TypeTimeOpt

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

  CHARACTER(9) :: default_char = "TRANSIENT"
  !! Default time dependency

  INTEGER(I4B) :: totalTimeStep = 1
  !! Total number of time steps

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
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the object information

END TYPE TimeOpt_

!----------------------------------------------------------------------------
!                                                             TimeOpt
!----------------------------------------------------------------------------

TYPE(TimeOpt_), PARAMETER :: TypeTimeOpt = TimeOpt_()

!----------------------------------------------------------------------------
!                                                                    Methods
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                   ToNumber
!----------------------------------------------------------------------------

FUNCTION obj_ToNumber(obj, name) RESULT(ans)
  CLASS(TimeOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  ! Internal variables
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
END FUNCTION obj_ToNumber

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(TimeOpt_), INTENT(in) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(msg, unitno)
  CALL Display(obj%static, 'Static: ', unitno=unitno)
  CALL Display(obj%steady, 'Steady: ', unitno=unitno)
  CALL Display(obj%pseudostatic, 'Pseudostatic: ', unitno=unitno)
  CALL Display(obj%transient, 'Transient: ', unitno=unitno)
  CALL Display(obj%dynamic, 'Dynamic: ', unitno=unitno)
  CALL Display(obj%default, 'Default: ', unitno=unitno)
  CALL Display(obj%default_char, 'Default Character: ', unitno=unitno)
  CALL Display(obj%totalTimeStep, 'Total Time Steps: ', unitno=unitno)
  CALL Display(obj%currentTime, 'Current Time: ', unitno=unitno)
  CALL Display(obj%dt, 'Time Step: ', unitno=unitno)
  CALL Display(obj%startTime, 'Start Time: ', unitno=unitno)
  CALL Display(obj%endTime, 'End Time: ', unitno=unitno)

END SUBROUTINE obj_Display

END MODULE TimeOpt_Class
