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

MODULE FieldOpt_Class
USE GlobalData, ONLY: I4B, Constant, Space, Time, SpaceTime
IMPLICIT NONE

PRIVATE
PUBLIC :: FieldOpt_, TypeFieldOpt

INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_NORMAL = 100
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT = Constant
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_SPACE = Space
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_TIME = Time
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_SPACETIME = SpaceTime
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT_SPACE = Time
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT_TIME = Space

!----------------------------------------------------------------------------
!                                                                  FieldOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary: Options for field variables

TYPE :: FieldOpt_
  INTEGER(I4B) :: normal = FIELD_TYPE_NORMAL
  INTEGER(I4B) :: constant = FIELD_TYPE_CONSTANT
  INTEGER(I4B) :: space = FIELD_TYPE_SPACE
  INTEGER(I4B) :: time = FIELD_TYPE_TIME
  INTEGER(I4B) :: spaceTime = FIELD_TYPE_SPACETIME
  CHARACTER(6) :: normal_char = "NORMAL"
  CHARACTER(8) :: constant_char = "CONSTANT"
  CHARACTER(5) :: space_char = "SPACE"
  CHARACTER(4) :: time_char = "TIME"
  CHARACTER(9) :: spaceTime_char = "SPACETIME"
END TYPE FieldOpt_

TYPE(FieldOpt_), PARAMETER :: TypeFieldOpt = FieldOpt_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FieldOpt_Class
