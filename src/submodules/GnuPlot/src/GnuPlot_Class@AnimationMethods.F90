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

SUBMODULE(GnuPlot_Class) AnimationMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              animationStart
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_animationStart
CHARACTER(*), PARAMETER :: myName = "obj_animationStart"
IF (obj%hasmultiplot) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ERROR] :: animation is not supported in multiplot mode')
END IF

IF (PRESENT(pauseSeconds)) THEN
  obj%pause_seconds = pauseSeconds
ELSE
  obj%pause_seconds = defaultPause
END IF

obj%frame_number = 0

CALL create_outputfile(obj)
obj%hasfileopen = .TRUE.
obj%hasanimation = .TRUE.

END PROCEDURE obj_animationStart

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_animationShow
CHARACTER(*), PARAMETER :: myName = "obj_animationShow"
obj%frame_number = 0
obj%hasanimation = .FALSE.

CALL finalize_plot(obj)

END PROCEDURE obj_animationShow

END SUBMODULE AnimationMethods
