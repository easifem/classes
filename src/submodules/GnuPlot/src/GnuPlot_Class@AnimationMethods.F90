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

SUBMODULE(GnuPlot_Class) AnimationMethods

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              animationStart
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_animationStart
CHARACTER(*), PARAMETER :: myName = "obj_animationStart"
IF (obj%setMultiplot) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ERROR] :: animation is not supported in multiplot mode')
END IF

IF (PRESENT(pauseSeconds)) THEN
  obj%pauseSeconds = pauseSeconds
ELSE
  obj%pauseSeconds = defaultOpt%pauseSeconds
END IF

obj%frameIndex = 0

CALL obj%Initiate()

obj%showAnimation = .TRUE.

END PROCEDURE obj_animationStart

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_animationShow
CHARACTER(*), PARAMETER :: myName = "obj_animationShow"

obj%frameIndex = 0
obj%showAnimation = .FALSE.

CALL obj%DEALLOCATE()

END PROCEDURE obj_animationShow

END SUBMODULE AnimationMethods
