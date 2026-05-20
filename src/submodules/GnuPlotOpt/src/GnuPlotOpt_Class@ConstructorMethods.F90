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

SUBMODULE(GnuPlotOpt_Class) ConstructorMethods
USE BaseMethod, ONLY: Display
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

! TODO: fix mismatch of subroutine name
MODULE PROCEDURE obj_Deallocate

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Finalize
! WARN: This causes a repeatation of gnuplot execution
! CALL obj%DEALLOCATE()
END PROCEDURE obj_Finalize

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL Display("GnuPlotOpt_Class", msg, unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
