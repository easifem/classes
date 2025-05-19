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

#ifdef USE_PLPLOT
SUBMODULE(PLPlot_Class) PlotMethods
USE BaseMethod
USE EasyPlplot
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Show
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Show
CALL Show()
END PROCEDURE plot_Show

!----------------------------------------------------------------------------
!                                                                 Figure
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Figure
CALL Figure()
END PROCEDURE plot_Figure

!----------------------------------------------------------------------------
!                                                                 Subplot
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Subplot
CALL Subplot(ny=ncol, nx=nrow, i=i, aspect=aspect, is3D=is3D)
END PROCEDURE plot_Subplot

END SUBMODULE PlotMethods
#endif
