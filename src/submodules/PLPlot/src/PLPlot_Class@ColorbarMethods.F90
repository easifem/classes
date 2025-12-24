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
SUBMODULE(PLPlot_Class) ColorbarMethods
USE EasyPlplot, ONLY: Colorbar, Colorbar2

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Colorbar
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Colorbar
CALL Colorbar(z=z, N=N, leftLabel=leftLabel, rightLabel=rightLabel)
END PROCEDURE plot_Colorbar

!----------------------------------------------------------------------------
!                                                                 Colorbar
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Colorbar2
CALL Colorbar2(z=z, N=N, leftLabel=leftLabel, rightLabel=rightLabel)
END PROCEDURE plot_Colorbar2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ColorbarMethods
#endif
