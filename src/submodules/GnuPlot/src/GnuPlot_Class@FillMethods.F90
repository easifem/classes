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

SUBMODULE(PLPlot_Class) FillMethods
USE BaseMethod
USE EasyPlplot
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_FillBetween
  CALL FillBetween( &
    & x=x, &
    & y1=y1, &
    & y0=y0, &
    & fillColor=fillColor, &
    & fillPattern=fillPattern, &
    & lineWidth=lineWidth )
END PROCEDURE plot_FillBetween

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_FillBetweenx
  CALL FillBetweenx( &
    & y=y, &
    & x1=x1, &
    & x0=x0, &
    & fillColor=fillColor, &
    & fillPattern=fillPattern, &
    & lineWidth=lineWidth )
END PROCEDURE plot_FillBetweenx

END SUBMODULE FillMethods