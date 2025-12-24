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

#ifdef USE_PLPLOT
SUBMODULE(PLPlot_Class) BarMethods
USE EasyPlplot, ONLY: Bar, Barh, Hist

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Bar
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Bar
CALL Bar(x=x, y=y, c=c, relWidth=relWidth, fillColor=fillColor, &
         fillPattern=fillPattern, lineColor=lineColor, lineWidth=lineWidth)
END PROCEDURE plot_Bar

!----------------------------------------------------------------------------
!                                                                 Bar
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Barh
CALL Barh(x=x, y=y, c=c, relWidth=relWidth, fillColor=fillColor, &
          fillPattern=fillPattern, lineColor=lineColor, lineWidth=lineWidth)
END PROCEDURE plot_Barh

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Hist
CALL Hist(d=d, N=N, db=db, relWidth=relWidth, fillColor=fillColor, &
          fillPattern=fillPattern, lineColor=lineColor, lineWidth=lineWidth)
END PROCEDURE plot_Hist

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE BarMethods
#endif
