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

SUBMODULE(GnuPlot_Class) MultiPlotMethods

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                multiplot
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_multiplot
CHARACTER(*), PARAMETER :: myName = "obj_multiplot"
IF (obj%hasanimation) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ERROR] :: animation is not supported in multiplot mode')
END IF

IF (rows > 0) THEN
  obj%multiplot_rows = rows
END IF

IF (cols > 0) THEN
  obj%multiplot_cols = cols
END IF

obj%hasmultiplot = .TRUE.
obj%multiplot_total_plots = 0

CALL obj%Initiate()

END PROCEDURE obj_multiplot

END SUBMODULE MultiPlotMethods
