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

SUBMODULE(GnuPlot_Class) PlotMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Plot1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot1
CHARACTER(*), PARAMETER :: myName = "obj_plot1"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement
END PROCEDURE obj_plot1

!----------------------------------------------------------------------------
!                                                                 plot2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot2
CHARACTER(*), PARAMETER :: myName = "obj_plot2"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_plot2
END PROCEDURE obj_plot2

!----------------------------------------------------------------------------
!                                                                 plot3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot3
CHARACTER(*), PARAMETER :: myName = "obj_plot3"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_plot3
END PROCEDURE obj_plot3

END SUBMODULE PlotMethods
