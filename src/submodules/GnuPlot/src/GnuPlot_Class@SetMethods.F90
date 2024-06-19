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

SUBMODULE(GnuPlot_Class) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setPm3dOpts
obj%pm3dOpts_stmt = "set pm3d "//TRIM(opts)
END PROCEDURE obj_setPm3dOpts

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setCBTicks
obj%cbTicks_stmt = "set cbtics "//TRIM(opts)
END PROCEDURE obj_setCBTicks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setCntrLevels
obj%cntrLevels_stmt = "set cntrparam levels "//TRIM(opts)
END PROCEDURE obj_setCntrLevels

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setCBLim
obj%hasCBRange = .TRUE.
obj%CBRange = avec
END PROCEDURE obj_setCBLim

END SUBMODULE SetMethods

