! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(ElastoDynamics2DFEM_Class) SolveMethods

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Solve
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Solve()"
#endif

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

CALL obj%linsolver%Solve(sol=obj%sol, rhs=obj%rhs)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_Solve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SolveMethods
