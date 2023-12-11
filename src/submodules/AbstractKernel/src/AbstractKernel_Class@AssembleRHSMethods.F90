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

SUBMODULE(AbstractKernel_Class) AssembleRHSMethods
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                AssembleRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHS
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS"
CALL e%raiseError(modName//'::'//myName//" - "// &
& '[IMPLEMENTATION ERROR] :: the routine should be implemented by subclass')
! TODO: Implement obj_AssembleRHS
END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!                                                         AssembleBodyForce
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleBodyForce
CHARACTER(*), PARAMETER :: myName = "obj_AssembleBodyForce"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_AssembleBodyForce
END PROCEDURE obj_AssembleBodyForce

!----------------------------------------------------------------------------
!                                                     AssembleSurfaceForce
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleSurfaceForce
CHARACTER(*), PARAMETER :: myName = "obj_AssembleSurfaceForce"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_AssembleSurfaceForce
END PROCEDURE obj_AssembleSurfaceForce

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AssembleRHSMethods
