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
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"
CALL e%raiseError(modName//'::'//myName//" - "// &
& '[IMPLEMENTATION ERROR] :: the routine should be implemented by subclass')
END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!                                                         SetBodySourceFunc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetBodySourceFunc
CHARACTER(*), PARAMETER :: myName = "obj_SetBodySourceFunc()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

obj%bodySourceFunc => func

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_SetBodySourceFunc

!----------------------------------------------------------------------------
!                                                         AssembleBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleBodySource
CHARACTER(*), PARAMETER :: myName = "obj_AssembleBodySource()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
END PROCEDURE obj_AssembleBodySource

!----------------------------------------------------------------------------
!                                                       AssemblePointSource
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssemblePointSource
CHARACTER(*), PARAMETER :: myName = "obj_AssemblePointSource()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
END PROCEDURE obj_AssemblePointSource

!----------------------------------------------------------------------------
!                                                     AssembleSurfaceSource
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleSurfaceSource
CHARACTER(*), PARAMETER :: myName = "obj_AssembleSurfaceSource()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
END PROCEDURE obj_AssembleSurfaceSource

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AssembleRHSMethods
