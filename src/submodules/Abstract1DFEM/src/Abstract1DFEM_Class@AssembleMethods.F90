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

SUBMODULE(Abstract1DFEM_Class) AssembleMethods

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleTanmat

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AssembleTanmat()"

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
#endif

END PROCEDURE obj_AssembleTanmat

!----------------------------------------------------------------------------
!                                                                AssembleRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHS

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
#endif

END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AssembleMethods
