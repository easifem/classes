! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(TimeFEDOF_Class) SetMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                      SetFE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFE()"
#endif

INTEGER(I4B) :: cellOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cellOrder = obj%cellOrder
CALL obj%fe%SetOrder(order=cellOrder)
! CALL obj%fe%SetOrientation(cellOrient=cellOrient)
cellOrder = cellOrder * obj%scaleForQuadOrder
CALL obj%fe%SetQuadratureOrder(order=cellOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFE

END SUBMODULE SetMethods
