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

SUBMODULE(LineH1DGHierarchicalFE_Class) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeRefelemOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "LineH1HierarchicalFE_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                LineH1HierarchicalFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LineH1DGHierarchicalFEPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_LineH1DGHierarchicalFEPointer1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_LineH1DGHierarchicalFEPointer1

!----------------------------------------------------------------------------
!                                                           FixOrientOfNptrs
!----------------------------------------------------------------------------

#include "../../include/Donot_FixOrientOfNptrs.F90"

!----------------------------------------------------------------------------
!                                                             SetOrientation
!----------------------------------------------------------------------------

#include "../../include/Donot_SetOrientation.F90"

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../../include/errors.F90"

END SUBMODULE Methods
