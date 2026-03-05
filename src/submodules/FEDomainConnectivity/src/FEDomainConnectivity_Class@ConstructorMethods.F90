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

SUBMODULE(FEDomainConnectivity_Class) ConstructorMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "FEDomainConnectivity_Class@ConstructorMethods.F90"
#endif
CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInitiated = .FALSE.
obj%isFacetToCell = .FALSE.
obj%isNodeToNode = .FALSE.
obj%isCellToCell = .FALSE.
IF (ALLOCATED(obj%nodeToNode)) DEALLOCATE (obj%nodeToNode)
IF (ALLOCATED(obj%cellToCell)) DEALLOCATE (obj%cellToCell)
IF (ALLOCATED(obj%facetToCell)) DEALLOCATE (obj%facetToCell)
IF (ALLOCATED(obj%elemToElem)) DEALLOCATE (obj%elemToElem)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate1

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate2()"
#endif
#include "../../include/deallocate_vector.F90"
END PROCEDURE obj_Deallocate2

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate3()"
#endif

#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE obj_Deallocate3

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include  "../../include/errors.F90"

END SUBMODULE ConstructorMethods
