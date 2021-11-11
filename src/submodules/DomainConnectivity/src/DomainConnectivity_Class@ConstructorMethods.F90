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

SUBMODULE(DomainConnectivity_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           dc_AddSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_AddSurrogate
CALL e%addSurrogate(userObj)
END PROCEDURE dc_AddSurrogate

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_DeallocateData
obj%isInitiated = .FALSE.
obj%isFacetToCell = .FALSE.
obj%isNodeToNode = .FALSE.
obj%isCellToCell = .FALSE.
IF (ALLOCATED(obj%facetToCell)) DEALLOCATE (obj%facetToCell)
IF (ALLOCATED(obj%nodeToNode)) DEALLOCATE (obj%nodeToNode)
IF (ALLOCATED(obj%cellToCell)) DEALLOCATE (obj%cellToCell)
IF (ALLOCATED(obj%elemToElem)) DEALLOCATE (obj%elemToElem)
END PROCEDURE dc_DeallocateData

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_Final
CALL obj%DeallocateData()
END PROCEDURE dc_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
