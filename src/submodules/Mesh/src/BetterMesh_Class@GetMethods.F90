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

SUBMODULE(BetterMesh_Class) GetMethods
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
CHARACTER(*), PARAMETER :: myName = "obj_GetNNE()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
! INTEGER(I4B) :: iel
! iel = obj%GetLocalElemNumber(globalElement)
! ans = SIZE(obj%elementData(iel)%globalNodes)
END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                                  GetMaxNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNNE
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxNNE()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
! ans = 0
! IF (ASSOCIATED(obj%refelem)) THEN
!   ans = .NNE.obj%refelem
! END IF
END PROCEDURE obj_GetMaxNNE

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
CHARACTER(*), PARAMETER :: myName = "obj_GetOrder()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetOrder

!----------------------------------------------------------------------------
!                                                            GetXidimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetXidimension
CHARACTER(*), PARAMETER :: myName = "obj_GetXidimension()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetXidimension

!----------------------------------------------------------------------------
!                                                       GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity1
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetConnectivity1()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
! INTEGER(I4B), ALLOCATABLE :: cellNptrs(:)
! INTEGER(I4B) :: localFaceID, cellNum
!
! SELECT CASE (elementType)
! CASE (INTERNAL_ELEMENT)
!   IF (isMaster) THEN
!     cellNum = obj%internalFacetData(facetElement)%masterCellNumber
!     localFaceID = obj%internalFacetData(facetElement)%masterLocalFacetID
!   ELSE
!     cellNum = obj%internalFacetData(facetElement)%slaveCellNumber
!     localFaceID = obj%internalFacetData(facetElement)%slaveLocalFacetID
!   END IF
!
! CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
!   cellNum = obj%boundaryFacetData(facetElement)%masterCellNumber
!   localFaceID = obj%boundaryFacetData(facetElement)%masterLocalFacetID
! END SELECT
!
! IF (cellNum .NE. 0) THEN
!   cellNptrs = obj%GetConnectivity(globalElement=cellNum)
!   ans = cellNptrs(GetConnectivity(obj%facetElements(localFaceID)))
! ELSE
!   ALLOCATE (ans(0))
! END IF
!
! IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
END PROCEDURE obj_GetFacetConnectivity1

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity2
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetConnectivity2()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
! INTEGER(I4B), ALLOCATABLE :: nptrs(:), indx(:)
! nptrs = obj%GetConnectivity(globalElement=globalElement)
! indx = GetConnectivity(obj%facetElements(iface))
! ans = nptrs(indx)
! IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
! IF (ALLOCATED(indx)) DEALLOCATE (indx)
END PROCEDURE obj_GetFacetConnectivity2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
