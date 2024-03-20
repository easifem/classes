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

SUBMODULE(Mesh_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          GetRefElemPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRefElemPointer
ans => obj%refelem
END PROCEDURE obj_GetRefElemPointer

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
ans = obj%refelem%order
END PROCEDURE obj_GetOrder

!----------------------------------------------------------------------------
!                                                       GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity1
INTEGER(I4B), ALLOCATABLE :: cellNptrs(:)
INTEGER(I4B) :: localFaceID, cellNum

SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
  IF (isMaster) THEN
    cellNum = obj%internalFacetData(facetElement)%masterCellNumber
    localFaceID = obj%internalFacetData(facetElement)%masterLocalFacetID
  ELSE
    cellNum = obj%internalFacetData(facetElement)%slaveCellNumber
    localFaceID = obj%internalFacetData(facetElement)%slaveLocalFacetID
  END IF

CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
  cellNum = obj%boundaryFacetData(facetElement)%masterCellNumber
  localFaceID = obj%boundaryFacetData(facetElement)%masterLocalFacetID
END SELECT

IF (cellNum .NE. 0) THEN
  cellNptrs = obj%GetConnectivity(globalElement=cellNum)
  ans = cellNptrs(GetConnectivity(obj%facetElements(localFaceID)))
ELSE
  ALLOCATE (ans(0))
END IF

IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
END PROCEDURE obj_GetFacetConnectivity1

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity2
INTEGER(I4B), ALLOCATABLE :: nptrs(:), indx(:)
nptrs = obj%GetConnectivity(globalElement=globalElement)
indx = GetConnectivity(obj%facetElements(iface))
ans = nptrs(indx)
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(indx)) DEALLOCATE (indx)
END PROCEDURE obj_GetFacetConnectivity2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
CALL AbstractMeshGetParam(obj=obj, &
    & isInitiated=isInitiated,  &
    & isNodeToElementsInitiated=isNodeToElementsInitiated,  &
    & isNodeToNodesInitiated=isNodeToNodesInitiated, &
    & isExtraNodeToNodesInitiated=isExtraNodeToNodesInitiated, &
    & isElementToElementsInitiated=isElementToElementsInitiated, &
    & isBoundaryDataInitiated=isBoundaryDataInitiated,  &
    & isFacetDataInitiated=isFacetDataInitiated, uid=uid, &
    & xidim=xidim, elemType=elemType, nsd=nsd, maxNptrs=maxNptrs, &
    & minNptrs=minNptrs, maxElemNum=maxElemNum, minElemNum=minElemNum,  &
    & tNodes=tNodes, tElements=tElements, &
    & minX=minX, minY=minY, minZ=minZ, maxX=maxX, maxY=maxY, maxZ=maxZ, &
    & x=x, y=y, z=z, tElements_topology_wise=tElements_topology_wise,  &
    & tElemTopologies=tElemTopologies,  &
    & elemTopologies=elemTopologies)

IF (PRESENT(xidim)) xidim = obj%xidim
IF (PRESENT(elemType)) elemType = obj%elemType
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
