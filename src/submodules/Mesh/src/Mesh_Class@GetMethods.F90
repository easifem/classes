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
USE ReferenceElement_Method
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
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity
INTEGER(I4B), ALLOCATABLE :: nptrs(:), indx(:)
nptrs = obj%GetConnectivity(globalElement=globalElement)
indx = GetConnectivity(obj%facetElements(iface))
ans = nptrs(indx)
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(indx)) DEALLOCATE (indx)
END PROCEDURE obj_GetFacetConnectivity

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

IF (PRESENT(elemType)) elemType = obj%elemType
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                         isFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isFacetElement
ans = ALLOCATED(obj%facetElements)
END PROCEDURE obj_isFacetElement

!----------------------------------------------------------------------------
!                                                      GetTotalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFacetElements
IF (ALLOCATED(obj%facetElements)) THEN
  ans = SIZE(obj%facetElements)
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetTotalFacetElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
