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
!                                                                  GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
ans = 0
IF (ASSOCIATED(obj%refelem)) THEN
  ans = .NNE.obj%refelem
END IF
END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                                  GetMaxNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNNE
ans = 0
IF (ASSOCIATED(obj%refelem)) THEN
  ans = .NNE.obj%refelem
END IF
END PROCEDURE obj_GetMaxNNE

!----------------------------------------------------------------------------
!                                                          GetRefElemPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRefElemPointer
ans => obj%refelem
END PROCEDURE obj_GetRefElemPointer

!----------------------------------------------------------------------------
!                                                               GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs
INTEGER(I4B) :: ii
ALLOCATE (ans(obj%tNodes))
DO CONCURRENT(ii=1:SIZE(ans))
  ans(ii) = obj%nodeData(ii)%globalNodeNum
END DO
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                          GetInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
INTEGER(I4B) :: ii, dummy
ALLOCATE (ans(obj%GetTotalInternalNodes()))
dummy = 0
DO ii = 1, obj%tNodes
  IF (obj%nodeData(ii)%nodeType .EQ. INTERNAL_NODE) THEN
    dummy = dummy + 1
    ans(dummy) = obj%nodeData(ii)%globalNodeNum
  END IF
END DO
END PROCEDURE obj_GetInternalNptrs

!----------------------------------------------------------------------------
!                                                          GetBoundaryNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryNptrs
INTEGER(I4B) :: ii, dummy
ALLOCATE (ans(obj%GetTotalBoundaryNodes()))
dummy = 0
DO ii = 1, obj%tNodes
  IF (obj%nodeData(ii)%nodeType .EQ. BOUNDARY_NODE) THEN
    dummy = dummy + 1
    ans(dummy) = obj%nodeData(ii)%globalNodeNum
  END IF
END DO
END PROCEDURE obj_GetBoundaryNptrs

!----------------------------------------------------------------------------
!                                                            isBoundaryNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryNode
INTEGER(I4B) :: localnode
localnode = obj%GetLocalNodeNumber(GlobalNode)
IF (obj%nodeData(localnode)%nodeType .NE. INTERNAL_NODE) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_isBoundaryNode

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryElement
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement)
IF (obj%elementData(iel)%elementType .LE. BOUNDARY_ELEMENT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_isBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainBoundaryElement
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement)
IF (obj%elementData(iel)%elementType .EQ. DOMAIN_BOUNDARY_ELEMENT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_isDomainBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainFacetElement

IF (obj%boundaryFacetData(facetElement)%elementType  &
  & .EQ. DOMAIN_BOUNDARY_ELEMENT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF

END PROCEDURE obj_isDomainFacetElement

!----------------------------------------------------------------------------
!                                                 GetTotalBoundaryElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryElements
ans = COUNT(obj%elementData(:)%elementType == BOUNDARY_ELEMENT)
END PROCEDURE obj_GetTotalBoundaryElements

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
ans = obj%elementData(obj%GetLocalElemNumber(globalElement))%globalNodes
END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeConnectivity
INTEGER(I4B) :: iel, telem

telem = obj%GetTotalElements()

DO iel = 1, telem
  VALUE(:, iel) = obj%elementData(iel)%globalNodes
END DO

END PROCEDURE obj_GetNodeConnectivity

!----------------------------------------------------------------------------
!                                                         GetGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber2
IF (localNode .EQ. 0) THEN
  ans = 0
ELSE IF (localNode .LE. obj%tNodes) THEN
  ans = obj%nodeData(localNode)%globalNodeNum
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                        GetGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemNumber2
IF (localElement .EQ. 0) THEN
  ans = 0
ELSE IF (localElement .LE. obj%tElements) THEN
  ans = obj%elementData(localElement)%globalElemNum
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetGlobalElemNumber2

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
INTEGER(I4B) :: ii
IF (.NOT. obj%isNodePresent(GlobalNode)) THEN
  ALLOCATE (ans(0))
ELSE
  ii = obj%GetLocalNodeNumber(GlobalNode)
  ans = obj%nodeData(ii)%globalElements
END IF
END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes1
INTEGER(I4B), ALLOCATABLE :: nptrs(:), extranptrs(:)
INTEGER(I4B) :: i, j
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes1()"

i = obj%GetLocalNodeNumber(GlobalNode=GlobalNode)

! check
IF (obj%isExtraNodeToNodesInitiated) THEN

  IF (i .EQ. 0) THEN
    ALLOCATE (ans(0))
  ELSE
    IF (IncludeSelf) THEN

      nptrs = obj%nodeData(i)%globalNodes
      i = SIZE(nptrs)

      IF (ALLOCATED(obj%nodeData(i)%extraGlobalNodes)) THEN
        extranptrs = obj%nodeData(i)%extraGlobalNodes
        j = SIZE(extranptrs)
        CALL Reallocate(ans, i + j + 1)
        ans(1) = GlobalNode
        ans(2:1 + i) = nptrs
        ans(2 + i:1 + i + j) = extranptrs
      ELSE
        CALL Reallocate(ans, i + 1)
        ans(1) = GlobalNode
        ans(2:1 + i) = nptrs
      END IF

    ELSE
      CALL APPEND( &
        & ans, &
        & obj%nodeData(i)%globalNodes, &
        & obj%nodeData(i)%extraGlobalNodes)
    END IF
  END IF

ELSE

  IF (i .EQ. 0) THEN
    ALLOCATE (ans(0))
  ELSE
    IF (IncludeSelf) THEN
      nptrs = obj%nodeData(i)%globalNodes
      i = SIZE(nptrs)
      ALLOCATE (ans(i + 1))
      ans(1) = GlobalNode
      ans(2:) = nptrs
    ELSE
      ans = obj%nodeData(i)%globalNodes
    END IF
  END IF

END IF

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(extranptrs)) DEALLOCATE (extranptrs)

END PROCEDURE obj_GetNodeToNodes1

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements
LOGICAL(LGT) :: onlyElem
INTEGER(I4B), ALLOCATABLE :: Indx(:)
INTEGER(I4B) :: iel

onlyElem = .FALSE.
IF (PRESENT(onlyElements)) onlyElem = onlyElements
iel = obj%GetLocalElemNumber(globalElement)

IF (ALLOCATED(obj%elementData(iel)%globalElements)) THEN
  IF (onlyElem) THEN
    Indx = obj%elementData(iel)%globalElements
    ALLOCATE (ans(SIZE(Indx) / 3, 1))
    ans(:, 1) = Indx(1 :: 3)
    DEALLOCATE (Indx)
  ELSE
    Indx = obj%elementData(iel)%globalElements
    ans = TRANSPOSE(RESHAPE(Indx, [3, SIZE(Indx) / 3]))
  END IF
ELSE
  ALLOCATE (ans(0, 0))
END IF

END PROCEDURE obj_GetElementToElements

!----------------------------------------------------------------------------
!                                                     GetBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryElementData
INTEGER(I4B) :: iel

IF (obj%isBoundaryElement(globalElement)) THEN
  iel = obj%GetLocalElemNumber(globalElement)
  ans = obj%elementData(iel)%boundaryData
ELSE
  ALLOCATE (ans(0))
END IF

END PROCEDURE obj_GetBoundaryElementData

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
ans = obj%refelem%order
END PROCEDURE obj_GetOrder

!----------------------------------------------------------------------------
!                                                                     GetNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNSD
ans = obj%NSD
END PROCEDURE obj_GetNSD

!----------------------------------------------------------------------------
!                                                            GetXidimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetXidimension
ans = obj%xidim
END PROCEDURE obj_GetXidimension

!----------------------------------------------------------------------------
!                                              GetTotalInternalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInternalFacetElements
ans = 0
IF (ALLOCATED(obj%internalFacetData)) THEN
  ans = SIZE(obj%internalFacetData)
END IF
END PROCEDURE obj_GetTotalInternalFacetElements

!----------------------------------------------------------------------------
!                                              GetTotalBoundaryFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryFacetElements
ans = 0
IF (ALLOCATED(obj%boundaryFacetData)) THEN
  ans = SIZE(obj%boundaryFacetData)
END IF
END PROCEDURE obj_GetTotalBoundaryFacetElements

!----------------------------------------------------------------------------
!                                                       GetMasterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMasterCellNumber
SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
  ans = obj%internalFacetData(facetElement)%masterCellNumber
CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
  ans = obj%boundaryFacetData(facetElement)%masterCellNumber
END SELECT
END PROCEDURE obj_GetMasterCellNumber

!----------------------------------------------------------------------------
!                                                         GetSlaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSlaveCellNumber
SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
  ans = obj%internalFacetData(facetElement)%slaveCellNumber
CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
  ans = 0
END SELECT
END PROCEDURE obj_GetSlaveCellNumber

!----------------------------------------------------------------------------
!                                                              GetCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellNumber
SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
  ans(1) = obj%internalFacetData(facetElement)%masterCellNumber
  ans(2) = obj%internalFacetData(facetElement)%slaveCellNumber
CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
  ans(1) = obj%boundaryFacetData(facetElement)%masterCellNumber
  ans(2) = 0
END SELECT
END PROCEDURE obj_GetCellNumber

!----------------------------------------------------------------------------
!                                                           GetLocalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetID
SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
  IF (isMaster) THEN
    ans = obj%internalFacetData(facetElement)%masterLocalFacetID
  ELSE
    ans = obj%internalFacetData(facetElement)%slaveLocalFacetID
  END IF
CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
  ans = obj%boundaryFacetData(facetElement)%masterLocalFacetID
END SELECT
END PROCEDURE obj_GetLocalFacetID

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
!                                                        GetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetElementType
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement=globalElement)
ans = obj%facetElementType(:, iel)
END PROCEDURE obj_GetFacetElementType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuery
CALL AbstractMeshGetQuery(obj=obj, &
    & isInitiated=isInitiated,  &
    & isNodeToElementsInitiated=isNodeToElementsInitiated,  &
    & isNodeToNodesInitiated=isNodeToNodesInitiated, &
    & isExtraNodeToNodesInitiated=isExtraNodeToNodesInitiated, &
    & isElementToElementsInitiated=isElementToElementsInitiated, &
    & isBoundaryDataInitiated=isBoundaryDataInitiated,  &
    & isFacetDataInitiated=isFacetDataInitiated, uid=uid, &
    & xidim=xidim, elemType=elemType, nsd=nsd, maxNptrs=maxNptrs, &
    & minNptrs=minNptrs, maxElemNum=maxElemNum, minElemNum=minElemNum,  &
    & tNodes=tNodes, tIntNodes=tIntNodes, tElements=tElements, &
    & minX=minX, minY=minY, minZ=minZ, maxX=maxX, maxY=maxY, maxZ=maxZ, &
    & x=x, y=y, z=z, tElements_topology_wise=tElements_topology_wise,  &
    & tElemTopologies=tElemTopologies,  &
    & elemTopologies=elemTopologies)

IF (PRESENT(xidim)) xidim = obj%xidim
IF (PRESENT(elemType)) elemType = obj%elemType
END PROCEDURE obj_GetQuery

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
