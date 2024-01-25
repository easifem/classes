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
!                                                                      SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_size
ans = obj%tElements
END PROCEDURE obj_size

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum
INTEGER(I4B) :: ii
! main
CALL Reallocate(ans, obj%GetTotalElements())
DO ii = 1, SIZE(ans)
  ans(ii) = obj%GetGlobalElemNumber(localElement=ii)
END DO
END PROCEDURE obj_GetElemNum

!----------------------------------------------------------------------------
!                                                          GetRefElemPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRefElemPointer
ans => obj%refelem
END PROCEDURE obj_GetRefElemPointer

!----------------------------------------------------------------------------
!                                                         GetBoundingEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingEntity
IF (ALLOCATED(obj%boundingEntity)) THEN
  ans = obj%boundingEntity
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE obj_GetBoundingEntity

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
!                                                           isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodePresent
ans = .TRUE.
IF (globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs) THEN
  ans = .FALSE.
ELSE IF (obj%local_nptrs(globalNode) .EQ. 0) THEN
  ans = .FALSE.
END IF
END PROCEDURE obj_isNodePresent

!----------------------------------------------------------------------------
!                                                           isAnyNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isAnyNodePresent
LOGICAL(LGT), ALLOCATABLE :: cond(:)
INTEGER(I4B) :: ii, n
!
n = SIZE(globalNode)
ALLOCATE (cond(n))
!
DO ii = 1, n
  cond(ii) = obj%isNodePresent(globalNode=globalNode(ii))
END DO
!
ans = ANY(cond)
!
IF (ALLOCATED(cond)) DEALLOCATE (cond)
!
END PROCEDURE obj_isAnyNodePresent

!----------------------------------------------------------------------------
!                                                           isAllNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isAllNodePresent
LOGICAL(LGT), ALLOCATABLE :: cond(:)
INTEGER(I4B) :: ii, n
!
n = SIZE(globalNode)
ALLOCATE (cond(n))
!
DO ii = 1, n
  cond(ii) = obj%isNodePresent(globalNode=globalNode(ii))
END DO
!
ans = ALL(cond)
IF (ALLOCATED(cond)) DEALLOCATE (cond)
!
END PROCEDURE obj_isAllNodePresent

!----------------------------------------------------------------------------
!                                                           isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElementPresent
IF (GlobalElement .GT. obj%maxElemNum &
  & .OR. GlobalElement .LT. obj%minElemNum &
  & .OR. obj%local_elemNumber(GlobalElement) .EQ. 0) THEN
  ans = .FALSE.
ELSE
  ans = .TRUE.
END IF
END PROCEDURE obj_isElementPresent

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
!
IF (obj%boundaryFacetData(facetElement)%elementType  &
  & .EQ. DOMAIN_BOUNDARY_ELEMENT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
!
END PROCEDURE obj_isDomainFacetElement

!----------------------------------------------------------------------------
!                                                     GetTotalInternalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInternalNodes
ans = obj%tIntNodes
END PROCEDURE obj_GetTotalInternalNodes

!----------------------------------------------------------------------------
!                                                       GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes
ans = obj%tNodes
END PROCEDURE obj_GetTotalNodes

!----------------------------------------------------------------------------
!                                                   GetTotalBoundaryNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryNodes
ans = obj%tNodes - obj%tIntNodes
END PROCEDURE obj_GetTotalBoundaryNodes

!----------------------------------------------------------------------------
!                                                 GetTotalBoundaryElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryElements
ans = COUNT(obj%elementData(:)%elementType == BOUNDARY_ELEMENT)
END PROCEDURE obj_GetTotalBoundaryElements

!----------------------------------------------------------------------------
!                                                            GetBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingBox1
REAL(DFP) :: lim(6)
lim(1) = obj%minX
lim(2) = obj%maxX
lim(3) = obj%minY
lim(4) = obj%maxY
lim(5) = obj%minZ
lim(6) = obj%maxZ
CALL Initiate(obj=ans, nsd=3_I4B, lim=lim)
END PROCEDURE obj_GetBoundingBox1

!----------------------------------------------------------------------------
!                                                             GetBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingBox2
INTEGER(I4B) :: nsd
REAL(DFP) :: lim(6)
!> main
lim = 0.0_DFP
nsd = SIZE(nodes, 1)
IF (PRESENT(local_nptrs)) THEN
  lim(1:nsd * 2:2) = MINVAL(nodes(1:nsd,  &
    & local_nptrs(obj%GetNptrs())),  &
    & dim=2)
  lim(2:nsd * 2:2) = MAXVAL(nodes(1:nsd,  &
    & local_nptrs(obj%GetNptrs())),  &
    & dim=2)
ELSE
  lim(1:nsd * 2:2) = MINVAL(nodes(1:nsd, &
    & obj%GetNptrs()), &
    & dim=2)
  lim(2:nsd * 2:2) = MAXVAL(nodes(1:nsd, &
    & obj%GetNptrs()), &
    & dim=2)
END IF
CALL Initiate(obj=ans, nsd=nsd, lim=lim)
END PROCEDURE obj_GetBoundingBox2

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
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(GlobalNode)
  ans(ii) = obj_GetLocalNodeNumber2(obj, GlobalNode(ii))
END DO
END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                        GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
IF (GlobalNode .LT. obj%MinNptrs &
  & .OR. GlobalNode .GT. obj%maxNptrs) THEN
  ans = 0
ELSE
  ans = obj%Local_Nptrs(GlobalNode)
END IF
END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                        GetGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(LocalNode)
  ans(ii) = obj_GetGlobalNodeNumber2(obj, LocalNode(ii))
END DO
END PROCEDURE obj_GetGlobalNodeNumber1

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

MODULE PROCEDURE obj_GetGlobalElemNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj_GetGlobalElemNumber2(obj, localElement(ii))
END DO
END PROCEDURE obj_GetGlobalElemNumber1

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
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(GlobalElement)
  ans(ii) = obj_GetLocalElemNumber2(obj, GlobalElement(ii))
END DO
END PROCEDURE obj_GetLocalElemNumber1

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber2
IF (GlobalElement .LT. obj%MinElemNum &
  & .OR. GlobalElement .GT. obj%maxElemNum) THEN
  ans = 0
ELSE
  ans = obj%local_elemNumber(GlobalElement)
END IF
END PROCEDURE obj_GetLocalElemNumber2

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
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii
!> main
DO ii = 1, SIZE(GlobalNode)
  ivec = obj%GetNodeToElements(GlobalNode=GlobalNode(ii))
  IF (ALLOCATED(ivec)) THEN
    IF (SIZE(ivec) .NE. 0) THEN
      CALL append(intvec, ivec)
    END IF
  END IF
END DO
CALL RemoveDuplicates(intvec)
ans = intvec
CALL DEALLOCATE (intvec)
IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes1
!
! Define internal variable
!
INTEGER(I4B), ALLOCATABLE :: nptrs(:), extranptrs(:)
INTEGER(I4B) :: i, j
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes1()"

i = obj%GetLocalNodeNumber(GlobalNode=GlobalNode)

! check
IF (obj%isExtraNodeToNodesInitiated) THEN
  !
  IF (i .EQ. 0) THEN
    ALLOCATE (ans(0))
  ELSE
    IF (IncludeSelf) THEN
      !
      nptrs = obj%nodeData(i)%globalNodes
      i = SIZE(nptrs)
      !
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
  !
ELSE
  !
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
  !
END IF
!
!
!
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(extranptrs)) DEALLOCATE (extranptrs)
!
END PROCEDURE obj_GetNodeToNodes1

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii
!> main
DO ii = 1, SIZE(GlobalNode)
  ivec = obj%GetNodeToNodes(GlobalNode=GlobalNode(ii), &
    & IncludeSelf=IncludeSelf)
  IF (ALLOCATED(ivec)) THEN
    IF (SIZE(ivec) .NE. 0) CALL append(intvec, ivec)
  END IF
END DO
CALL RemoveDuplicates(intvec)
ans = intvec
CALL DEALLOCATE (intvec)
IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
END PROCEDURE obj_GetNodeToNodes2

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements
LOGICAL(LGT) :: onlyElem
INTEGER(I4B), ALLOCATABLE :: Indx(:)
INTEGER(I4B) :: iel
!
onlyElem = .FALSE.
IF (PRESENT(onlyElements)) onlyElem = onlyElements
iel = obj%GetLocalElemNumber(globalElement)
!
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
!
IF (obj%isBoundaryElement(globalElement)) THEN
  iel = obj%GetLocalElemNumber(globalElement)
  ans = obj%elementData(iel)%boundaryData
ELSE
  ALLOCATE (ans(0))
END IF
!
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
!                                                                GetMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaterial
LOGICAL(LGT) :: isok
ans = 0

isok = ALLOCATED(obj%material)
IF (.NOT. isok) RETURN
isok = medium .LE. SIZE(obj%material)
IF (.NOT. isok) RETURN

ans = obj%material(medium)
END PROCEDURE obj_GetMaterial

!----------------------------------------------------------------------------
!                                                         GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMaterial
IF (ALLOCATED(obj%material)) THEN
  ans = SIZE(obj%material)
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetTotalMaterial

!----------------------------------------------------------------------------
!                                                      GetTotalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFacetElements
ans = obj%GetTotalInternalFacetElements() &
  & + obj%GetTotalBoundaryFacetElements()
END PROCEDURE obj_GetTotalFacetElements

!----------------------------------------------------------------------------
!                                              GetTotalInternalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInternalFacetElements
IF (ALLOCATED(obj%internalFacetData)) THEN
  ans = SIZE(obj%internalFacetData)
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetTotalInternalFacetElements

!----------------------------------------------------------------------------
!                                              GetTotalBoundaryFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryFacetElements
IF (ALLOCATED(obj%boundaryFacetData)) THEN
  ans = SIZE(obj%boundaryFacetData)
ELSE
  ans = 0
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
IF (PRESENT(isInitiated)) isInitiated = obj%isInitiated
IF (PRESENT(isNodeToElementsInitiated)) isNodeToElementsInitiated =  &
& obj%isNodeToElementsInitiated
IF (PRESENT(isNodeToNodesInitiated)) isNodeToNodesInitiated =  &
  & obj%isNodeToNodesInitiated
IF (PRESENT(isExtraNodeToNodesInitiated)) isExtraNodeToNodesInitiated =  &
  & obj%isExtraNodeToNodesInitiated
IF (PRESENT(isElementToElementsInitiated)) isElementToElementsInitiated =  &
  & obj%isElementToElementsInitiated
IF (PRESENT(isBoundaryDataInitiated)) isBoundaryDataInitiated = &
  & obj%isBoundaryDataInitiated
IF (PRESENT(isFacetDataInitiated)) isFacetDataInitiated =  &
  & obj%isFacetDataInitiated
IF (PRESENT(uid)) uid = obj%uid
IF (PRESENT(xidim)) xidim = obj%xidim
IF (PRESENT(elemType)) elemType = obj%elemType
IF (PRESENT(nsd)) nsd = obj%nsd
IF (PRESENT(maxNptrs)) maxNptrs = obj%maxNptrs
IF (PRESENT(minNptrs)) minNptrs = obj%minNptrs
IF (PRESENT(maxElemNum)) maxElemNum = obj%maxElemNum
IF (PRESENT(minElemNum)) minElemNum = obj%minElemNum
IF (PRESENT(tNodes)) tNodes = obj%tNodes
IF (PRESENT(tIntNodes)) tIntNodes = obj%tIntNodes
IF (PRESENT(tElements)) tElements = obj%tElements
IF (PRESENT(minX)) minX = obj%minX
IF (PRESENT(minY)) minY = obj%minY
IF (PRESENT(minZ)) minZ = obj%minZ
IF (PRESENT(maxX)) maxX = obj%maxX
IF (PRESENT(maxY)) maxY = obj%maxY
IF (PRESENT(maxZ)) maxZ = obj%maxZ
END PROCEDURE obj_GetQuery

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinElemNumber
ans = obj%minElemNum
END PROCEDURE obj_GetMinElemNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxElemNumber
ans = obj%maxElemNum
END PROCEDURE obj_GetMaxElemNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinNodeNumber
ans = obj%minNptrs
END PROCEDURE obj_GetMinNodeNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNodeNumber
ans = obj%maxNptrs
END PROCEDURE obj_GetMaxNodeNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
