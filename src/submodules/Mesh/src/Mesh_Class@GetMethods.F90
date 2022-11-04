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
!                                                                      SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_size
ans = obj%tElements
END PROCEDURE mesh_size

!----------------------------------------------------------------------------
!                                                                getElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getElemNum
INTEGER(I4B) :: ii
  !! main
CALL Reallocate(ans, obj%getTotalElements())
DO ii = 1, SIZE(ans)
  ans(ii) = obj%getGlobalElemNumber(localElement=ii)
END DO
END PROCEDURE mesh_getElemNum

!----------------------------------------------------------------------------
!                                                          getRefElemPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getRefElemPointer
ans => obj%refelem
END PROCEDURE mesh_getRefElemPointer

!----------------------------------------------------------------------------
!                                                         getBoundingEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundingEntity
IF (ALLOCATED(obj%boundingEntity)) THEN
  ans = obj%boundingEntity
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE mesh_getBoundingEntity

!----------------------------------------------------------------------------
!                                                               getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNptrs
INTEGER(I4B) :: ii
ALLOCATE (ans(obj%tNodes))
DO CONCURRENT(ii=1:SIZE(ans))
  ans(ii) = obj%nodeData(ii)%globalNodeNum
END DO
END PROCEDURE mesh_getNptrs

!----------------------------------------------------------------------------
!                                                          getInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getInternalNptrs
INTEGER(I4B) :: ii, dummy
ALLOCATE (ans(obj%getTotalInternalNodes()))
dummy = 0
DO ii = 1, obj%tNodes
  IF (obj%nodeData(ii)%nodeType .EQ. INTERNAL_NODE) THEN
    dummy = dummy + 1
    ans(dummy) = obj%nodeData(ii)%globalNodeNum
  END IF
END DO
END PROCEDURE mesh_getInternalNptrs

!----------------------------------------------------------------------------
!                                                          getBoundaryNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundaryNptrs
INTEGER(I4B) :: ii, dummy
ALLOCATE (ans(obj%getTotalBoundaryNodes()))
dummy = 0
DO ii = 1, obj%tNodes
  IF (obj%nodeData(ii)%nodeType .EQ. BOUNDARY_NODE) THEN
    dummy = dummy + 1
    ans(dummy) = obj%nodeData(ii)%globalNodeNum
  END IF
END DO
END PROCEDURE mesh_getBoundaryNptrs

!----------------------------------------------------------------------------
!                                                            isBoundaryNode
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isBoundaryNode
INTEGER(I4B) :: localnode
localnode = obj%getLocalNodeNumber(GlobalNode)
IF (obj%nodeData(localnode)%nodeType .NE. INTERNAL_NODE) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE mesh_isBoundaryNode

!----------------------------------------------------------------------------
!                                                           isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isNodePresent
ans = .TRUE.
IF (globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs) THEN
  ans = .FALSE.
ELSE IF (obj%local_nptrs(globalNode) .EQ. 0) THEN
  ans = .FALSE.
END IF
END PROCEDURE mesh_isNodePresent

!----------------------------------------------------------------------------
!                                                           isAnyNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isAnyNodePresent
LOGICAL(LGT), ALLOCATABLE :: cond(:)
INTEGER(I4B) :: ii, n
  !!
n = SIZE(globalNode)
ALLOCATE (cond(n))
  !!
DO ii = 1, n
  cond(ii) = obj%isNodePresent(globalNode=globalNode(ii))
END DO
  !!
ans = ANY(cond)
  !!
IF (ALLOCATED(cond)) DEALLOCATE (cond)
  !!
END PROCEDURE mesh_isAnyNodePresent

!----------------------------------------------------------------------------
!                                                           isAllNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isAllNodePresent
LOGICAL(LGT), ALLOCATABLE :: cond(:)
INTEGER(I4B) :: ii, n
  !!
n = SIZE(globalNode)
ALLOCATE (cond(n))
  !!
DO ii = 1, n
  cond(ii) = obj%isNodePresent(globalNode=globalNode(ii))
END DO
  !!
ans = ALL(cond)
IF (ALLOCATED(cond)) DEALLOCATE (cond)
  !!
END PROCEDURE mesh_isAllNodePresent

!----------------------------------------------------------------------------
!                                                           isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isElementPresent
IF (GlobalElement .GT. obj%maxElemNum &
  & .OR. GlobalElement .LT. obj%minElemNum &
  & .OR. obj%local_elemNumber(GlobalElement) .EQ. 0) THEN
  ans = .FALSE.
ELSE
  ans = .TRUE.
END IF
END PROCEDURE mesh_isElementPresent

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isBoundaryElement
INTEGER(I4B) :: iel
iel = obj%getLocalElemNumber(globalElement)
IF (obj%elementData(iel)%elementType .LE. BOUNDARY_ELEMENT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE mesh_isBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isDomainBoundaryElement
INTEGER(I4B) :: iel
iel = obj%getLocalElemNumber(globalElement)
IF (obj%elementData(iel)%elementType .EQ. DOMAIN_BOUNDARY_ELEMENT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE mesh_isDomainBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isDomainFacetElement
  !!
IF (obj%boundaryFacetData(facetElement)%elementType  &
  & .EQ. DOMAIN_BOUNDARY_ELEMENT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
  !!
END PROCEDURE mesh_isDomainFacetElement

!----------------------------------------------------------------------------
!                                                     getTotalInternalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalInternalNodes
ans = obj%tIntNodes
END PROCEDURE mesh_getTotalInternalNodes

!----------------------------------------------------------------------------
!                                                       getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalNodes
ans = obj%tNodes
END PROCEDURE mesh_getTotalNodes

!----------------------------------------------------------------------------
!                                                   getTotalBoundaryNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalBoundaryNodes
ans = obj%tNodes - obj%tIntNodes
END PROCEDURE mesh_getTotalBoundaryNodes

!----------------------------------------------------------------------------
!                                                 getTotalBoundaryElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalBoundaryElements
ans = COUNT(obj%elementData(:)%elementType == BOUNDARY_ELEMENT)
END PROCEDURE mesh_getTotalBoundaryElements

!----------------------------------------------------------------------------
!                                                            getBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundingBox1
REAL(DFP) :: lim(6)
lim(1) = obj%minX
lim(2) = obj%maxX
lim(3) = obj%minY
lim(4) = obj%maxY
lim(5) = obj%minZ
lim(6) = obj%maxZ
CALL Initiate(obj=ans, nsd=3_I4B, lim=lim)
END PROCEDURE mesh_getBoundingBox1

!----------------------------------------------------------------------------
!                                                             getBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundingBox2
INTEGER(I4B) :: nsd
REAL(DFP) :: lim(6)
!> main
lim = 0.0_DFP
nsd = SIZE(nodes, 1)
IF (PRESENT(local_nptrs)) THEN
  lim(1:nsd * 2:2) = MINVAL(nodes(1:nsd,  &
    & local_nptrs(obj%getNptrs())),  &
    & dim=2)
  lim(2:nsd * 2:2) = MAXVAL(nodes(1:nsd,  &
    & local_nptrs(obj%getNptrs())),  &
    & dim=2)
ELSE
  lim(1:nsd * 2:2) = MINVAL(nodes(1:nsd, &
    & obj%getNptrs()), &
    & dim=2)
  lim(2:nsd * 2:2) = MAXVAL(nodes(1:nsd, &
    & obj%getNptrs()), &
    & dim=2)
END IF
CALL Initiate(obj=ans, nsd=nsd, lim=lim)
END PROCEDURE mesh_getBoundingBox2

!----------------------------------------------------------------------------
!                                                            getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getConnectivity
  !!
ans = obj%elementData(obj%getLocalElemNumber(globalElement))%globalNodes
  !!
END PROCEDURE mesh_getConnectivity

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalNodeNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(GlobalNode)
  ans(ii) = mesh_getLocalNodeNumber2(obj, GlobalNode(ii))
END DO
END PROCEDURE mesh_getLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                        getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalNodeNumber2
IF (GlobalNode .LT. obj%MinNptrs &
  & .OR. GlobalNode .GT. obj%maxNptrs) THEN
  ans = 0
ELSE
  ans = obj%Local_Nptrs(GlobalNode)
END IF
END PROCEDURE mesh_getLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                        getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalNodeNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(LocalNode)
  ans(ii) = mesh_getGlobalNodeNumber2(obj, LocalNode(ii))
END DO
END PROCEDURE mesh_getGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalNodeNumber2
IF (localNode .EQ. 0) THEN
  ans = 0
ELSE IF (localNode .LE. obj%tNodes) THEN
  ans = obj%nodeData(localNode)%globalNodeNum
ELSE
  ans = 0
END IF
END PROCEDURE mesh_getGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                        getGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalElemNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = mesh_getGlobalElemNumber2(obj, localElement(ii))
END DO
END PROCEDURE mesh_getGlobalElemNumber1

!----------------------------------------------------------------------------
!                                                        getGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalElemNumber2
IF (localElement .EQ. 0) THEN
  ans = 0
ELSE IF (localElement .LE. obj%tElements) THEN
  ans = obj%elementData(localElement)%globalElemNum
ELSE
  ans = 0
END IF
END PROCEDURE mesh_getGlobalElemNumber2

!----------------------------------------------------------------------------
!                                                         getLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalElemNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(GlobalElement)
  ans(ii) = mesh_getLocalElemNumber2(obj, GlobalElement(ii))
END DO
END PROCEDURE mesh_getLocalElemNumber1

!----------------------------------------------------------------------------
!                                                         getLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalElemNumber2
IF (GlobalElement .LT. obj%MinElemNum &
  & .OR. GlobalElement .GT. obj%maxElemNum) THEN
  ans = 0
ELSE
  ans = obj%local_elemNumber(GlobalElement)
END IF
END PROCEDURE mesh_getLocalElemNumber2

!----------------------------------------------------------------------------
!                                                          getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeToElements1
INTEGER(I4B) :: ii
IF (.NOT. obj%isNodePresent(GlobalNode)) THEN
  ALLOCATE (ans(0))
ELSE
  ii = obj%getLocalNodeNumber(GlobalNode)
  ans = obj%nodeData(ii)%globalElements
END IF
END PROCEDURE mesh_getNodeToElements1

!----------------------------------------------------------------------------
!                                                          getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeToElements2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii
!> main
DO ii = 1, SIZE(GlobalNode)
  ivec = obj%getNodeToElements(GlobalNode=GlobalNode(ii))
  IF (ALLOCATED(ivec)) THEN
    IF (SIZE(ivec) .NE. 0) CALL append(intvec, ivec)
  END IF
END DO
CALL RemoveDuplicates(intvec)
ans = intvec
CALL DEALLOCATE (intvec)
IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
END PROCEDURE mesh_getNodeToElements2

!----------------------------------------------------------------------------
!                                                            getNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeToNodes1
  !!
  !! Define internal variable
  !!
INTEGER(I4B), ALLOCATABLE :: nptrs(:), extranptrs(:)
INTEGER(I4B) :: i, j
CHARACTER(LEN=*), PARAMETER :: myName = "mesh_getNodeToNodes1"
  !!
  !!
  !!
i = obj%getLocalNodeNumber(GlobalNode=GlobalNode)
  !!
  !! check
  !!
IF (obj%isExtraNodeToNodesInitiated) THEN
    !!
  call display(myname//" debug, inside is extranodeto")
  IF (i .EQ. 0) THEN
    ALLOCATE (ans(0))
  ELSE
    IF (IncludeSelf) THEN
      nptrs = obj%nodeData(i)%globalNodes
      extranptrs = obj%nodeData(i)%extraGlobalNodes
      i = SIZE(nptrs)
      j = SIZE(extranptrs)
      CALL Reallocate(ans, i + j + 1)
      ans(1) = GlobalNode
      ans(2:1 + i) = nptrs
      ans(2 + i:1 + i + j) = extranptrs
    ELSE
      CALL APPEND( &
        & ans, &
        & obj%nodeData(i)%globalNodes, &
        & obj%nodeData(i)%extraGlobalNodes)
    END IF
  END IF
    !!
ELSE
  !!
  IF (i .EQ. 0) THEN
    ALLOCATE (ans(0))
  ELSE
    IF (IncludeSelf) THEN
      nptrs = obj%nodeData(i)%globalNodes
      call display(nptrs, myname//" debug, nptrs=")
      i = SIZE(nptrs)
      ALLOCATE (ans(i + 1))
      ans(1) = GlobalNode
      ans(2:) = nptrs
    ELSE
      ans = obj%nodeData(i)%globalNodes
    END IF
  END IF
  !!
END IF
  !!
  !!
  !!
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(extranptrs)) DEALLOCATE (extranptrs)
  !!
END PROCEDURE mesh_getNodeToNodes1

!----------------------------------------------------------------------------
!                                                            getNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeToNodes2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii
!> main
DO ii = 1, SIZE(GlobalNode)
  ivec = obj%getNodeToNodes(GlobalNode=GlobalNode(ii), &
    & IncludeSelf=IncludeSelf)
  IF (ALLOCATED(ivec)) THEN
    IF (SIZE(ivec) .NE. 0) CALL append(intvec, ivec)
  END IF
END DO
CALL RemoveDuplicates(intvec)
ans = intvec
CALL DEALLOCATE (intvec)
IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
END PROCEDURE mesh_getNodeToNodes2

!----------------------------------------------------------------------------
!                                                       getElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getElementToElements
LOGICAL(LGT) :: onlyElem
INTEGER(I4B), ALLOCATABLE :: Indx(:)
INTEGER(I4B) :: tSize, iel
  !!
onlyElem = .FALSE.
IF (PRESENT(onlyElements)) onlyElem = onlyElements
iel = obj%getLocalElemNumber(globalElement)
  !!
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

END PROCEDURE mesh_getElementToElements

!----------------------------------------------------------------------------
!                                                     getBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundaryElementData
INTEGER(I4B) :: iel
  !!
IF (obj%isBoundaryElement(globalElement)) THEN
  iel = obj%getLocalElemNumber(globalElement)
  ans = obj%elementData(iel)%boundaryData
ELSE
  ALLOCATE (ans(0))
END IF
  !!
END PROCEDURE mesh_getBoundaryElementData

!----------------------------------------------------------------------------
!                                                                  getOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getOrder
ans = obj%refelem%order
END PROCEDURE mesh_getOrder

!----------------------------------------------------------------------------
!                                                                     getNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNSD
ans = obj%NSD
END PROCEDURE mesh_getNSD

!----------------------------------------------------------------------------
!                                                            getXidimension
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getXidimension
ans = obj%xidim
END PROCEDURE mesh_getXidimension

!----------------------------------------------------------------------------
!                                                                getMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getMaterial
IF (medium .LE. SIZE(obj%material)) THEN
  ans = obj%material(medium)
ELSE
  ans = 0
END IF
END PROCEDURE mesh_getMaterial

!----------------------------------------------------------------------------
!                                                      getTotalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalFacetElements
ans = obj%getTotalInternalFacetElements() &
  & + obj%getTotalBoundaryFacetElements()
END PROCEDURE mesh_getTotalFacetElements

!----------------------------------------------------------------------------
!                                              getTotalInternalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalInternalFacetElements
IF (ALLOCATED(obj%internalFacetData)) THEN
  ans = SIZE(obj%internalFacetData)
ELSE
  ans = 0
END IF
END PROCEDURE mesh_getTotalInternalFacetElements

!----------------------------------------------------------------------------
!                                              getTotalBoundaryFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalBoundaryFacetElements
IF (ALLOCATED(obj%boundaryFacetData)) THEN
  ans = SIZE(obj%boundaryFacetData)
ELSE
  ans = 0
END IF
END PROCEDURE mesh_getTotalBoundaryFacetElements

!----------------------------------------------------------------------------
!                                                       getMasterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getMasterCellNumber
  !!
SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
  ans = obj%internalFacetData(facetElement)%masterCellNumber
CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
  ans = obj%boundaryFacetData(facetElement)%masterCellNumber
END SELECT
  !!
END PROCEDURE mesh_getMasterCellNumber

!----------------------------------------------------------------------------
!                                                         getSlaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getSlaveCellNumber
  !!
SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
  ans = obj%internalFacetData(facetElement)%slaveCellNumber
CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
  ans = 0
END SELECT
  !!
END PROCEDURE mesh_getSlaveCellNumber

!----------------------------------------------------------------------------
!                                                              getCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getCellNumber
  !!
SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
  ans(1) = obj%internalFacetData(facetElement)%masterCellNumber
  ans(2) = obj%internalFacetData(facetElement)%slaveCellNumber
CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
  ans(1) = obj%boundaryFacetData(facetElement)%masterCellNumber
  ans(2) = 0
END SELECT
  !!
END PROCEDURE mesh_getCellNumber

!----------------------------------------------------------------------------
!                                                           getLocalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalFacetID
  !!
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
  !!
END PROCEDURE mesh_getLocalFacetID

!----------------------------------------------------------------------------
!                                                       getFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getFacetConnectivity1
  !!
INTEGER(I4B), ALLOCATABLE :: cellNptrs(:)
INTEGER(I4B) :: localFaceID, cellNum
  !!
SELECT CASE (elementType)
CASE (INTERNAL_ELEMENT)
    !!
  IF (isMaster) THEN
      !!
    cellNum = obj%internalFacetData(facetElement)%masterCellNumber
    localFaceID = obj%internalFacetData(facetElement)%masterLocalFacetID
      !!
  ELSE
      !!
    cellNum = obj%internalFacetData(facetElement)%slaveCellNumber
    localFaceID = obj%internalFacetData(facetElement)%slaveLocalFacetID
      !!
  END IF
    !!
CASE (DOMAIN_BOUNDARY_ELEMENT, BOUNDARY_ELEMENT)
    !!
  cellNum = obj%boundaryFacetData(facetElement)%masterCellNumber
  localFaceID = obj%boundaryFacetData(facetElement)%masterLocalFacetID
    !!
END SELECT
  !!
IF (cellNum .NE. 0) THEN
  cellNptrs = obj%getConnectivity(globalElement=cellNum)
  ans = cellNptrs(getConnectivity(obj%facetElements(localFaceID)))
ELSE
  ALLOCATE (ans(0))
END IF
  !!
IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
  !!
END PROCEDURE mesh_getFacetConnectivity1

!----------------------------------------------------------------------------
!                                                      getFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getFacetConnectivity2
INTEGER(I4B), ALLOCATABLE :: nptrs(:), indx(:)
  !!
nptrs = obj%getConnectivity(globalElement=globalElement)
indx = getConnectivity(obj%facetElements(iface))
ans = nptrs(indx)
  !!
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(indx)) DEALLOCATE (indx)
END PROCEDURE mesh_getFacetConnectivity2

!----------------------------------------------------------------------------
!                                                        getFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getFacetElementType
INTEGER(I4B) :: iel
iel = obj%getLocalElemNumber(globalElement=globalElement)
ans = obj%facetElementType(:, iel)
END PROCEDURE mesh_getFacetElementType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
