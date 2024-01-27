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

SUBMODULE(AbstractMesh_Class) GetMethods
USE BaseType, ONLY: IntVector_
USE IntVector_Method
USE ReallocateUtility
USE BoundingBox_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
! ans = 0
! IF (ASSOCIATED(obj%refelem)) THEN
!   ans = .NNE.obj%refelem
! END IF
END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                                  GetMaxNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNNE
! ans = 0
! IF (ASSOCIATED(obj%refelem)) THEN
!   ans = .NNE.obj%refelem
! END IF
END PROCEDURE obj_GetMaxNNE

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = obj%tElements
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum
INTEGER(I4B) :: ii
CALL Reallocate(ans, obj%GetTotalElements())
DO ii = 1, SIZE(ans)
  ans(ii) = obj%GetGlobalElemNumber(localElement=ii)
END DO
END PROCEDURE obj_GetElemNum

!----------------------------------------------------------------------------
!                                                         GetBoundingEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingEntity
IF (ALLOCATED(obj%boundingEntity)) THEN
  ans = obj%boundingEntity
  RETURN
END IF
ALLOCATE (ans(0))
END PROCEDURE obj_GetBoundingEntity

!----------------------------------------------------------------------------
!                                                               GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs

END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                          GetInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs

END PROCEDURE obj_GetInternalNptrs

!----------------------------------------------------------------------------
!                                                          GetBoundaryNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryNptrs

END PROCEDURE obj_GetBoundaryNptrs

!----------------------------------------------------------------------------
!                                                            isBoundaryNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryNode

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
LOGICAL(LGT) :: cond(SIZE(globalNode))
INTEGER(I4B) :: ii, n

n = SIZE(globalNode)

DO ii = 1, n
  cond(ii) = obj%isNodePresent(globalNode=globalNode(ii))
END DO

ans = ANY(cond)
END PROCEDURE obj_isAnyNodePresent

!----------------------------------------------------------------------------
!                                                           isAllNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isAllNodePresent
LOGICAL(LGT) :: cond(SIZE(globalNode))
INTEGER(I4B) :: ii, n

n = SIZE(globalNode)

DO ii = 1, n
  cond(ii) = obj%isNodePresent(globalNode=globalNode(ii))
END DO

ans = ALL(cond)
END PROCEDURE obj_isAllNodePresent

!----------------------------------------------------------------------------
!                                                           isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElementPresent
LOGICAL(LGT) :: isok

isok = globalElement .GT. obj%maxElemNum &
  & .OR. globalElement .LT. obj%minElemNum &
  & .OR. obj%local_elemNumber(globalElement) .EQ. 0

ans = .NOT. isok
END PROCEDURE obj_isElementPresent

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryElement

END PROCEDURE obj_isBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainBoundaryElement

END PROCEDURE obj_isDomainBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainFacetElement
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

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeConnectivity

END PROCEDURE obj_GetNodeConnectivity

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  ans(ii) = obj%GetLocalNodeNumber(globalNode(ii))
END DO
END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                        GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
LOGICAL(LGT) :: abool

abool = globalNode .LT. obj%MinNptrs &
  & .OR. globalNode .GT. obj%maxNptrs

IF (abool) THEN
  ans = 0
  RETURN
END IF

ans = obj%local_nptrs(globalNode)
END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                        GetGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localNode)
  ans(ii) = obj%GetGlobalNodeNumber(localNode(ii))
END DO
END PROCEDURE obj_GetGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber2

END PROCEDURE obj_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                        GetGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%GetGlobalElemNumber(localElement(ii))
END DO
END PROCEDURE obj_GetGlobalElemNumber1

!----------------------------------------------------------------------------
!                                                        GetGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemNumber2

END PROCEDURE obj_GetGlobalElemNumber2

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalElement)
  ans(ii) = obj%GetLocalElemNumber(globalElement(ii))
END DO
END PROCEDURE obj_GetLocalElemNumber1

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber2
#ifdef DEBUG_VER
LOGICAL(LGT) :: abool
abool = globalElement .LT. obj%MinElemNum &
  & .OR. globalElement .GT. obj%maxElemNum

IF (abool) THEN
  ans = 0
  RETURN
END IF
#endif

ans = obj%local_elemNumber(globalElement)
END PROCEDURE obj_GetLocalElemNumber2

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1

END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii

DO ii = 1, SIZE(globalNode)
  ivec = obj%GetNodeToElements(globalNode=globalNode(ii))
  IF (ALLOCATED(ivec)) THEN
    IF (SIZE(ivec) .NE. 0) THEN
      CALL Append(intvec, ivec)
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

END PROCEDURE obj_GetNodeToNodes1

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii

DO ii = 1, SIZE(globalNode)
  ivec = obj%GetNodeToNodes(globalNode=globalNode(ii), &
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

END PROCEDURE obj_GetElementToElements

!----------------------------------------------------------------------------
!                                                     GetBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryElementData

END PROCEDURE obj_GetBoundaryElementData

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder

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

END PROCEDURE obj_GetXidimension

!----------------------------------------------------------------------------
!                                                                GetMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaterial
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
ans = 0

isok = ALLOCATED(obj%material)
IF (.NOT. isok) RETURN

isok = medium .LE. SIZE(obj%material)
IF (.NOT. isok) RETURN
#endif

ans = obj%material(medium)
END PROCEDURE obj_GetMaterial

!----------------------------------------------------------------------------
!                                                         GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMaterial
ans = 0
IF (ALLOCATED(obj%material)) THEN
  ans = SIZE(obj%material)
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

END PROCEDURE obj_GetTotalInternalFacetElements

!----------------------------------------------------------------------------
!                                              GetTotalBoundaryFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryFacetElements

END PROCEDURE obj_GetTotalBoundaryFacetElements

!----------------------------------------------------------------------------
!                                                       GetMasterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMasterCellNumber

END PROCEDURE obj_GetMasterCellNumber

!----------------------------------------------------------------------------
!                                                         GetSlaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSlaveCellNumber

END PROCEDURE obj_GetSlaveCellNumber

!----------------------------------------------------------------------------
!                                                              GetCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellNumber

END PROCEDURE obj_GetCellNumber

!----------------------------------------------------------------------------
!                                                           GetLocalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetID

END PROCEDURE obj_GetLocalFacetID

!----------------------------------------------------------------------------
!                                                       GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity1

END PROCEDURE obj_GetFacetConnectivity1

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity2

END PROCEDURE obj_GetFacetConnectivity2

!----------------------------------------------------------------------------
!                                                        GetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetElementType

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

IF (PRESENT(tElements_topology_wise))  &
  & tElements_topology_wise = obj%tElements_topology_wise

IF (PRESENT(tElemTopologies)) tElemTopologies = obj%tElemTopologies

IF (PRESENT(elemTopologies)) elemTopologies = obj%elemTopologies

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
