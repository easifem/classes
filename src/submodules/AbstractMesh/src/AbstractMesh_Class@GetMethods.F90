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
USE ReallocateUtility
USE IntegerUtility
USE AppendUtility
USE BoundingBox_Method
USE InputUtility
USE Display_Method
USE ReferenceElement_Method, ONLY: REFELEM_MAX_FACES, &
  & GetEdgeConnectivity,  &
  & GetFaceConnectivity,  &
  & ElementOrder, &
  & TotalEntities, &
  & RefElemGetGeoParam

IMPLICIT NONE

#ifdef MAX_NODES_IN_ELEM
INTEGER(I4B), PARAMETER :: MaxNodesInElement = MAX_NODES_IN_ELEM
#else
INTEGER(I4B), PARAMETER :: MaxNodesInElement = 125
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                  GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
INTEGER(I4B) :: iel

#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = 0

#ifdef DEBUG_VER

isok = ALLOCATED(obj%elementData(iel)%globalNodes)
IF (isok) ans = SIZE(obj%elementData(iel)%globalNodes)

#else

ans = SIZE(obj%elementData(iel)%globalNodes)

#endif

END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                                  GetMaxNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNNE
ans = obj%maxNNE
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
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE obj_GetBoundingEntity

!----------------------------------------------------------------------------
!                                                               GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs
INTEGER(I4B) :: ii
DO CONCURRENT(ii=1:SIZE(ans))
  ans(ii) = obj%nodeData(ii)%globalNodeNum
END DO
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                               GetNptrs_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
INTEGER(I4B) :: ii
DO CONCURRENT(ii=1:SIZE(obj%nodeData))
  nptrs(ii) = obj%nodeData(ii)%globalNodeNum
END DO
END PROCEDURE obj_GetNptrs_

!----------------------------------------------------------------------------
!                                                          GetInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
INTEGER(I4B) :: ii, dummy
dummy = obj%GetTotalInternalNodes()
ALLOCATE (ans(dummy))
dummy = 0
DO ii = 1, obj%tNodes
  IF (obj%nodeData(ii)%nodeType .EQ. INTERNAL_NODE) THEN
    dummy = dummy + 1
    ans(dummy) = obj%nodeData(ii)%globalNodeNum
  END IF
END DO
END PROCEDURE obj_GetInternalNptrs

!----------------------------------------------------------------------------
!                                                          GetInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetInternalNptrs_()"
LOGICAL(LGT) :: problem
#endif
INTEGER(I4B) :: ii, dummy

dummy = obj%GetTotalInternalNodes()

#ifdef DEBUG_VER
problem = dummy .GT. SIZE(nptrs)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: size of nptrs is not enough '//  &
    & 'it should be ateast '//tostring(dummy))
  RETURN
END IF
#endif

dummy = 0
DO ii = 1, obj%tNodes
  IF (obj%nodeData(ii)%nodeType .EQ. INTERNAL_NODE) THEN
    dummy = dummy + 1
    nptrs(dummy) = obj%nodeData(ii)%globalNodeNum
  END IF
END DO
END PROCEDURE obj_GetInternalNptrs_

!----------------------------------------------------------------------------
!                                                          GetBoundaryNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryNptrs
INTEGER(I4B) :: ii, dummy

dummy = obj%GetTotalBoundaryNodes()
CALL Reallocate(ans, dummy)
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
localnode = obj%GetLocalNodeNumber(GlobalNode, islocal=islocal)
ans = obj%nodeData(localnode)%nodeType .NE. INTERNAL_NODE
END PROCEDURE obj_isBoundaryNode

!----------------------------------------------------------------------------
!                                                           isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodePresent1
LOGICAL(LGT) :: abool, islocal0

islocal0 = Input(default=.FALSE., option=islocal)

IF (islocal0) THEN
  ans = (globalNode .GT. 0_I4B) .AND. (globalNode .LE. obj%tNodes)

ELSE

  abool = globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs
  ans = .NOT. abool
  IF (ans) THEN
    ans = obj%local_nptrs(globalNode) .GT. 0
  END IF

END IF

END PROCEDURE obj_isNodePresent1

!----------------------------------------------------------------------------
!                                                           isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodePresent2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  ans(ii) = obj%isNodePresent(globalNode(ii), islocal=islocal)
END DO
END PROCEDURE obj_isNodePresent2

!----------------------------------------------------------------------------
!                                                         GetNodeMask
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeMask
INTEGER(I4B) :: ii, jj, kk, tsize
LOGICAL(LGT) :: isok

isok = .NOT. PRESENT(local_nptrs)
mask = .FALSE.

IF (isok) THEN

  tsize = SIZE(obj%nodeData)
  DO CONCURRENT(ii=1:tsize)
    jj = obj%nodeData(ii)%globalNodeNum
    mask(jj) = .TRUE.
  END DO

  RETURN

END IF

tsize = SIZE(obj%nodeData)
DO CONCURRENT(ii=1:tsize)
  jj = obj%nodeData(ii)%globalNodeNum
  kk = local_nptrs(jj)
  mask(kk) = .TRUE.
END DO

END PROCEDURE obj_GetNodeMask

!----------------------------------------------------------------------------
!                                                           isAnyNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isAnyNodePresent
LOGICAL(LGT) :: cond(SIZE(globalNode))
INTEGER(I4B) :: ii, n

n = SIZE(globalNode)

DO ii = 1, n
  cond(ii) = obj%isNodePresent(globalNode=globalNode(ii), islocal=islocal)
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
  cond(ii) = obj%isNodePresent(globalNode=globalNode(ii), islocal=islocal)
END DO

ans = ALL(cond)
END PROCEDURE obj_isAllNodePresent

!----------------------------------------------------------------------------
!                                                           isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElementPresent
LOGICAL(LGT) :: isok
LOGICAL(LGT) :: islocal0

islocal0 = Input(default=.FALSE., option=islocal)

IF (islocal0) THEN
  ans = (globalElement .GT. 0_I4B) .AND. (globalElement .LT. obj%tElements)

ELSE
  isok = (globalElement .GT. obj%maxElemNum) .OR.  &
    & (globalElement .LT. obj%minElemNum)

  ans = .NOT. isok

  IF (ans) THEN
    ans = .NOT. (isok .OR. obj%local_elemNumber(globalElement) .EQ. 0)
  END IF
END IF

END PROCEDURE obj_isElementPresent

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryElement
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = obj%elementData(iel)%elementType .LE. BOUNDARY_ELEMENT
END PROCEDURE obj_isBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainBoundaryElement
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = obj%elementData(iel)%elementType .EQ. DOMAIN_BOUNDARY_ELEMENT
END PROCEDURE obj_isDomainBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainFacetElement
ans = obj%boundaryFacetData(facetElement)%elementType  &
  & .EQ. DOMAIN_BOUNDARY_ELEMENT
END PROCEDURE obj_isDomainFacetElement

!----------------------------------------------------------------------------
!                                                     GetTotalInternalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInternalNodes
INTEGER(I4B) :: ii
ans = 0
DO ii = 1, obj%tNodes
  IF (obj%nodeData(ii)%nodeType .EQ. INTERNAL_NODE) THEN
    ans = ans + 1
  END IF
END DO
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
INTEGER(I4B) :: tIntNodes
tIntNodes = obj%GetTotalInternalNodes()
ans = obj%tNodes - tIntNodes
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
INTEGER(I4B) :: nsd, tsize, ii
REAL(DFP) :: lim(6)
LOGICAL(LGT) :: mask(SIZE(nodes, 1), SIZE(nodes, 2))

lim = 0.0_DFP
nsd = SIZE(nodes, 1)
tsize = SIZE(mask, 2)

CALL obj%GetNodeMask(mask=mask(1, :), local_nptrs=local_nptrs)
DO ii = 2, nsd
  mask(ii, :) = mask(1, :)
END DO

lim(1:nsd * 2:2) = MINVAL(nodes(1:nsd, :), dim=2, mask=mask)
lim(2:nsd * 2:2) = MAXVAL(nodes(1:nsd, :), dim=2, mask=mask)

CALL Initiate(obj=ans, nsd=nsd, lim=lim)
END PROCEDURE obj_GetBoundingBox2

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity()"
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: iel

#ifdef DEBUG_VER
problem = .NOT. obj%isElementPresent(globalElement, islocal=islocal)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: problem in getting localElement number')
END IF
#endif

iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = obj%elementData(iel)%globalNodes
END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeConnectivity()"
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: iel, telem, ii, nn

telem = obj%GetTotalElements()

#ifdef DEBUG_VER
problem = (SIZE(VALUE, 2) .LT. telem) .OR. (SIZE(VALUE, 1) .LT. obj%maxNNE)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The size of value is not correct.')
END IF
#endif

DO iel = 1, telem
  nn = SIZE(obj%elementData(iel)%globalNodes)
  DO ii = 1, nn
    VALUE(ii, iel) = obj%elementData(iel)%globalNodes(ii)
  END DO
END DO

END PROCEDURE obj_GetNodeConnectivity

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
INTEGER(I4B) :: ii

DO ii = 1, SIZE(globalNode)
  ans(ii) = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
END DO

END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                        GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber2()"
LOGICAL(LGT) :: problem
#endif
LOGICAL(LGT) :: islocal0

#ifdef DEBUG_VER
problem = .NOT. obj%isNodePresent(globalnode, islocal=islocal)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalNode '//tostring(globalNode)// &
    ' is out of bound')
END IF
#endif

islocal0 = Input(option=islocal, default=.FALSE.)

IF (islocal0) THEN
  ans = globalNode
ELSE
  ans = obj%local_nptrs(globalNode)
END IF

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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalNodeNumber2()"
LOGICAL(LGT) :: problem

problem = (localNode .EQ. 0) .OR. (localNode .GT. obj%tNodes)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: localNode is out of bound.')
END IF
#endif

ans = obj%nodeData(localNode)%globalNodeNum
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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalNodeNumber2()"
LOGICAL(LGT) :: problem

problem = (localElement .EQ. 0) .OR. (LocalElement .GT. obj%tElements)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: localElement is out of bound.')
END IF
#endif

ans = obj%elementData(localElement)%globalElemNum
END PROCEDURE obj_GetGlobalElemNumber2

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalElement)
  ans(ii) = obj%GetLocalElemNumber(globalElement(ii), islocal=islocal)
END DO
END PROCEDURE obj_GetLocalElemNumber1

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemNumber2()"
LOGICAL(LGT) :: problem
#endif

LOGICAL(LGT) :: islocal0

islocal0 = Input(default=.FALSE., option=islocal)

IF (islocal0) THEN
  ans = globalElement
  RETURN
END IF

#ifdef DEBUG_VER

problem = (globalElement .LT. obj%minElemNum)  &
  & .OR. (globalElement .GT. obj%maxElemNum)

IF (problem) THEN
  ans = 0
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalElement '//tostring(globalElement)// &
    & ' not present.')
  RETURN
END IF

#endif

ans = obj%local_elemNumber(globalElement)

END PROCEDURE obj_GetLocalElemNumber2

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements1()"
LOGICAL(LGT) :: problem

problem = .NOT. obj%isNodePresent(globalNode, islocal=islocal)
IF (problem) THEN
  ALLOCATE (ans(0))
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalNode is not present')
END IF
#endif

ii = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
ans = obj%nodeData(ii)%globalElements
END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
INTEGER(I4B) :: ii, jj, kk, n, lnode(SIZE(globalNode)),  &
  & nn(SIZE(globalNode) + 1)

nn(1) = 1
n = SIZE(globalNode)

DO ii = 1, n
  lnode(ii) = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
  nn(ii + 1) = nn(ii) + SIZE(obj%nodeData(lnode(ii))%globalElements)
END DO

CALL Reallocate(ans, nn(n + 1) - 1)

DO ii = 1, n
  kk = 0
  DO jj = nn(ii), nn(ii + 1) - 1
    kk = kk + 1
    ans(jj) = obj%nodeData(lnode(ii))%globalElements(kk)
  END DO
END DO

CALL RemoveDuplicates(ans)
END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes1()"
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: i

#ifdef DEBUG_VER
problem = .NOT. obj%isNodePresent(globalNode=globalNode, islocal=islocal)
IF (problem) THEN
  ALLOCATE (ans(0))
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalNode is out of bound.')
  RETURN
END IF
#endif

i = obj%GetLocalNodeNumber(GlobalNode=GlobalNode, islocal=islocal)

#ifdef DEBUG_VER
IF (obj%isExtraNodeToNodesInitiated) THEN
  problem = .NOT. ALLOCATED(obj%nodeData(i)%extraGlobalNodes)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: extraGlobalNodes is not ALLOCATED.')
  END IF
END IF
#endif

IF (obj%isExtraNodeToNodesInitiated .AND. IncludeSelf) THEN
  CALL Append(ans, [globalNode], obj%nodeData(i)%globalNodes,  &
    & obj%nodeData(i)%extraGlobalNodes)
  RETURN
END IF

IF (obj%isExtraNodeToNodesInitiated .AND. (.NOT. IncludeSelf)) THEN
  CALL Append(ans, obj%nodeData(i)%globalNodes,  &
    & obj%nodeData(i)%extraGlobalNodes)
  RETURN
END IF

IF (IncludeSelf) THEN
  CALL Append(ans, [globalNode], obj%nodeData(i)%globalNodes)
  RETURN
END IF

ans = obj%nodeData(i)%globalNodes

END PROCEDURE obj_GetNodeToNodes1

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes2
INTEGER(I4B) :: ii, jj, kk, n, lnode(SIZE(globalNode)),  &
  & nn(SIZE(globalNode) + 1)

nn(1) = 1
n = SIZE(globalNode)

DO ii = 1, n
  lnode(ii) = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
  nn(ii + 1) = nn(ii) + SIZE(obj%nodeData(lnode(ii))%globalNodes)
END DO

CALL Reallocate(ans, nn(n + 1) - 1)

DO ii = 1, n
  kk = 0
  DO jj = nn(ii), nn(ii + 1) - 1
    kk = kk + 1
    ans(jj) = obj%nodeData(lnode(ii))%globalNodes(kk)
  END DO
END DO

CALL RemoveDuplicates(ans)

END PROCEDURE obj_GetNodeToNodes2

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements
LOGICAL(LGT) :: onlyElem
INTEGER(I4B) :: iel, tsize

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElementToElements()"
LOGICAL(LGT) :: problem
#endif

iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)

#ifdef DEBUG_VER
problem = .NOT. ALLOCATED(obj%elementData(iel)%globalElements)

IF (problem) THEN
  CALL Reallocate(ans, 0, 0)
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalElements not found! '//  &
    & 'local element number = '//tostring(iel))
  RETURN
END IF
#endif

onlyElem = Input(option=onlyElements, default=.FALSE.)

ASSOCIATE (indx => obj%elementData(iel)%globalElements)

  tsize = SIZE(indx)

  IF (onlyElem) THEN

    ALLOCATE (ans(tsize / 3, 1))
    ans(:, 1) = indx(1 :: 3)

  ELSE

    ALLOCATE (ans(tsize / 3, 3))
    ans = TRANSPOSE(RESHAPE(indx, [3, tsize / 3]))
  END IF

END ASSOCIATE

END PROCEDURE obj_GetElementToElements

!----------------------------------------------------------------------------
!                                                     GetBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryElementData
INTEGER(I4B) :: iel, tsize
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
tsize = SIZE(obj%elementData(iel)%boundaryData)
CALL Reallocate(ans, tsize)
ans = obj%elementData(iel)%boundaryData
END PROCEDURE obj_GetBoundaryElementData

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
CHARACTER(*), PARAMETER :: myName = "obj_GetOrder()"
ans = 0
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is not available')
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
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractMeshGetFacetConnectivity
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
  ans = obj%GetFacetConnectivity(iface=localFaceID, globalElement=cellNum)
ELSE
  ALLOCATE (ans(0))
END IF

IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
END PROCEDURE AbstractMeshGetFacetConnectivity

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetConnectivity2()"
#endif

INTEGER(I4B) :: iel, temp4(4), elemType, order,  &
  & con(MaxNodesInElement, REFELEM_MAX_FACES), &
  & ii, tFaceNodes(REFELEM_MAX_FACES)

iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)

SELECT CASE (obj%xidim)

CASE (1_I4B)
  CALL Reallocate(ans, 1)
  IF (iface .EQ. 1) THEN
    ans(1) = obj%elementData(iel)%globalNodes(1)
  ELSE
    ans(1) = obj%elementData(iel)%globalNodes(2)
  END IF

CASE (2_I4B)

  elemType = obj%elementData(iel)%name
  order = ElementOrder(elemType)

  CALL Reallocate(ans, order + 1)
  CALL GetEdgeConnectivity(elemType=elemType, con=con, order=order, &
    & opt=1_I4B)

  DO ii = 1, order + 1
    ans(ii) = obj%elementData(iel)%globalNodes(con(ii, iface))
  END DO

CASE (3_I4B)

  elemType = obj%elementData(iel)%name
  temp4 = TotalEntities(elemType)
  order = ElementOrder(elemType)

  CALL RefElemGetGeoParam(elemType=elemType,  &
    & faceCon=con,  &
    & faceOpt=1_I4B, &
    & order=order, &
    & tFaceNodes=tFaceNodes)

  CALL Reallocate(ans, tFaceNodes(iface))

#ifdef DEBUG_VER
  DO ii = 1, SIZE(ans)

    IF (con(ii, iface) .EQ. 0_I4B) THEN
      CALL Display(elemType, "elemType: ")
      CALL Display(temp4, "TotalEntities: ")
      CALL Display(order, "order: ")
      CALL Display(tFaceNodes, "tFaceNodes: ")
      CALL Display(iface, "iface: ")
      CALL Display(con, "con: ")
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: con(ii, iface) is zero')
      RETURN
    END IF

    ans(ii) = obj%elementData(iel)%globalNodes(con(ii, iface))
  END DO

#else

  DO ii = 1, SIZE(ans)
    ans(ii) = obj%elementData(iel)%globalNodes(con(ii, iface))
  END DO

#endif

END SELECT

END PROCEDURE obj_GetFacetConnectivity

!----------------------------------------------------------------------------
!                                                        GetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetElementType
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
ans = obj%facetElementType(:, iel)
END PROCEDURE obj_GetFacetElementType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
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

IF (PRESENT(tElements)) tElements = obj%tElements

IF (PRESENT(minX)) minX = obj%minX

IF (PRESENT(minY)) minY = obj%minY

IF (PRESENT(minZ)) minZ = obj%minZ

IF (PRESENT(maxX)) maxX = obj%maxX

IF (PRESENT(maxY)) maxY = obj%maxY

IF (PRESENT(maxZ)) maxZ = obj%maxZ
END PROCEDURE obj_GetParam

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
