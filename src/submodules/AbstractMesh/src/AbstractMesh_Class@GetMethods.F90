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
USE ReallocateUtility, ONLY: Reallocate
USE IntegerUtility, ONLY: RemoveDuplicates, RemoveDuplicates_
USE AppendUtility, ONLY: Append
USE BoundingBox_Method, ONLY: InitBox => Initiate
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: Display, ToString
USE ReferenceElement_Method, ONLY: &
  REFELEM_MAX_FACES => PARAM_REFELEM_MAX_FACES, &
  GetEdgeConnectivity, &
  GetFaceConnectivity, &
  ElementOrder, &
  TotalEntities, &
  RefElemGetGeoParam
USE FacetData_Class, ONLY: FacetData_Iselement, &
                           FacetData_GetParam
USE ElemData_Class, ONLY: INTERNAL_ELEMENT, &
                          BOUNDARY_ELEMENT, &
                          DOMAIN_BOUNDARY_ELEMENT, &
                          ElemData_GetTotalEntities, &
                          ElemData_GetConnectivity, &
                          ElemData_GetElementToElements
USE NodeData_Class, ONLY: INTERNAL_NODE, &
                          BOUNDARY_NODE

IMPLICIT NONE

#ifdef MAX_NODES_IN_ELEM
INTEGER(I4B), PARAMETER :: MaxNodesInElement = MAX_NODES_IN_ELEM
#else
INTEGER(I4B), PARAMETER :: MaxNodesInElement = 125
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              GetElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemData
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
elemdata = obj%elementData(iel)
END PROCEDURE obj_GetElemData

!----------------------------------------------------------------------------
!                                                       GetElemDataPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemDataPointer
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans => obj%elementData(iel)
END PROCEDURE obj_GetElemDataPointer

!----------------------------------------------------------------------------
!                                                                  GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = SIZE(obj%elementData(iel)%globalNodes)
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
  ans(ii) = obj%GetglobalElemNumber(localElement=ii)
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
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs
INTEGER(I4B) :: ii
DO CONCURRENT(ii=1:SIZE(ans))
  ans(ii) = obj%nodeData(ii)%globalNodeNum
END DO
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                                  GetNptrs_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
INTEGER(I4B) :: ii, n
n = SIZE(obj%nodeData)
DO CONCURRENT(ii=1:n)
  nptrs(ii) = obj%nodeData(ii)%globalNodeNum
END DO
IF (PRESENT(tsize)) tsize = n
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
    & 'it should be ateast '//ToString(dummy))
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
localnode = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
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
  ans = (globalElement .GT. 0_I4B) .AND. (globalElement .LE. obj%tElements)

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
ans = FacetData_Iselement(obj=obj%facetData(facetElement), &
                          filter=DOMAIN_BOUNDARY_ELEMENT)
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
!                                                              GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes
ans = obj%tNodes
END PROCEDURE obj_GetTotalNodes

!----------------------------------------------------------------------------
!                                                             GetTotalFaces
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFaces
ans = obj%tFaces
END PROCEDURE obj_GetTotalFaces

!----------------------------------------------------------------------------
!                                                             GetTotalEdges
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEdges
ans = obj%tEdges
END PROCEDURE obj_GetTotalEdges

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
CALL InitBox(obj=ans, nsd=3_I4B, lim=lim)
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

CALL InitBox(obj=ans, nsd=nsd, lim=lim)
END PROCEDURE obj_GetBoundingBox2

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
INTEGER(I4B) :: tsize
INTEGER(I4B) :: temp(PARAM_MAX_CONNECTIVITY_SIZE)
CALL obj%GetConnectivity_(globalElement=globalElement, &
                          ans=temp, tsize=tsize, opt=opt, islocal=islocal)
ALLOCATE (ans(tsize))
ans(1:tsize) = temp(1:tsize)
END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity_
INTEGER(I4B) :: iel

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity_()"
LOGICAL(LGT) :: problem

problem = .NOT. obj%isElementPresent(globalElement, islocal=islocal)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: problem in getting localElement number')
END IF
#endif

iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)

CALL ElemData_GetConnectivity(obj=obj%elementData(iel), con=ans, &
                              tsize=tsize, opt=opt)

END PROCEDURE obj_GetConnectivity_

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
LOGICAL(LGT) :: islocal0

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber2()"
LOGICAL(LGT) :: problem

problem = .NOT. obj%isNodePresent(globalnode, islocal=islocal)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalNode '//ToString(globalNode)// &
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
!                                                        GetglobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetglobalNodeNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localNode)
  ans(ii) = obj%GetglobalNodeNumber(localNode(ii))
END DO
END PROCEDURE obj_GetglobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetglobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetglobalNodeNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetglobalNodeNumber2()"
LOGICAL(LGT) :: problem

problem = (localNode .EQ. 0) .OR. (localNode .GT. obj%tNodes)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: localNode is out of bound.')
END IF
#endif

ans = obj%nodeData(localNode)%globalNodeNum
END PROCEDURE obj_GetglobalNodeNumber2

!----------------------------------------------------------------------------
!                                                        GetglobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetglobalElemNumber1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%GetglobalElemNumber(localElement(ii))
END DO
END PROCEDURE obj_GetglobalElemNumber1

!----------------------------------------------------------------------------
!                                                        GetglobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetglobalElemNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetglobalNodeNumber2()"
LOGICAL(LGT) :: problem

problem = (localElement .EQ. 0) .OR. (LocalElement .GT. obj%tElements)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: localElement is out of bound.')
END IF
#endif

ans = obj%elementData(localElement)%globalElemNum
END PROCEDURE obj_GetglobalElemNumber2

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
CHARACTER(*), PARAMETER :: myName = "obj_GetglobalElemNumber2()"
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
    & '[INTERNAL ERROR] :: globalElement '//ToString(globalElement)// &
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

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

ii = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
ans = obj%nodeData(ii)%globalElements
END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
INTEGER(I4B) :: ii, jj, kk, n, lnode(SIZE(globalNode)),  &
  & nn(SIZE(globalNode) + 1)

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

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
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1_
INTEGER(I4B) :: ii, jj
LOGICAL(LGT) :: problem

tsize = 0
problem = .NOT. obj%isNodePresent(globalNode, islocal=islocal)
IF (problem) RETURN

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

ii = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
tsize = SIZE(obj%nodeData(ii)%globalElements)

DO jj = 1, tsize
  ans(jj) = obj%nodeData(ii)%globalElements(jj)
END DO
END PROCEDURE obj_GetNodeToElements1_

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2_
INTEGER(I4B) :: ii, jj, kk, n, lnode, a, b

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

a = 1
n = SIZE(globalNode)

DO ii = 1, n
  lnode = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
  b = a + SIZE(obj%nodeData(lnode)%globalElements)

  kk = 0
  DO jj = a, b - 1
    kk = kk + 1
    ans(jj) = obj%nodeData(lnode)%globalElements(kk)
  END DO

  b = a

END DO

tsize = b - 1

IF (tsize .LE. 1) RETURN

CALL RemoveDuplicates_(obj=ans(1:tsize), tsize=tsize, isSorted=.FALSE.)
END PROCEDURE obj_GetNodeToElements2_

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes1()"
LOGICAL(LGT) :: problem
#endif

LOGICAL(LGT) :: abool

INTEGER(I4B) :: i, j

#ifdef DEBUG_VER
problem = .NOT. obj%isNodePresent(globalNode=globalNode, islocal=islocal)
IF (problem) THEN
  ALLOCATE (ans(0))
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalNode is out of bound.')
  RETURN
END IF
#endif

i = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)

#ifdef DEBUG_VER
IF (obj%isExtraNodeToNodesInitiated) THEN
  problem = .NOT. ALLOCATED(obj%nodeData(i)%extraglobalNodes)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: extraglobalNodes is not ALLOCATED.')
  END IF
END IF
#endif

abool = obj%isExtraNodeToNodesInitiated .AND. IncludeSelf
IF (abool) THEN
  j = obj%GetglobalNodeNumber(i)
  CALL Append(ans, [j], obj%nodeData(i)%globalNodes,  &
    & obj%nodeData(i)%extraglobalNodes)
  RETURN
END IF

abool = obj%isExtraNodeToNodesInitiated .AND. (.NOT. IncludeSelf)
IF (abool) THEN
  CALL Append(ans, obj%nodeData(i)%globalNodes,  &
    & obj%nodeData(i)%extraglobalNodes)
  RETURN
END IF

IF (IncludeSelf) THEN
  j = obj%GetglobalNodeNumber(i)
  CALL Append(ans, [j], obj%nodeData(i)%globalNodes)
  RETURN
END IF

ans = obj%nodeData(i)%globalNodes

END PROCEDURE obj_GetNodeToNodes1

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes2
INTEGER(I4B) :: ii, n, tsize, lnode
INTEGER(I4B), ALLOCATABLE :: temp(:)

n = SIZE(globalNode)
tsize = 0
DO ii = 1, n
  lnode = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
  tsize = tsize + SIZE(obj%nodeData(lnode)%globalNodes)
END DO

IF (includeSelf) THEN
  CALL Reallocate(temp, tsize + n)
ELSE
  CALL Reallocate(temp, tsize)
END IF

CALL obj%GetNodeToNodes_(globalNode=globalNode, includeSelf=includeSelf, &
                         ans=temp, tsize=tsize, islocal=islocal)
CALL Reallocate(ans, tsize)
ans = temp(1:tsize)

END PROCEDURE obj_GetNodeToNodes2

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes1_()"
LOGICAL(LGT) :: problem
#endif

LOGICAL(LGT) :: abool

INTEGER(I4B) :: i, a

tsize = 0
#ifdef DEBUG_VER

problem = .NOT. obj%isNodePresent(globalNode=globalNode, islocal=islocal)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalNode not present.')
  RETURN
END IF

#endif

i = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)

#ifdef DEBUG_VER
IF (obj%isExtraNodeToNodesInitiated) THEN
  problem = .NOT. ALLOCATED(obj%nodeData(i)%extraglobalNodes)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: extraglobalNodes is not ALLOCATED.')
  END IF
END IF
#endif

#ifdef DEBUG_VER

a = 0
IF (IncludeSelf) THEN

  a = 1
  tsize = 1

  problem = SIZE(ans) .LT. 1
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: size of ans is not enough')
    RETURN
  END IF

  ans(1) = obj%GetglobalNodeNumber(i)

END IF

#else

a = 0
IF (IncludeSelf) THEN

  a = 1
  tsize = 1
  ans(1) = obj%GetglobalNodeNumber(i)

END IF

#endif

tsize = a + SIZE(obj%nodeData(i)%globalNodes)

#ifdef DEBUG_VER

! problem = size ans .lt. 1
problem = SIZE(ans) .LT. tsize
! call raiseError if problem is true
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: size of ans is not enough')
  RETURN
END IF

ans(a + 1:tsize) = obj%nodedata(i)%globalNodes

#else

ans(a + 1:tsize) = obj%nodedata(i)%globalNodes

#endif

! exatranodes

abool = obj%isExtraNodeToNodesInitiated

#ifdef DEBUG_VER

IF (abool) THEN

  a = tsize
  tsize = tsize + SIZE(obj%nodeData(i)%extraglobalNodes)

  problem = SIZE(ans) .LT. tsize
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: size of ans is not enough')
    RETURN
  END IF

  ans(a + 1:tsize) = obj%nodedata(i)%extraglobalNodes

END IF

#else

IF (abool) THEN

  a = tsize
  tsize = tsize + SIZE(obj%nodeData(i)%extraglobalNodes)
  ans(a + 1:tsize) = obj%nodedata(i)%extraglobalNodes

END IF

#endif

END PROCEDURE obj_GetNodeToNodes1_

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes2_
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes2_()"
LOGICAL(LGT) :: problem

LOGICAL(LGT) :: abool

INTEGER(I4B) :: i, a, jj

tsize = 0
a = 0

DO jj = 1, SIZE(globalNode)

 problem = .NOT. obj%isNodePresent(globalNode=globalNode(jj), islocal=islocal)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: globalNode node present.')
    RETURN
  END IF

  i = obj%GetLocalNodeNumber(globalNode=globalNode(jj), islocal=islocal)

  IF (obj%isExtraNodeToNodesInitiated) THEN
    problem = .NOT. ALLOCATED(obj%nodeData(i)%extraglobalNodes)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: extraglobalNodes is not ALLOCATED.')
    END IF
    RETURN
  END IF

  IF (IncludeSelf) THEN

    ans(tsize + 1) = obj%GetglobalNodeNumber(i)
    a = a + 1
    tsize = tsize + 1

  END IF

  tsize = a + SIZE(obj%nodeData(i)%globalNodes)
  ans(a + 1:tsize) = obj%nodedata(i)%globalNodes
  a = tsize

  abool = obj%isExtraNodeToNodesInitiated
  IF (abool) THEN

    tsize = tsize + SIZE(obj%nodeData(i)%extraglobalNodes)
    ans(a + 1:tsize) = obj%nodedata(i)%extraglobalNodes
    a = tsize

  END IF

END DO

CALL RemoveDuplicates_(obj=ans(1:tsize), tsize=tsize, isSorted=.FALSE.)

END PROCEDURE obj_GetNodeToNodes2_

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements
LOGICAL(LGT) :: onlyElem
INTEGER(I4B) :: nrow, temp(REFELEM_MAX_FACES, 3), ii, ncol, jj

onlyElem = Input(default=.FALSE., option=onlyElem)

IF (onlyElem) THEN
  CALL obj%GetElementToElements_(globalElement=globalElement, &
                                 islocal=islocal, ans=temp(:, 1), tsize=nrow)
  ALLOCATE (ans(nrow, 1))
  DO ii = 1, nrow; ans(ii, 1) = temp(ii, 1); END DO
  RETURN

END IF

CALL obj%GetElementToElements_(globalElement=globalelement, &
                              islocal=islocal, ans=temp, nrow=nrow, ncol=ncol)
ALLOCATE (ans(nrow, ncol))

DO jj = 1, ncol; DO ii = 1, nrow; ans(ii, jj) = temp(ii, jj); END DO; END DO
END PROCEDURE obj_GetElementToElements

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements1_
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
CALL ElemData_GetElementToElements(obj=obj%elementData(iel), ans=ans, &
                                   tsize=tsize)
END PROCEDURE obj_GetElementToElements1_

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements2_
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
CALL ElemData_GetElementToElements(obj=obj%elementData(iel), ans=ans, &
                                   nrow=nrow, ncol=ncol)
END PROCEDURE obj_GetElementToElements2_

!----------------------------------------------------------------------------
!                                                     GetBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryElementData
INTEGER(I4B) :: iel, tsize, ii
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
tsize = SIZE(obj%elementData(iel)%boundaryData)
CALL Reallocate(ans, tsize)
DO ii = 1, tsize
  ans(ii) = obj%elementData(iel)%boundaryData(ii)
END DO
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

MODULE PROCEDURE obj_GetMaterial1
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = obj%elementData(iel)%material(medium)
END PROCEDURE obj_GetMaterial1

!----------------------------------------------------------------------------
!                                                         GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMaterial1
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = 0 ! default value
IF (ALLOCATED(obj%elementData(iel)%material)) THEN
  ans = SIZE(obj%elementData(iel)%material)
END IF
END PROCEDURE obj_GetTotalMaterial1

!----------------------------------------------------------------------------
!                                                      GetTotalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFacetElements
ans = 0
IF (ALLOCATED(obj%facetData)) ans = SIZE(obj%facetData)
END PROCEDURE obj_GetTotalFacetElements

!----------------------------------------------------------------------------
!                                              GetTotalInternalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInternalFacetElements
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize

ans = 0
isok = ALLOCATED(obj%facetData)
IF (.NOT. isok) RETURN

tsize = SIZE(obj%facetData)
DO ii = 1, tsize
  isok = FacetData_Iselement(obj=obj%facetData(ii), filter=INTERNAL_ELEMENT)
  IF (isok) ans = ans + 1
END DO

END PROCEDURE obj_GetTotalInternalFacetElements

!----------------------------------------------------------------------------
!                                              GetTotalBoundaryFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryFacetElements
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize

ans = 0
isok = ALLOCATED(obj%facetData)
IF (.NOT. isok) RETURN
tsize = SIZE(obj%facetData)

DO ii = 1, tsize
  isok = FacetData_Iselement(obj=obj%facetData(ii), filter=BOUNDARY_ELEMENT)
  IF (isok) ans = ans + 1
END DO

DO ii = 1, tsize
  isok = FacetData_Iselement(obj=obj%facetData(ii), &
                             filter=DOMAIN_BOUNDARY_ELEMENT)
  IF (isok) ans = ans + 1
END DO
END PROCEDURE obj_GetTotalBoundaryFacetElements

!----------------------------------------------------------------------------
!                                                       GetMasterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMasterCellNumber
CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                        masterCellNumber=ans)
END PROCEDURE obj_GetMasterCellNumber

!----------------------------------------------------------------------------
!                                                         GetSlaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSlaveCellNumber
CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                        slaveCellNumber=ans)
END PROCEDURE obj_GetSlaveCellNumber

!----------------------------------------------------------------------------
!                                                              GetCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellNumber
CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                        masterCellNumber=ans(1), slaveCellNumber=ans(2))
END PROCEDURE obj_GetCellNumber

!----------------------------------------------------------------------------
!                                                           GetLocalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetID
IF (isMaster) THEN
  CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                          masterLocalFacetID=ans)
ELSE
  CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                          slaveLocalFacetID=ans)
END IF
END PROCEDURE obj_GetLocalFacetID

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractMeshGetFacetConnectivity
INTEGER(I4B) :: localFaceID, cellNum

IF (isMaster) THEN
  CALL FacetData_GetParam(obj=obj%facetData(facetElement),  &
    & masterCellNumber=cellNum, masterLocalFacetID=localFaceID)
ELSE
  CALL FacetData_GetParam(obj=obj%facetData(facetElement),  &
    & slaveCellNumber=cellNum, slaveLocalFacetID=localFaceID)
END IF

IF (cellNum .EQ. 0) THEN
  ALLOCATE (ans(0))
  RETURN
END IF

ans = obj%GetFacetConnectivity(iface=localFaceID, globalElement=cellNum)

END PROCEDURE AbstractMeshGetFacetConnectivity

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity
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

  DO ii = 1, tFaceNodes(iface)
    ans(ii) = obj%elementData(iel)%globalNodes(con(ii, iface))
  END DO

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
!                                                           GetMaxNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNodeNumber
ans = obj%maxNptrs
END PROCEDURE obj_GetMaxNodeNumber

!----------------------------------------------------------------------------
!                                                                 isInit
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isInit
ans = obj%isInitiated
END PROCEDURE obj_isInit

!----------------------------------------------------------------------------
!                                                              isNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodeToElements
ans = obj%isNodeToElementsInitiated
END PROCEDURE obj_isNodeToElements

!----------------------------------------------------------------------------
!                                                              isNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodeToNodes
ans = obj%isNodeToNodesInitiated
END PROCEDURE obj_isNodeToNodes

!----------------------------------------------------------------------------
!                                                         isExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isExtraNodeToNodes
ans = obj%isExtraNodeToNodesInitiated
END PROCEDURE obj_isExtraNodeToNodes

!----------------------------------------------------------------------------
!                                                       isElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElementToElements
ans = obj%isElementToElementsInitiated
END PROCEDURE obj_isElementToElements

!----------------------------------------------------------------------------
!                                                       isEdgeConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isEdgeConnectivity
ans = obj%isEdgeConnectivityInitiated
END PROCEDURE obj_isEdgeConnectivity

!----------------------------------------------------------------------------
!                                                         isFaceConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isFaceConnectivity
ans = obj%isFaceConnectivityInitiated
END PROCEDURE obj_isFaceConnectivity

!----------------------------------------------------------------------------
!                                                             isBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryData
ans = obj%isBoundaryDataInitiated
END PROCEDURE obj_isBoundaryData

!----------------------------------------------------------------------------
!                                                                 isFaceData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isFacetData
ans = obj%isFacetDataInitiated
END PROCEDURE obj_isFacetData

!----------------------------------------------------------------------------
!                                                           isElementActive
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElementActive
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = obj%elementData(iel)%isActive
END PROCEDURE obj_isElementActive

!----------------------------------------------------------------------------
!                                                           GetFacetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetParam
CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                        elementType=elementType)
END PROCEDURE obj_GetFacetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntities1
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = ElemData_GetTotalEntities(obj%elementData(iel))
END PROCEDURE obj_GetTotalEntities1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntities2
ans(1) = obj%tNodes
ans(2) = obj%tEdges
ans(3) = obj%tFaces
ans(4) = obj%tElements
END PROCEDURE obj_GetTotalEntities2

!----------------------------------------------------------------------------
!                                                           GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord2
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeeCoord2()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetNodeCoord2

END SUBMODULE GetMethods
