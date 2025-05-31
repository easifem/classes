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
USE HashTables, ONLY: HashTable_, Hashkey

USE GlobalData, ONLY: MaxDFP, MinDFP

USE ReallocateUtility, ONLY: Reallocate

USE IntegerUtility, ONLY: RemoveDuplicates, RemoveDuplicates_

USE AppendUtility, ONLY: Append

USE BoundingBox_Method, ONLY: Center, GetRadiusSqr, isInside, &
                              BoundingBox_Initiate => Initiate

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

USE Elemdata_Class, ONLY: INTERNAL_ELEMENT, &
                          BOUNDARY_ELEMENT, &
                          DOMAIN_BOUNDARY_ELEMENT, &
                          Elemdata_GetTotalEntities, &
                          Elemdata_GetConnectivity, &
                          Elemdata_GetConnectivity2, &
                          Elemdata_GetElementToElements, &
                          Elemdata_GetGlobalNodesPointer, &
                          Elemdata_GetTotalGlobalElements, &
                          Elemdata_name, &
                          Elemdata_topoName, &
                          Elemdata_topoIndx, &
                          Elemdata_GetOrientation, &
                          Elemdata_Meshid, &
                          Elemdata_localElemNum, &
                          Elemdata_globalElemNum, &
                          Elemdata_GetTotalGlobalNodes, &
                          Elemdata_IsBoundaryElement, &
                          Elemdata_FindFace, &
                          Elemdata_FindEdge, &
                          Elemdata_GetGlobalFaceNumber, &
                          Elemdata_GetGlobalEdgeNumber, &
                          Elemdata_Order

USE NodeData_Class, ONLY: INTERNAL_NODE, BOUNDARY_NODE, &
                          NodeData_GetNodeType, &
                          NodeData_GetGlobalNodeNum, &
                          NodeData_GetTotalGlobalElements, &
                          NodeData_GetGlobalElements, &
                          NodeData_GetTotalGlobalNodes, &
                          NodeData_GetGlobalNodes, &
                          NodeData_GetGlobalNodes2, &
                          NodeData_GetExtraGlobalNodes, &
                          NodeData_GetTotalExtraGlobalNodes, &
                          NodeData_GetNodeCoord

USE Kdtree2_Module, ONLY: Kdtree2_r_nearest, Kdtree2_n_nearest

IMPLICIT NONE

#ifdef MAX_NODES_IN_ELEM
INTEGER(I4B), PARAMETER :: MaxNodesInElement = MAX_NODES_IN_ELEM
#else
INTEGER(I4B), PARAMETER :: MaxNodesInElement = 125
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                             GetTotalElementsTopologyWise
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElementsTopologyWise
ans = obj%tElements_topology_wise
END PROCEDURE obj_GetTotalElementsTopologyWise

!----------------------------------------------------------------------------
!                                                           GetTotalTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalTopology
ans = obj%tElemTopologies
END PROCEDURE obj_GetTotalTopology

!----------------------------------------------------------------------------
!                                                       GetElemTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemTopology1
ans = obj%elemTopologies
END PROCEDURE obj_GetElemTopology1

!----------------------------------------------------------------------------
!                                                            GetElemTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemTopology2
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalelement, islocal=islocal)
ans = Elemdata_topoName(obj%elementData(iel)%ptr)
END PROCEDURE obj_GetElemTopology2

!----------------------------------------------------------------------------
!                                                        GetElemTopologyIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemTopologyIndx
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalelement, islocal=islocal)
ans = Elemdata_topoIndx(obj%elementData(iel)%ptr)
END PROCEDURE obj_GetElemTopologyIndx

!----------------------------------------------------------------------------
!                                                                GetElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemType
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalelement, islocal=islocal)
ans = Elemdata_name(obj%elementData(iel)%ptr)
END PROCEDURE obj_GetElemType

!----------------------------------------------------------------------------
!                                                              GetElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemData
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
elemdata = obj%elementData(iel)%ptr
END PROCEDURE obj_GetElemData

!----------------------------------------------------------------------------
!                                                       GetElemDataPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemDataPointer
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans => obj%elementData(iel)%ptr
END PROCEDURE obj_GetElemDataPointer

!----------------------------------------------------------------------------
!                                                                  GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = SIZE(obj%elementData(iel)%ptr%globalNodes)
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
!                                                           GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements2
INTEGER(I4B) :: ii, found
LOGICAL(LGT) :: isok

ans = 0
DO ii = 1, obj%tElements
  isok = obj%IsElementActive(globalElement=ii, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  found = Elemdata_MeshID(obj=obj%elementData(ii)%ptr)
  IF (found .EQ. meshid) ans = ans + 1

END DO

END PROCEDURE obj_GetTotalElements2

!----------------------------------------------------------------------------
!                                                           GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements3
INTEGER(I4B) :: ii, found
LOGICAL(LGT) :: isok

ans = 0
DO ii = 1, obj%tElements
  isok = obj%IsElementActive(globalElement=ii, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  found = Elemdata_MeshID(obj=obj%elementData(ii)%ptr)
  isok = ANY(found .EQ. meshid)
  IF (isok) ans = ans + 1
END DO
END PROCEDURE obj_GetTotalElements3

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum1
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: islocal0

tsize = obj%GetTotalElements()
ALLOCATE (ans(tsize))
islocal0 = .FALSE.
IF (PRESENT(islocal)) islocal0 = islocal

CALL obj%GetElemNum_(ans=ans, tsize=tsize, islocal=islocal0)
END PROCEDURE obj_GetElemNum1

!----------------------------------------------------------------------------
!                                                           GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum2
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: islocal0

tsize = obj%GetTotalElements(meshid=meshid)
ALLOCATE (ans(tsize))
islocal0 = .FALSE.
IF (PRESENT(islocal)) islocal0 = islocal

CALL obj%GetElemNum_(meshid=meshid, ans=ans, tsize=tsize, islocal=islocal0)
END PROCEDURE obj_GetElemNum2

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum1_
INTEGER(I4B) :: ii

tsize = obj%GetTotalElements()
IF (islocal) THEN

  DO ii = 1, tsize
    ans(ii) = Elemdata_localElemNum(obj%elementData(ii)%ptr)
  END DO

  RETURN
END IF

DO ii = 1, tsize
  ans(ii) = Elemdata_globalElemNum(obj%elementData(ii)%ptr)
END DO

END PROCEDURE obj_GetElemNum1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum2_
INTEGER(I4B) :: ii, found, fake_tsize
LOGICAL(LGT) :: isok

fake_tsize = obj%GetTotalElements()
tsize = 0

IF (islocal) THEN

  DO ii = 1, fake_tsize

    isok = obj%IsElementActive(globalElement=ii, islocal=.TRUE.)
    IF (.NOT. isok) CYCLE

    found = Elemdata_Meshid(obj=obj%elementData(ii)%ptr)
    IF (found .EQ. meshid) THEN
      tsize = tsize + 1
      ans(tsize) = Elemdata_localElemNum(obj%elementData(ii)%ptr)
    END IF

  END DO

  RETURN
END IF

DO ii = 1, fake_tsize

  isok = obj%IsElementActive(globalElement=ii, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  found = Elemdata_Meshid(obj=obj%elementData(ii)%ptr)
  IF (found .EQ. meshid) THEN
    tsize = tsize + 1
    ans(tsize) = Elemdata_globalElemNum(obj%elementData(ii)%ptr)
  END IF

END DO

END PROCEDURE obj_GetElemNum2_

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum3_
INTEGER(I4B) :: tmeshid, mysize, ii

tsize = 0
tmeshid = SIZE(meshid)

DO ii = 1, tmeshid
  CALL obj%GetElemNum_(meshid=meshid(ii), islocal=islocal, &
                       ans=ans(tsize + 1:), tsize=mysize)
  tsize = tsize + mysize
END DO
END PROCEDURE obj_GetElemNum3_

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

MODULE PROCEDURE obj_GetNptrs1
INTEGER(I4B) :: tsize
CALL obj%GetNptrs_(ans=ans, tsize=tsize)
END PROCEDURE obj_GetNptrs1

!----------------------------------------------------------------------------
!                                                                GetNptrs2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs2
INTEGER(I4B) :: tsize
tsize = obj%GetTotalNodes(meshid=meshid)
ALLOCATE (ans(tsize))
CALL obj%GetNptrs_(ans=ans, tsize=tsize, meshid=meshid)
END PROCEDURE obj_GetNptrs2

!----------------------------------------------------------------------------
!                                                                  GetNptrs_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs1_
INTEGER(I4B) :: ii
tsize = SIZE(obj%nodeData)
DO CONCURRENT(ii=1:tsize)
  ans(ii) = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
END DO
END PROCEDURE obj_GetNptrs1_

!----------------------------------------------------------------------------
!                                                                 GetNptrs2_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs2_
INTEGER(I4B) :: ii, jj, kk, ll, fake_tsize, found_meshid
LOGICAL(LGT) :: isok
LOGICAL(LGT), ALLOCATABLE :: foundNodes(:)
INTEGER(I4B), POINTER :: intptr(:)

fake_tsize = obj%GetTotalElements()
tsize = 0

ii = obj%GetMaxNodeNumber()
ALLOCATE (foundNodes(ii))
foundNodes = .FALSE.

DO ii = 1, fake_tsize
  isok = obj%IsElementActive(globalElement=ii, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  found_meshid = Elemdata_meshid(obj%elementData(ii)%ptr)

  isok = found_meshid .EQ. meshid
  IF (.NOT. isok) CYCLE

  intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(ii)%ptr)
  jj = Elemdata_GetTotalGlobalNodes(obj%elementData(ii)%ptr)

  DO kk = 1, jj
    ll = intptr(kk)

    isok = foundNodes(ll)
    IF (isok) CYCLE

    foundNodes(ll) = .TRUE.
    tsize = tsize + 1
    ans(tsize) = ll

  END DO

END DO

DEALLOCATE (foundNodes)
intptr => NULL()

END PROCEDURE obj_GetNptrs2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs3_
INTEGER(I4B) :: ii, jj, kk, ll, ierr, telem, iel
TYPE(HashTable_) :: tbl
INTEGER(I4B), POINTER :: intptr(:)
LOGICAL(LGT) :: isok

tsize = 0
telem = SIZE(globalElement)

DO ii = 1, telem
  iel = obj%GetLocalElemNumber(globalElement=globalElement(ii), &
                               islocal=islocal)

  isok = obj%IsElementActive(globalElement=iel, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(iel)%ptr)
  jj = Elemdata_GetTotalGlobalNodes(obj%elementData(iel)%ptr)

  DO kk = 1, jj
    ll = intptr(kk)

    !! check if the key is present
    CALL tbl%check_key(key=Hashkey(ll), stat=ierr)
    isok = ierr .EQ. 0_I4B
    IF (isok) CYCLE

    !! if not present then set the key
    CALL tbl%Set(key=Hashkey(ll), VALUE=.TRUE.)
    tsize = tsize + 1
    ans(tsize) = ll

  END DO

END DO

intptr => NULL()

END PROCEDURE obj_GetNptrs3_

!----------------------------------------------------------------------------
!                                                               GetNptrsInBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrsInBox
INTEGER(I4B) :: tnodes, ii
INTEGER(I4B), ALLOCATABLE :: nptrs0(:)

tnodes = obj%GetTotalNodes()
ALLOCATE (nptrs0(tnodes))
CALL obj%GetNptrsInBox_(box=box, nptrs=nptrs0, tnodes=tnodes, &
                        isStrict=isStrict)

ALLOCATE (nptrs(tnodes))
DO CONCURRENT(ii=1:tnodes)
  nptrs(ii) = nptrs0(ii)
END DO
DEALLOCATE (nptrs0)
END PROCEDURE obj_GetNptrsInBox

!----------------------------------------------------------------------------
!                                                           GetNptrsInBox_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrsInBox_
! nptrs = box.Nptrs.obj%nodeCoord
REAL(DFP) :: qv(3), r2
INTEGER(I4B) :: ii, jj, kk, nsd, tsize
! CHARACTER(*), PARAMETER :: myName = "obj_GetNptrsInBox_()"
LOGICAL(LGT) :: isok, abool

isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
IF (.NOT. isok) THEN
  CALL obj%InitiateKdtree()
END IF

qv = Center(box)
r2 = GetRadiusSqr(box)
nsd = obj%GetNSD()

CALL Kdtree2_r_nearest(tp=obj%kdtree, qv=qv(1:nsd), r2=r2, &
               nfound=tnodes, nalloc=SIZE(obj%kdresult), results=obj%kdresult)

isok = Input(default=.TRUE., option=isStrict)

IF (.NOT. isok) THEN
  !$OMP PARALLEL DO PRIVATE(ii)
  DO ii = 1, tnodes
    nptrs(ii) = obj%GetGlobalNodeNumber(obj%kdresult(ii)%idx)
  END DO
  !$OMP END PARALLEL DO
  RETURN
END IF

jj = 0
qv = 0.0_DFP
DO ii = 1, tnodes

  kk = obj%kdresult(ii)%idx
  CALL NodeData_GetNodeCoord(obj=obj%nodeData(kk)%ptr, ans=qv, tsize=tsize)
  abool = IsInside(box, qv(1:nsd))
  IF (abool) THEN
    jj = jj + 1
    nptrs(jj) = obj%GetGlobalNodeNumber(kk)
  END IF

END DO

tnodes = jj

END PROCEDURE obj_GetNptrsInBox_

!----------------------------------------------------------------------------
!                                                          GetInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
INTEGER(I4B) :: ii, dummy, nodeType
dummy = obj%GetTotalInternalNodes()
ALLOCATE (ans(dummy))
dummy = 0
DO ii = 1, obj%tNodes
  nodeType = NodeData_GetNodeType(obj%nodeData(ii)%ptr)
  IF (nodeType .EQ. INTERNAL_NODE) THEN
    dummy = dummy + 1
    ans(dummy) = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
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

INTEGER(I4B) :: ii, dummy, nodeType

dummy = obj%GetTotalInternalNodes()

#ifdef DEBUG_VER
problem = dummy .GT. SIZE(nptrs)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: size of nptrs is not enough '// &
                    'it should be ateast '//ToString(dummy))
  RETURN
END IF
#endif

dummy = 0
DO ii = 1, obj%tNodes
  nodeType = NodeData_GetNodeType(obj%nodeData(ii)%ptr)
  IF (nodeType .EQ. INTERNAL_NODE) THEN
    dummy = dummy + 1
    nptrs(dummy) = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
  END IF
END DO
END PROCEDURE obj_GetInternalNptrs_

!----------------------------------------------------------------------------
!                                                          GetBoundaryNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryNptrs
INTEGER(I4B) :: ii, dummy, nodeType

dummy = obj%GetTotalBoundaryNodes()
CALL Reallocate(ans, dummy)
dummy = 0
DO ii = 1, obj%tNodes

  nodeType = NodeData_GetNodeType(obj%nodeData(ii)%ptr)

  IF (nodeType .EQ. BOUNDARY_NODE) THEN
    dummy = dummy + 1
    ans(dummy) = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
    ! ans(dummy) = obj%nodeData(ii)%ptr%globalNodeNum

  END IF
END DO
END PROCEDURE obj_GetBoundaryNptrs

!----------------------------------------------------------------------------
!                                                            isBoundaryNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryNode
INTEGER(I4B) :: localnode, nodeType
localnode = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
nodeType = NodeData_GetNodeType(obj%nodeData(localnode)%ptr)
ans = nodeType .NE. INTERNAL_NODE
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
    ! jj = obj%nodeData(ii)%ptr%globalNodeNum
    jj = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
    mask(jj) = .TRUE.
  END DO

  RETURN

END IF

tsize = SIZE(obj%nodeData)
DO CONCURRENT(ii=1:tsize)
  ! jj = obj%nodeData(ii)%ptr%globalNodeNum
  jj = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
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
LOGICAL(LGT) :: islocal0

islocal0 = Input(default=.FALSE., option=islocal)

IF (islocal0) THEN
  ans = (globalElement .GT. 0_I4B) .AND. (globalElement .LE. obj%tElements)
  RETURN
END IF

ans = (globalElement .LE. obj%maxElemNum) &
      .AND. (globalElement .GE. obj%minElemNum)

IF (ans) ans = obj%local_elemNumber(globalElement) .NE. 0

END PROCEDURE obj_isElementPresent

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryElement
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = Elemdata_IsBoundaryElement(obj%elementData(iel)%ptr)
END PROCEDURE obj_isBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainBoundaryElement
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = obj%elementData(iel)%ptr%elementType .EQ. DOMAIN_BOUNDARY_ELEMENT
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
INTEGER(I4B) :: ii, nodeType
ans = 0
DO ii = 1, obj%tNodes
  nodeType = NodeData_GetNodeType(obj%nodeData(ii)%ptr)
  IF (nodeType .EQ. INTERNAL_NODE) THEN
    ans = ans + 1
  END IF
END DO
END PROCEDURE obj_GetTotalInternalNodes

!----------------------------------------------------------------------------
!                                                              GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes1
ans = obj%tNodes
END PROCEDURE obj_GetTotalNodes1

!----------------------------------------------------------------------------
!                                                           GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes2
INTEGER(I4B) :: ii, jj, kk, ll, fake_tsize, found_meshid
LOGICAL(LGT) :: isok
LOGICAL(LGT), ALLOCATABLE :: foundNodes(:)
INTEGER(I4B), POINTER :: intptr(:)

fake_tsize = obj%GetTotalElements()
ans = 0

ii = obj%GetMaxNodeNumber()
ALLOCATE (foundNodes(ii))
foundNodes = .FALSE.

DO ii = 1, fake_tsize
  isok = obj%IsElementActive(globalElement=ii, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  found_meshid = Elemdata_meshid(obj%elementData(ii)%ptr)

  isok = found_meshid .EQ. meshid
  IF (.NOT. isok) CYCLE

  intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(ii)%ptr)
  jj = Elemdata_GetTotalGlobalNodes(obj%elementData(ii)%ptr)

  DO kk = 1, jj
    ll = intptr(kk)

    isok = foundNodes(ll)
    IF (isok) CYCLE

    foundNodes(ll) = .TRUE.
    ans = ans + 1

  END DO

END DO

DEALLOCATE (foundNodes)
intptr => NULL()

END PROCEDURE obj_GetTotalNodes2

!----------------------------------------------------------------------------
!                                                       GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes3
INTEGER(I4B) :: ii, jj, kk, ll, ierr, tsize, iel
TYPE(HashTable_) :: tbl
INTEGER(I4B), POINTER :: intptr(:)
LOGICAL(LGT) :: isok

ans = 0
tsize = SIZE(globalElement)

DO ii = 1, tsize
  iel = obj%GetLocalElemNumber(globalElement(ii), islocal=islocal)

  isok = obj%IsElementActive(globalElement=iel, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(iel)%ptr)
  jj = Elemdata_GetTotalGlobalNodes(obj%elementData(iel)%ptr)

  DO kk = 1, jj
    ll = intptr(kk)

    !! check if the key is present
    CALL tbl%check_key(key=Hashkey(ll), stat=ierr)
    isok = ierr .EQ. 0_I4B
    IF (isok) CYCLE

    !! if not present then set the key
    CALL tbl%Set(key=Hashkey(ll), VALUE=.TRUE.)
    ans = ans + 1

  END DO

END DO

intptr => NULL()

END PROCEDURE obj_GetTotalNodes3

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
INTEGER(I4B) :: ii, tcells
ans = 0
tcells = obj%GetTotalElements()
DO ii = 1, tcells
  IF (obj%elementData(ii)%ptr%elementType == BOUNDARY_ELEMENT) THEN
    ans = ans + 1
  END IF
END DO
END PROCEDURE obj_GetTotalBoundaryElements

!----------------------------------------------------------------------------
!                                                            GetBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingBox1
! CHARACTER(*), PARAMETER :: myName = "obj_GetBoundingBox1()"
REAL(DFP) :: lim(6), x(3)
INTEGER(I4B) :: nsd, tnodes, ii, tsize

!> main

nsd = obj%GetNSD()
tnodes = obj%GetTotalNodes()

lim(1:nsd * 2:2) = MinDFP
lim(2:nsd * 2:2) = MaxDFP

DO ii = 1, tnodes
  CALL NodeData_GetNodeCoord(obj%nodeData(ii)%ptr, ans=x, tsize=tsize)
  lim(1) = MIN(lim(1), x(1))
  lim(2) = MAX(lim(2), x(1))

  lim(3) = MIN(lim(3), x(2))
  lim(4) = MAX(lim(4), x(2))

  lim(5) = MIN(lim(5), x(3))
  lim(6) = MAX(lim(6), x(3))
END DO

CALL BoundingBox_Initiate(obj=ans, nsd=3_I4B, lim=lim)

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

CALL BoundingBox_Initiate(obj=ans, nsd=nsd, lim=lim)
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

MODULE PROCEDURE obj_GetConnectivity1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity1_()"
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: iel

#ifdef DEBUG_VER
problem = .NOT. obj%isElementPresent(globalElement, islocal=islocal)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: problem in getting localElement number'// &
                    ' from globalElement = '//ToString(globalElement))
  RETURN
END IF
#endif

iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)

CALL Elemdata_GetConnectivity(obj=obj%elementData(iel)%ptr, con=ans, &
                              tsize=tsize, opt=opt)

END PROCEDURE obj_GetConnectivity1_

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity2_()"
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: iel

#ifdef DEBUG_VER
problem = .NOT. obj%isElementPresent(globalElement=globalElement, islocal=islocal)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: problem in getting localElement number'// &
                    ' from globalElement = '//ToString(globalElement))
  RETURN
END IF
#endif

iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

CALL Elemdata_GetConnectivity2(obj=obj%elementData(iel)%ptr, &
         cellCon=cellCon, faceCon=faceCon, edgeCon=edgeCon, nodeCon=nodeCon, &
                    tCellCon=tCellCon, tFaceCon=tFaceCon, tEdgeCon=tEdgeCon, &
                               tNodeCon=tNodeCon)

END PROCEDURE obj_GetConnectivity2_

!----------------------------------------------------------------------------
!                                                            GetOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrientation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetOrientation()"
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: iel

#ifdef DEBUG_VER
problem = .NOT. obj%isElementPresent(globalElement=globalElement, islocal=islocal)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: problem in getting localElement number'// &
                    ' from globalElement = '//ToString(globalElement))
  RETURN
END IF
#endif

iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

CALL Elemdata_GetOrientation(obj=obj%elementData(iel)%ptr, &
                             cellOrient=cellOrient, faceOrient=faceOrient, &
                             edgeOrient=edgeOrient, tCellOrient=tCellOrient, &
                             tFaceOrient=tFaceOrient, tEdgeOrient=tEdgeOrient)

END PROCEDURE obj_GetOrientation

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
  nn = SIZE(obj%elementData(iel)%ptr%globalNodes)
  DO ii = 1, nn
    VALUE(ii, iel) = obj%elementData(iel)%ptr%globalNodes(ii)
  END DO
END DO

END PROCEDURE obj_GetNodeConnectivity

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
CALL obj%GetLocalNodeNumber_(globalNode=globalNode, ans=ans, islocal=islocal)
END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                       GetLocalNodeNumber_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1_
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: islocal0

islocal0 = Input(option=islocal, default=.FALSE.)
tsize = SIZE(globalNode)

IF (islocal0) THEN
  DO CONCURRENT(ii=1:tsize)
    ans(ii) = globalNode(ii)
  END DO
  RETURN
END IF

DO CONCURRENT(ii=1:tsize)
  ans(ii) = obj%local_nptrs(globalNode(ii))
END DO

END PROCEDURE obj_GetLocalNodeNumber1_

!----------------------------------------------------------------------------
!                                                        GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
LOGICAL(LGT) :: islocal0

islocal0 = Input(option=islocal, default=.FALSE.)

IF (islocal0) THEN
  ans = globalNode
  RETURN
END IF

ans = obj%local_nptrs(globalNode)

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
                    '[INTERNAL ERROR] :: localNode is out of bound.')
END IF
#endif

! ans = obj%nodeData(localNode)%ptr%globalNodeNum
ans = NodeData_GetGlobalNodeNum(obj%nodeData(localNode)%ptr)
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

ans = obj%elementData(localElement)%ptr%globalElemNum
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

problem = (globalElement .LT. obj%minElemNum) &
          .OR. (globalElement .GT. obj%maxElemNum)

IF (problem) THEN
  ans = 0
  CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[INTERNAL ERROR] :: globalElement '//ToString(globalElement)// &
                    ' not present.')
  RETURN
END IF

#endif

ans = obj%local_elemNumber(globalElement)

END PROCEDURE obj_GetLocalElemNumber2

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
INTEGER(I4B) :: ii, tsize

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
! ans = obj%nodeData(ii)%ptr%globalElements

tsize = NodeData_GetTotalGlobalElements(obj%nodeData(ii)%ptr)
ALLOCATE (ans(tsize))
CALL NodeData_GetGlobalElements(obj=obj%nodeData(ii)%ptr, ans=ans, tsize=tsize)

END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
INTEGER(I4B) :: ii, jj, n, lnode(SIZE(globalNode)), &
                nn(SIZE(globalNode) + 1)

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

nn(1) = 1
n = SIZE(globalNode)

DO ii = 1, n
  lnode(ii) = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
  ! nn(ii + 1) = nn(ii) + SIZE(obj%nodeData(lnode(ii))%ptr%globalElements)
  nn(ii + 1) = nn(ii) + &
               NodeData_GetTotalGlobalElements(obj%nodeData(lnode(ii))%ptr)
END DO

CALL Reallocate(ans, nn(n + 1) - 1)

DO ii = 1, n
  CALL NodeData_GetGlobalElements(obj=obj%nodeData(lnode(ii))%ptr, &
                                  ans=ans(nn(ii):), tsize=jj)
END DO

CALL RemoveDuplicates(ans)
END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1_
INTEGER(I4B) :: ii
LOGICAL(LGT) :: problem

tsize = 0
problem = .NOT. obj%isNodePresent(globalNode, islocal=islocal)
IF (problem) RETURN

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

ii = obj%GetLocalNodeNumber(globalNode, islocal=islocal)

CALL NodeData_GetGlobalElements(obj=obj%nodeData(ii)%ptr, &
                                ans=ans, tsize=tsize)
END PROCEDURE obj_GetNodeToElements1_

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2_
INTEGER(I4B) :: ii, n, lnode, a, b

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

a = 1
n = SIZE(globalNode)

DO ii = 1, n
  lnode = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)

  CALL NodeData_GetGlobalElements(obj=obj%nodeData(lnode)%ptr, &
                                  ans=ans(a:), tsize=b)

  a = a + b

END DO

tsize = a - 1

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
                    '[INTERNAL ERROR] :: globalNode is out of bound.')
  RETURN
END IF
#endif

i = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)

abool = obj%isExtraNodeToNodesInitiated .AND. IncludeSelf

IF (abool) THEN
  j = NodeData_GetTotalGlobalNodes(obj%nodeData(i)%ptr) + &
      NodeData_GetTotalExtraGlobalNodes(obj%nodeData(i)%ptr)
  ALLOCATE (ans(j + 1))
  ans(1) = obj%GetGlobalNodeNumber(i)
  CALL NodeData_GetGlobalNodes2(obj%nodeData(i)%ptr, ans(2:), j)
  RETURN
END IF

abool = obj%isExtraNodeToNodesInitiated .AND. (.NOT. IncludeSelf)
IF (abool) THEN
  j = NodeData_GetTotalGlobalNodes(obj%nodeData(i)%ptr) + &
      NodeData_GetTotalExtraGlobalNodes(obj%nodeData(i)%ptr)
  ALLOCATE (ans(j))
  CALL NodeData_GetGlobalNodes2(obj%nodeData(i)%ptr, ans(1:), j)
  RETURN
END IF

IF (IncludeSelf) THEN
  j = NodeData_GetTotalGlobalNodes(obj%nodeData(i)%ptr)
  ALLOCATE (ans(j + 1))
  ans(1) = obj%GetGlobalNodeNumber(i)
  CALL NodeData_GetGlobalNodes(obj%nodeData(i)%ptr, ans(2:), j)
  RETURN
END IF

j = NodeData_GetTotalGlobalNodes(obj%nodeData(i)%ptr)
ALLOCATE (ans(j))
CALL NodeData_GetGlobalNodes(obj%nodeData(i)%ptr, ans(1:), j)
RETURN

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
  ! tsize = tsize + SIZE(obj%nodeData(lnode)%ptr%globalNodes)
  tsize = tsize + NodeData_GetTotalGlobalNodes(obj%nodeData(lnode)%ptr)
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

INTEGER(I4B) :: i, a, aint

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

a = 0
IF (IncludeSelf) THEN

  a = 1
  tsize = 1

  problem = SIZE(ans) .LT. 1
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: size of ans is not enough')
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

! tsize = a + SIZE(obj%nodeData(i)%ptr%globalNodes)
tsize = a + NodeData_GetTotalGlobalNodes(obj%nodeData(i)%ptr)

#ifdef DEBUG_VER

! problem = size ans .lt. 1
problem = SIZE(ans) .LT. tsize
! call raiseError if problem is true
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: size of ans is not enough')
  RETURN
END IF

! ans(a + 1:tsize) = obj%nodedata(i)%ptr%globalNodes
CALL NodeData_GetGlobalNodes(obj%nodeData(i)%ptr, ans(a + 1:), aint)

#else

! ans(a + 1:tsize) = obj%nodedata(i)%ptr%globalNodes
CALL NodeData_GetGlobalNodes(obj%nodeData(i)%ptr, ans(a + 1:), aint)

#endif

! exatranodes

abool = obj%isExtraNodeToNodesInitiated

#ifdef DEBUG_VER

IF (abool) THEN

  a = tsize
  ! tsize = tsize + SIZE(obj%nodeData(i)%ptr%extraglobalNodes)
  tsize = tsize + NodeData_GetTotalExtraGlobalNodes(obj%nodeData(i)%ptr)

  problem = SIZE(ans) .LT. tsize
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: size of ans is not enough')
    RETURN
  END IF

  ! ans(a + 1:tsize) = obj%nodedata(i)%ptr%extraglobalNodes
  CALL NodeData_GetExtraGlobalNodes(obj%nodeData(i)%ptr, ans(a + 1:), aint)

END IF

#else

IF (abool) THEN

  a = tsize
  ! tsize = tsize + SIZE(obj%nodeData(i)%ptr%extraglobalNodes)
  tsize = tsize + NodeData_GetTotalExtraGlobalNodes(obj%nodeData(i)%ptr)
  ! ans(a + 1:tsize) = obj%nodedata(i)%ptr%extraglobalNodes
  CALL NodeData_GetExtraGlobalNodes(obj%nodeData(i)%ptr, ans(a + 1:), aint)

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

INTEGER(I4B) :: i, jj, b

tsize = 0

DO jj = 1, SIZE(globalNode)

 problem = .NOT. obj%isNodePresent(globalNode=globalNode(jj), islocal=islocal)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: globalNode node present.')
    RETURN
  END IF

  i = obj%GetLocalNodeNumber(globalNode=globalNode(jj), islocal=islocal)

  IF (IncludeSelf) THEN

    ans(tsize + 1) = obj%GetglobalNodeNumber(i)
    tsize = tsize + 1

  END IF

  CALL NodeData_GetGlobalNodes(obj%nodeData(i)%ptr, ans(tsize + 1:), b)
  tsize = tsize + b

  abool = obj%isExtraNodeToNodesInitiated
  IF (abool) THEN

    CALL NodeData_GetExtraGlobalNodes(obj%nodeData(i)%ptr, &
                                      ans(tsize + 1:), b)
    tsize = tsize + b

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
CALL Elemdata_GetElementToElements(obj=obj%elementData(iel)%ptr, ans=ans, &
                                   tsize=tsize)
END PROCEDURE obj_GetElementToElements1_

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements2_
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
CALL Elemdata_GetElementToElements(obj=obj%elementData(iel)%ptr, ans=ans, &
                                   nrow=nrow, ncol=ncol)
END PROCEDURE obj_GetElementToElements2_

!----------------------------------------------------------------------------
!                                                     GetBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryElementData
INTEGER(I4B) :: iel, tsize, ii
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
tsize = SIZE(obj%elementData(iel)%ptr%boundaryData)
CALL Reallocate(ans, tsize)
DO ii = 1, tsize
  ans(ii) = obj%elementData(iel)%ptr%boundaryData(ii)
END DO
END PROCEDURE obj_GetBoundaryElementData

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder1
CHARACTER(*), PARAMETER :: myName = "obj_GetOrder()"
ans = 0
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is not available')
END PROCEDURE obj_GetOrder1

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder2
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
ans = Elemdata_Order(obj=obj%elementData(iel)%ptr)
END PROCEDURE obj_GetOrder2

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
ans = obj%elementData(iel)%ptr%material(medium)
END PROCEDURE obj_GetMaterial1

!----------------------------------------------------------------------------
!                                                         GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMaterial1
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
ans = 0 ! default value
IF (ALLOCATED(obj%elementData(iel)%ptr%material)) THEN
  ans = SIZE(obj%elementData(iel)%ptr%material)
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
!                                                                  FindFace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_FindFace
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

IF (onlyBoundaryElement) THEN
  CALL obj%InitiateBoundaryData()
END IF

CALL Elemdata_FindFace(obj=obj%elementData(iel)%ptr, faceCon=faceCon, &
                       isFace=isFace, localFaceNumber=localFaceNumber, &
                       onlyBoundaryElement=onlyBoundaryElement)
END PROCEDURE obj_FindFace

!----------------------------------------------------------------------------
!                                                                 FindEdge
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_FindEdge
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)

IF (onlyBoundaryElement) THEN
  CALL obj%InitiateBoundaryData()
END IF

CALL Elemdata_FindEdge(obj=obj%elementData(iel)%ptr, edgeCon=edgeCon, &
                       isEdge=isEdge, localEdgeNumber=localEdgeNumber, &
                       onlyBoundaryElement=onlyBoundaryElement)
END PROCEDURE obj_FindEdge

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
!                                                       GetGlobalFaceNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalFaceNumber
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
ans = Elemdata_GetGlobalFaceNumber(obj=obj%elementData(iel)%ptr, &
                                   localFaceNumber=localFaceNumber)
END PROCEDURE obj_GetGlobalFaceNumber

!----------------------------------------------------------------------------
!                                                       GetGlobalEdgeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalEdgeNumber
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
ans = Elemdata_GetGlobalEdgeNumber(obj=obj%elementData(iel)%ptr, &
                                   localEdgeNumber=localEdgeNumber)
END PROCEDURE obj_GetGlobalEdgeNumber

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
    ans(1) = obj%elementData(iel)%ptr%globalNodes(1)
  ELSE
    ans(1) = obj%elementData(iel)%ptr%globalNodes(2)
  END IF

CASE (2_I4B)

  elemType = obj%elementData(iel)%ptr%name
  order = ElementOrder(elemType)

  CALL Reallocate(ans, order + 1)
  CALL GetEdgeConnectivity(elemType=elemType, con=con, order=order, &
    & opt=1_I4B)

  DO ii = 1, order + 1
    ans(ii) = obj%elementData(iel)%ptr%globalNodes(con(ii, iface))
  END DO

CASE (3_I4B)

  elemType = obj%elementData(iel)%ptr%name
  temp4 = TotalEntities(elemType)
  order = ElementOrder(elemType)

  CALL RefElemGetGeoParam(elemType=elemType,  &
    & faceCon=con,  &
    & faceOpt=1_I4B, &
    & order=order, &
    & tFaceNodes=tFaceNodes)

  CALL Reallocate(ans, tFaceNodes(iface))

  DO ii = 1, tFaceNodes(iface)
    ans(ii) = obj%elementData(iel)%ptr%globalNodes(con(ii, iface))
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
ans = obj%elementData(iel)%ptr%isActive
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
ans = Elemdata_GetTotalEntities(obj%elementData(iel)%ptr)
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
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord2()"
#endif

INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncol = obj%GetTotalNodes()

!$OMP PARALLEL DO PRIVATE(ii, tsize)
DO ii = 1, ncol
  CALL NodeData_GetNodeCoord(obj=obj%nodeData(ii)%ptr, &
                             ans=nodeCoord(:, ii), tsize=tsize)
END DO
!$OMP END PARALLEL DO

nrow = tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeCoord2

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord3()"
#endif

INTEGER(I4B), POINTER :: globalNode(:)
INTEGER(I4B) :: iel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel = obj%GetLocalElemNumber(globalelement=globalelement, islocal=islocal)
globalNode => Elemdata_GetGlobalNodesPointer(obj%elementData(iel)%ptr)

CALL obj%GetNodeCoord(nodeCoord=nodeCoord, globalNode=globalNode, &
                      islocal=.FALSE., nrow=nrow, ncol=ncol)

globalNode => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeCoord3

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord4()"
#endif

INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = 0
ncol = SIZE(globalNode)

DO ii = 1, SIZE(globalNode)
  jj = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
  CALL NodeData_GetNodeCoord(obj=obj%nodeData(jj)%ptr, &
                             ans=nodeCoord(:, ii), tsize=nrow)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeCoord4

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord5()"
#endif

INTEGER(I4B) :: jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

jj = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
CALL NodeData_GetNodeCoord(obj=obj%nodeData(jj)%ptr, &
                           ans=nodeCoord, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeCoord5

!----------------------------------------------------------------------------
!                                                             GetNearestNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNearestNode1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNearestNode1()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
IF (.NOT. isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                   'AbstractMesh_::obj%kdtree is not initiating, initing it.')
#endif

  CALL obj%InitiateKdtree()

END IF

CALL Kdtree2_n_nearest(tp=obj%kdtree, qv=qv(1:obj%nsd), nn=1, &
                       results=obj%kdresult)

! INFO: This is a local node number
globalNode = obj%kdresult(1)%idx

CALL NodeData_GetNodeCoord(obj=obj%nodeData(globalNode)%ptr, &
                           ans=x, tsize=tsize)

globalNode = obj%GetGlobalNodeNumber(globalNode)
END PROCEDURE obj_GetNearestNode1

!----------------------------------------------------------------------------
!                                                             GetNearestNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNearestNode2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNearestNode2()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize

isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
IF (.NOT. isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                   'AbstractMesh_::obj%kdtree is not initiating, initing it.')
#endif

  CALL obj%InitiateKdtree()
END IF

CALL Kdtree2_n_nearest(tp=obj%kdtree, qv=qv(1:obj%nsd), nn=nn, &
                       results=obj%kdresult)

DO ii = 1, nn
  globalNode(ii) = obj%kdresult(ii)%idx

  CALL NodeData_GetNodeCoord(obj=obj%nodeData(globalNode(ii))%ptr, &
                             ans=x(:, ii), tsize=tsize)

  globalNode(ii) = obj%GetGlobalNodeNumber(localnode=globalNode(ii))
END DO

END PROCEDURE obj_GetNearestNode2

!----------------------------------------------------------------------------
!                                                       GetMaxNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNodeToElements
INTEGER(I4B) :: ii, tsize, tnodes
ans = 0
tnodes = obj%GetTotalNodes()

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

DO ii = 1, tnodes
  tsize = NodeData_GetTotalGlobalElements(obj%nodeData(ii)%ptr)
  ans = MAX(ans, tsize)
END DO
END PROCEDURE obj_GetMaxNodeToElements

!----------------------------------------------------------------------------
!                                                       GetMaxNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNodeToNodes
INTEGER(I4B) :: ii, tsize, tnodes
ans = 0
tnodes = obj%GetTotalNodes()

IF (.NOT. obj%isNodeToNodesInitiated) CALL obj%InitiateNodeToNodes()

DO ii = 1, tnodes
  tsize = NodeData_GetTotalGlobalNodes(obj%nodeData(ii)%ptr)
  ans = MAX(ans, tsize)
END DO
END PROCEDURE obj_GetMaxNodeToNodes

!----------------------------------------------------------------------------
!                                                    GetMaxElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxElementToElements
INTEGER(I4B) :: ii, tsize, tElements
ans = 0
tElements = obj%GetTotalElements()

IF (.NOT. obj%isElementToElementsInitiated) &
  CALL obj%InitiateElementToElements()

DO ii = 1, tElements
  tsize = Elemdata_GetTotalGlobalElements(obj%ElementData(ii)%ptr)
  ans = MAX(ans, tsize)
END DO
END PROCEDURE obj_GetMaxElementToElements

END SUBMODULE GetMethods
