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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(AbstractDomain_Class) GetMethods
USE ReallocateUtility
USE InputUtility
USE BoundingBox_Method, ONLY: Center, GetRadiusSqr, isInside
USE F95_BLAS, ONLY: Copy
USE Kdtree2_Module, ONLY: Kdtree2_r_nearest, Kdtree2_n_nearest
USE Display_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer1
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshPointer1()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
ans => NULL()
END PROCEDURE obj_GetMeshPointer1

!----------------------------------------------------------------------------
!                                                             IsNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsNodePresent
LOGICAL(LGT) :: islocal0
INTEGER(I4B) :: aint

islocal0 = Input(default=.FALSE., option=islocal)

IF (islocal0) THEN
  ans = (globalNode .GT. 0) .AND. (globalNode .LE. obj%tNodes)
  RETURN
END IF

ans = (globalNode .GE. obj%minNptrs) .AND. (globalNode .LE. obj%maxNptrs)

IF (ans) THEN
  aint = obj%GetLocalNodeNumber(globalNode)
  ans = aint .NE. 0_I4B
END IF

END PROCEDURE obj_IsNodePresent

!----------------------------------------------------------------------------
!                                                          isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElementPresent
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
ans = meshptr%IsElementPresent(globalElement=globalElement, islocal=islocal)
meshptr => NULL()
END PROCEDURE obj_IsElementPresent

!----------------------------------------------------------------------------
!                                                          GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum, &
                              globalElement=globalElement, islocal=islocal)
ans = meshptr%GetConnectivity(globalElement=globalElement, islocal=islocal)
meshptr => NULL()
END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                          GetConnectivity_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity_
CLASS(AbstractMesh_), POINTER :: meshptr

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum, &
                              globalElement=globalElement, islocal=islocal)
CALL meshptr%GetConnectivity_(globalElement=globalElement, &
                              islocal=islocal, ans=ans, tsize=tsize)
meshptr => NULL()
END PROCEDURE obj_GetConnectivity_

!----------------------------------------------------------------------------
!                                                                    GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
CLASS(AbstractMesh_), POINTER :: meshptr

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum, &
                              globalElement=globalElement, islocal=islocal)
ans = meshptr%GetNNE(globalElement=globalElement, islocal=islocal)

END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                         GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer()
ans = meshptr%GetNodeToElements(globalNode=globalNode, islocal=islocal)
meshptr => NULL()
END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                         GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer()
ans = meshptr%GetNodeToElements(globalNode=globalNode, islocal=islocal)
meshptr => NULL()
END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                         GetNodeToElements_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1_
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer()
CALL meshptr%GetNodeToElements_(globalNode=globalNode,  &
  & islocal=islocal, ans=ans, tsize=tsize)
meshptr => NULL()
END PROCEDURE obj_GetNodeToElements1_

!----------------------------------------------------------------------------
!                                                         GetNodeToElements_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2_
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer()
CALL meshptr%GetNodeToElements_(globalNode=globalNode,  &
  & islocal=islocal, ans=ans, tsize=tsize)
meshptr => NULL()
END PROCEDURE obj_GetNodeToElements2_

!----------------------------------------------------------------------------
!                                                             GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes
CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: case1, problem

ans = 0
case1 = (.NOT. PRESENT(dim)) .AND. (.NOT. PRESENT(entityNum))
IF (case1) THEN
  ans = obj%tNodes
  RETURN
END IF

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

problem = .NOT. ASSOCIATED(meshptr)
IF (problem) RETURN

ans = meshptr%GetTotalNodes()
NULLIFY (meshptr)

END PROCEDURE obj_GetTotalNodes

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes1
ans = obj%GetTotalNodes(dim=dim)
END PROCEDURE obj_tNodes1

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes2
ans = obj%GetTotalNodes()
END PROCEDURE obj_tNodes2

!----------------------------------------------------------------------------
!                                                           GetMaxNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes3
ans = obj%GetTotalNodes(dim=opt(1), entityNum=opt(2))
END PROCEDURE obj_tNodes3

!----------------------------------------------------------------------------
!                                                           GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements
CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: case1, isDim, isEntityNum

isEntityNum = PRESENT(entityNum)
isDim = PRESENT(dim)

case1 = isDim .AND. isEntityNum
IF (case1) THEN
  meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
  ans = meshptr%GetTotalElements()
  meshptr => NULL()
  RETURN
END IF

case1 = isDim .AND. (.NOT. isEntityNum)
IF (case1) THEN
  ans = obj%tElements(dim)
  RETURN
END IF

ans = SUM(obj%tElements)
END PROCEDURE obj_GetTotalElements

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tElements1
ans = obj%GetTotalElements()
END PROCEDURE obj_tElements1

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tElements2
ans = obj%GetTotalElements(dim=dim)
END PROCEDURE obj_tElements2

!----------------------------------------------------------------------------
!                                                           GetMaxNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tElements3
ans = obj%GetTotalElements(dim=opt(1), entityNum=opt(2))
END PROCEDURE obj_tElements3

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
ans = globalNode
END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
ans = globalNode
END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       GetGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber1
ans = localNode
END PROCEDURE obj_GetGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber2
ans = localNode
END PROCEDURE obj_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                         GetTotalEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntities
ans = obj%tEntities(dim)
END PROCEDURE obj_GetTotalEntities

!----------------------------------------------------------------------------
!                                                           GetDimEntityNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDimEntityNum
INTEGER(I4B) :: dim, entityNum, tsize
CLASS(AbstractMesh_), POINTER :: meshptr

! main
ans = 0
dimloop: DO dim = 0, obj%nsd

  tsize = obj%GetTotalEntities(dim=dim)
  DO entityNum = 1, tsize

    meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

    IF (meshptr%IsElementPresent(globalElement=globalElement, &
                                 islocal=islocal)) THEN
      ans = [dim, entityNum]
      EXIT dimloop
    END IF

  END DO

END DO dimloop

meshptr => NULL()
END PROCEDURE obj_GetDimEntityNum

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord1
INTEGER(I4B) :: ii, tsize, nsd, jj
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord1()"
LOGICAL(LGT) :: isok
CLASS(AbstractMesh_), POINTER :: meshptr

isok = ALLOCATED(obj%nodeCoord)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Nodecoord is not allocated.")
  RETURN
END IF

isok = (.NOT. PRESENT(dim)) .AND. (.NOT. PRESENT(entityNum))
IF (isok) THEN

  tsize = SIZE(obj%nodeCoord, 2)
  nsd = SIZE(obj%nodeCoord, 1)
  CALL Reallocate(nodeCoord, nsd, tsize)

  DO CONCURRENT(ii=1:tsize)
    nodeCoord(1:nsd, ii) = obj%nodeCoord(1:nsd, ii)
  END DO

  RETURN
END IF

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
tsize = meshptr%GetTotalNodes()
CALL Reallocate(nodeCoord, 3_I4B, tsize)
nsd = SIZE(obj%nodeCoord, 1)
DO ii = 1, tsize
  jj = meshptr%GetGlobalNodeNumber(localNode=ii)
  jj = obj%GetLocalNodeNumber(globalNode=jj)
  nodeCoord(1:nsd, ii) = obj%nodeCoord(1:nsd, jj)
END DO

NULLIFY (meshptr)

END PROCEDURE obj_GetNodeCoord1

!----------------------------------------------------------------------------
!                                                       GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord2
INTEGER(I4B) :: localNode(SIZE(globalNode))
localNode = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)
nodeCoord(1:obj%nsd, 1:SIZE(globalNode)) = obj%nodeCoord(1:obj%nsd, localNode)
END PROCEDURE obj_GetNodeCoord2

!----------------------------------------------------------------------------
!                                                         GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord3
INTEGER(I4B) :: localNode
localNode = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)
nodeCoord(1:obj%nsd) = obj%nodeCoord(1:obj%nsd, localNode)
END PROCEDURE obj_GetNodeCoord3

!----------------------------------------------------------------------------
!                                                        GetNodeCoordPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoordPointer
ans => obj%nodeCoord
END PROCEDURE obj_GetNodeCoordPointer

!----------------------------------------------------------------------------
!                                                           GetNearestNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNearestNode1
CHARACTER(*), PARAMETER :: myName = "obj_GetNearestNode1()"
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'AbstractDomain_::obj%kdtree is not initiating, initing it.')
  CALL obj%InitiateKdtree()
END IF

CALL Kdtree2_n_nearest(tp=obj%kdtree, qv=qv(1:obj%nsd), nn=1, &
                       results=obj%kdresult)

globalNode = obj%kdresult(1)%idx
x(1:obj%nsd) = obj%nodeCoord(1:obj%nsd, globalNode)
globalNode = obj%GetGlobalNodeNumber(localnode=globalNode)

END PROCEDURE obj_GetNearestNode1

!----------------------------------------------------------------------------
!                                                           GetNearestNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNearestNode2
CHARACTER(*), PARAMETER :: myName = "obj_GetNearestNode2()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii

isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'AbstractDomain_::obj%kdtree is not initiating, initing it.')
  CALL obj%InitiateKdtree()
END IF

CALL Kdtree2_n_nearest(tp=obj%kdtree, qv=qv(1:obj%nsd), nn=nn, &
                       results=obj%kdresult)

DO ii = 1, nn
  globalNode(ii) = obj%kdresult(ii)%idx
  x(1:obj%nsd, ii) = obj%nodeCoord(1:obj%nsd, globalNode(ii))
  globalNode(ii) = obj%GetGlobalNodeNumber(localnode=globalNode(ii))
END DO

END PROCEDURE obj_GetNearestNode2

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer(dim=dim)
ans = meshptr%GetNptrs()
meshptr => NULL()
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer(dim=dim)
CALL meshptr%GetNptrs_(nptrs=nptrs)
meshptr => NULL()
END PROCEDURE obj_GetNptrs_

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer(dim=dim)
ans = meshptr%GetInternalNptrs()
meshptr => NULL()
END PROCEDURE obj_GetInternalNptrs

!----------------------------------------------------------------------------
!                                                             GetNptrsInBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrsInBox
CHARACTER(*), PARAMETER :: myName = "obj_GetNptrsInBox()"
REAL(DFP) :: qv(3), r2
INTEGER(I4B) :: tnodes, ii, nsd
LOGICAL(LGT) :: isok
LOGICAL(LGT), ALLOCATABLE :: bools(:)
INTEGER(I4B), ALLOCATABLE :: nptrs0(:)

isok = (.NOT. ASSOCIATED(obj%kdtree)) .OR. (.NOT. ALLOCATED(obj%kdresult))
IF (isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'AbstractDomain_::obj%kdtree not initiated, initiating it...')

  CALL obj%InitiateKdtree()
END IF

qv = Center(box)
r2 = GetRadiusSqr(box)
nsd = obj%nsd

CALL Kdtree2_r_nearest(tp=obj%kdtree, qv=qv(1:nsd), r2=r2, &
               nfound=tnodes, nalloc=SIZE(obj%kdresult), results=obj%kdresult)

isok = Input(default=.TRUE., option=isStrict)

IF (.NOT. isok) THEN
  CALL Reallocate(nptrs, tnodes)
  DO CONCURRENT(ii=1:tnodes)
    nptrs(ii) = obj%GetGlobalNodeNumber(localNode=obj%kdresult(ii)%idx)
  END DO
  RETURN
END IF

CALL Reallocate(nptrs0, tnodes)
CALL Reallocate(bools, tnodes)
DO CONCURRENT(ii=1:tnodes)
  nptrs0(ii) = obj%GetGlobalNodeNumber(localNode=obj%kdresult(ii)%idx)
  bools(ii) = isInside(box, obj%nodeCoord(1:nsd, obj%kdresult(ii)%idx))
END DO

nptrs = PACK(nptrs0, bools)

IF (ALLOCATED(bools)) DEALLOCATE (bools)
IF (ALLOCATED(nptrs0)) DEALLOCATE (nptrs0)

END PROCEDURE obj_GetNptrsInBox

!----------------------------------------------------------------------------
!                                                             GetNptrsInBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrsInBox_
! nptrs = box.Nptrs.obj%nodeCoord
REAL(DFP) :: qv(3), r2
INTEGER(I4B) :: ii, jj, kk, nsd
CHARACTER(*), PARAMETER :: myName = "obj_GetNptrsInBox_()"
LOGICAL(LGT) :: isok, abool

isok = (.NOT. ASSOCIATED(obj%kdtree)) .OR. (.NOT. ALLOCATED(obj%kdresult))
IF (isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'AbstractDomain_::obj%kdtree not initiated, initiating it...')

  CALL obj%InitiateKdtree()
END IF

qv = Center(box)
r2 = GetRadiusSqr(box)
nsd = obj%nsd

CALL Kdtree2_r_nearest(tp=obj%kdtree, qv=qv(1:nsd), r2=r2, &
               nfound=tnodes, nalloc=SIZE(obj%kdresult), results=obj%kdresult)

#ifdef DEBUG_VER
isok = SIZE(nptrs) .LT. tnodes
IF (isok) THEN

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: size of nptrs is not enough')
  RETURN

END IF
#endif

isok = Input(default=.TRUE., option=isStrict)

IF (.NOT. isok) THEN
  DO CONCURRENT(ii=1:tnodes)
    nptrs(ii) = obj%kdresult(ii)%idx
  END DO
  RETURN
END IF

jj = 0
DO ii = 1, tnodes

  kk = obj%kdresult(ii)%idx
  abool = isInside(box, obj%nodeCoord(1:nsd, kk))
  IF (abool) THEN
    jj = jj + 1
    nptrs(jj) = kk
  END IF

END DO

tnodes = jj

END PROCEDURE obj_GetNptrsInBox_

!----------------------------------------------------------------------------
!                                                                     GetNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNSD
ans = obj%nsd
END PROCEDURE obj_GetNSD

!----------------------------------------------------------------------------
!                                                             GetBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingBox
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer(dim=dim)
ans = meshptr%GetBoundingBox(nodes=obj%nodeCoord)
meshptr => NULL()
END PROCEDURE obj_GetBoundingBox

!----------------------------------------------------------------------------
!                                                     GetTotalMeshFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMeshFacetData
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalMeshFacetData()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[DEPRECATED] :: We are working on alternative')
ans = 0
END PROCEDURE obj_GetTotalMeshFacetData

!----------------------------------------------------------------------------
!                                                          GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMaterial1
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer(dim=dim)
ans = meshptr%GetTotalMaterial(globalElement=globalElement, islocal=islocal)
meshptr => NULL()
END PROCEDURE obj_GetTotalMaterial1

!----------------------------------------------------------------------------
!                                                         GetUniqueElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetUniqueElemType
CHARACTER(*), PARAMETER :: myName = "obj_GetUniqueElemType()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[DEPRECATED] :: We are working on alternative.')
END PROCEDURE obj_GetUniqueElemType

!----------------------------------------------------------------------------
!                                                                  GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
IF (PRESENT(isInitiated)) isInitiated = obj%isInitiated
IF (PRESENT(engine)) engine = obj%engine%chars()
IF (PRESENT(majorVersion)) majorVersion = obj%majorVersion
IF (PRESENT(minorVersion)) minorVersion = obj%minorVersion
IF (PRESENT(version)) version = obj%version
IF (PRESENT(nsd)) nsd = obj%nsd
IF (PRESENT(maxNptrs)) maxNptrs = obj%maxNptrs
IF (PRESENT(minNptrs)) minNptrs = obj%minNptrs
IF (PRESENT(tNodes)) tNodes = obj%tNodes
IF (PRESENT(isNodeNumberSparse)) isNodeNumberSparse = obj%isNodeNumberSparse
IF (PRESENT(maxElemNum)) maxElemNum = obj%maxElemNum
IF (PRESENT(minElemNum)) minElemNum = obj%minElemNum
IF (PRESENT(isElemNumberSparse)) isElemNumberSparse = obj%isElemNumberSparse
IF (PRESENT(tEntitiesForElements)) tEntitiesForElements = obj%tEntitiesForElements
IF (PRESENT(tEntitiesForNodes)) tEntitiesForNodes = obj%tEntitiesForNodes
IF (PRESENT(tElements)) tElements = obj%tElements
IF (PRESENT(tEntities)) tEntities = obj%tEntities
IF (PRESENT(nodeCoord)) nodeCoord = obj%nodeCoord
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                             GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
CHARACTER(*), PARAMETER :: myName = "obj_GetOrder()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetOrder

!----------------------------------------------------------------------------
!                                                           GetMinElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinElemNumber
ans = obj%minElemNum
END PROCEDURE obj_GetMinElemNumber

!----------------------------------------------------------------------------
!                                                           GetMaxElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxElemNumber
ans = obj%maxElemNum
END PROCEDURE obj_GetMaxElemNumber

!----------------------------------------------------------------------------
!                                                           GetMinNodeNumber
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
!                                                               GetElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemType
CHARACTER(*), PARAMETER :: myName = "obj_GetElemType()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetElemType

!----------------------------------------------------------------------------
!                                                           GetDimEntityNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInit
ans = obj%isInitiated
END PROCEDURE obj_IsInit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
