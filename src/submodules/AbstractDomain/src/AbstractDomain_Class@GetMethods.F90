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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             IsNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsNodePresent
ans = obj%mesh%IsNodePresent(globalNode, islocal=islocal)
END PROCEDURE obj_IsNodePresent

!----------------------------------------------------------------------------
!                                                          isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElementPresent
INTEGER(I4B) :: dim0

dim0 = Input(default=obj%nsd, option=dim)
SELECT CASE (dim0)
CASE (3)
  ans = obj%meshVolume%IsElementPresent(globalElement=globalElement,  &
  & islocal=islocal)
CASE (2)
  ans = obj%meshSurface%IsElementPresent(globalElement=globalElement, &
  & islocal=islocal)
CASE (1)
  ans = obj%meshCurve%IsElementPresent(globalElement=globalElement, &
  & islocal=islocal)
CASE (0)
  ans = obj%meshPoint%IsElementPresent(globalElement=globalElement, &
  & islocal=islocal)
END SELECT

END PROCEDURE obj_IsElementPresent

!----------------------------------------------------------------------------
!                                                          GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity()"
#endif

INTEGER(I4B) :: dim0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

dim0 = Input(default=obj%nsd, option=dim)

SELECT CASE (dim0)
CASE (3)
  ans = obj%meshVolume%GetConnectivity(globalElement=globalElement, &
  & islocal=islocal)
CASE (2)
  ans = obj%meshSurface%GetConnectivity(globalElement=globalElement, &
  & islocal=islocal)
CASE (1)
  ans = obj%meshCurve%GetConnectivity(globalElement=globalElement, &
  & islocal=islocal)
CASE (0)
  ans = obj%meshPoint%GetConnectivity(globalElement=globalElement, &
  & islocal=islocal)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                          GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity_()"
#endif

INTEGER(I4B) :: dim0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

dim0 = Input(default=obj%nsd, option=dim)

SELECT CASE (dim0)
CASE (3)
  CALL obj%meshVolume%GetConnectivity_(globalElement=globalElement, &
  & islocal=islocal, ans=ans, tsize=tsize)
CASE (2)
  CALL obj%meshSurface%GetConnectivity_(globalElement=globalElement, &
  & islocal=islocal, ans=ans, tsize=tsize)
CASE (1)
  CALL obj%meshCurve%GetConnectivity_(globalElement=globalElement, &
  & islocal=islocal, ans=ans, tsize=tsize)
CASE (0)
  CALL obj%meshPoint%GetConnectivity_(globalElement=globalElement, &
  & islocal=islocal, ans=ans, tsize=tsize)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_GetConnectivity_

!----------------------------------------------------------------------------
!                                                                    GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNNE()"
#endif

INTEGER(I4B) :: dim0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

dim0 = Input(default=obj%nsd, option=dim)

SELECT CASE (dim0)
CASE (3)
  ans = obj%meshVolume%GetNNE(globalElement=globalElement, &
  & islocal=islocal)
CASE (2)
  ans = obj%meshSurface%GetNNE(globalElement=globalElement, &
  & islocal=islocal)
CASE (1)
  ans = obj%meshCurve%GetNNE(globalElement=globalElement, &
  & islocal=islocal)
CASE (0)
  ans = obj%meshPoint%GetNNE(globalElement=globalElement, &
  & islocal=islocal)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                         GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetNodeToElements(globalNode=globalNode,  &
    & islocal=islocal)
CASE (2)
  ans = obj%meshSurface%GetNodeToElements(globalNode=globalNode,  &
    & islocal=islocal)
CASE (1)
  ans = obj%meshCurve%GetNodeToElements(globalNode=globalNode, &
    & islocal=islocal)
CASE (0)
  ans = obj%meshPoint%GetNodeToElements(globalNode=globalNode, &
    & islocal=islocal)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                         GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetNodeToElements(globalNode=globalNode,  &
    & islocal=islocal)
CASE (2)
  ans = obj%meshSurface%GetNodeToElements(globalNode=globalNode,  &
    & islocal=islocal)
CASE (1)
  ans = obj%meshCurve%GetNodeToElements(globalNode=globalNode, &
    & islocal=islocal)
CASE (0)
  ans = obj%meshPoint%GetNodeToElements(globalNode=globalNode, &
    & islocal=islocal)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                         GetNodeToElements_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements1_()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

SELECT CASE (obj%nsd)
CASE (3)
  CALL obj%meshVolume%GetNodeToElements_(globalNode=globalNode,  &
    & islocal=islocal, ans=ans, tsize=tsize)
CASE (2)
  CALL obj%meshSurface%GetNodeToElements_(globalNode=globalNode,  &
    & islocal=islocal, ans=ans, tsize=tsize)
CASE (1)
  CALL obj%meshCurve%GetNodeToElements_(globalNode=globalNode, &
    & islocal=islocal, ans=ans, tsize=tsize)
CASE (0)
  CALL obj%meshPoint%GetNodeToElements_(globalNode=globalNode, &
    & islocal=islocal, ans=ans, tsize=tsize)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_GetNodeToElements1_

!----------------------------------------------------------------------------
!                                                         GetNodeToElements_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements2_()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

SELECT CASE (obj%nsd)
CASE (3)
  CALL obj%meshVolume%GetNodeToElements_(globalNode=globalNode,  &
    & islocal=islocal, ans=ans, tsize=tsize)
CASE (2)
  CALL obj%meshSurface%GetNodeToElements_(globalNode=globalNode,  &
    & islocal=islocal, ans=ans, tsize=tsize)
CASE (1)
  CALL obj%meshCurve%GetNodeToElements_(globalNode=globalNode, &
    & islocal=islocal, ans=ans, tsize=tsize)
CASE (0)
  CALL obj%meshPoint%GetNodeToElements_(globalNode=globalNode, &
    & islocal=islocal, ans=ans, tsize=tsize)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_GetNodeToElements2_

!----------------------------------------------------------------------------
!                                                             GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes
IF (PRESENT(dim)) THEN
  SELECT CASE (dim)
  CASE (3)
    ans = obj%meshVolume%GetTotalNodes()
  CASE (2)
    ans = obj%meshSurface%GetTotalNodes()
  CASE (1)
    ans = obj%meshCurve%GetTotalNodes()
  CASE (0)
    ans = obj%meshPoint%GetTotalNodes()
  END SELECT

ELSE
  ans = obj%tNodes
END IF
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
!                                                           GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements
IF (PRESENT(dim)) THEN
  SELECT CASE (dim)
  CASE (3)
    ans = obj%meshVolume%GetTotalElements()
  CASE (2)
    ans = obj%meshSurface%GetTotalElements()
  CASE (1)
    ans = obj%meshCurve%GetTotalElements()
  CASE (0)
    ans = obj%meshPoint%GetTotalElements()
  END SELECT

ELSE
  ans = SUM(obj%tElements)
END IF
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
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
ans = globalNode
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber1()"
! #endif
!
! SELECT CASE (obj%nsd)
! CASE (3)
!   ans = obj%meshVolume%GetLocalNodeNumber(globalNode=globalNode,  &
!   & islocal=islocal)
! CASE (2)
!   ans = obj%meshSurface%GetLocalNodeNumber(globalNode=globalNode, &
!   & islocal=islocal)
! CASE (1)
!   ans = obj%meshCurve%GetLocalNodeNumber(globalNode=globalNode, &
!   & islocal=islocal)
! CASE (0)
!   ans = obj%meshPoint%GetLocalNodeNumber(globalNode=globalNode, &
!   & islocal=islocal)
! CASE DEFAULT
!   ans = 0
! #ifdef DEBUG_VER
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[INTERNAL ERROR] :: No case found')
! #endif
! END SELECT

END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
ans = globalNode
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber2()"
! #endif
!
! SELECT CASE (obj%nsd)
! CASE (3)
!   ans = obj%meshVolume%GetLocalNodeNumber(globalNode=globalNode,  &
!   & islocal=islocal)
! CASE (2)
!   ans = obj%meshSurface%GetLocalNodeNumber(globalNode=globalNode, &
!   & islocal=islocal)
! CASE (1)
!   ans = obj%meshCurve%GetLocalNodeNumber(globalNode=globalNode, &
!   & islocal=islocal)
! CASE (0)
!   ans = obj%meshPoint%GetLocalNodeNumber(globalNode=globalNode, &
!   & islocal=islocal)
! CASE DEFAULT
!   ans = 0
! #ifdef DEBUG_VER
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[INTERNAL ERROR] :: No case found')
! #endif
! END SELECT

END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       GetGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber1
ans = localNode
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalNodeNumber1()"
! #endif
!
! SELECT CASE (obj%nsd)
! CASE (3)
!   ans = obj%meshVolume%GetGlobalNodeNumber(localNode=localNode)
! CASE (2)
!   ans = obj%meshSurface%GetGlobalNodeNumber(localNode=localNode)
! CASE (1)
!   ans = obj%meshCurve%GetGlobalNodeNumber(localNode=localNode)
! CASE (0)
!   ans = obj%meshPoint%GetGlobalNodeNumber(localNode=localNode)
! CASE DEFAULT
!   ans = 0
! #ifdef DEBUG_VER
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[INTERNAL ERROR] :: No case found')
! #endif
! END SELECT
END PROCEDURE obj_GetGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber2
ans = localNode
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalNodeNumber2()"
! #endif
!
! SELECT CASE (obj%nsd)
! CASE (3)
!   ans = obj%meshVolume%GetGlobalNodeNumber(localNode=localNode)
! CASE (2)
!   ans = obj%meshSurface%GetGlobalNodeNumber(localNode=localNode)
! CASE (1)
!   ans = obj%meshCurve%GetGlobalNodeNumber(localNode=localNode)
! CASE (0)
!   ans = obj%meshPoint%GetGlobalNodeNumber(localNode=localNode)
! CASE DEFAULT
!   ans = 0
! #ifdef DEBUG_VER
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[INTERNAL ERROR] :: No case found')
! #endif
! END SELECT
END PROCEDURE obj_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                         GetTotalEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntities
#ifdef DEBUG_VER
LOGICAL(LGT) :: problem
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEntities()"

problem = dim .LT. 0 .OR. dim .GT. 3

IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: dim of the mesh should be in [0,1,2,3]")
END IF
#endif

ans = obj%tEntities(dim)
END PROCEDURE obj_GetTotalEntities

!----------------------------------------------------------------------------
!                                                             GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer1
INTEGER(I4B) :: dim0
dim0 = Input(default=obj%nsd, option=dim)

SELECT CASE (dim0)
CASE (0)
  ans => obj%meshPoint
CASE (1)
  ans => obj%meshCurve
CASE (2)
  ans => obj%meshSurface
CASE (3)
  ans => obj%meshVolume
END SELECT

END PROCEDURE obj_GetMeshPointer1

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord()"
LOGICAL(LGT) :: problem

problem = .NOT. ALLOCATED(obj%nodeCoord)
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Nodecoord is not allocated.")
  RETURN
END IF
#endif

nodeCoord(1:obj%nsd, :) = obj%nodeCoord(1:obj%nsd, :)

END PROCEDURE obj_GetNodeCoord

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
!                                                        GetNodeCoordPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoordPointer
ans => obj%nodeCoord
END PROCEDURE obj_GetNodeCoordPointer

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs
SELECT CASE (dim)
CASE (3)
  ans = obj%meshVolume%GetNptrs()
CASE (2)
  ans = obj%meshSurface%GetNptrs()
CASE (1)
  ans = obj%meshCurve%GetNptrs()
CASE (0)
  ans = obj%meshPoint%GetNptrs()
END SELECT
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
SELECT CASE (dim)
CASE (3)
  CALL obj%meshVolume%GetNptrs_(nptrs=nptrs)
CASE (2)
  CALL obj%meshSurface%GetNptrs_(nptrs=nptrs)
CASE (1)
  CALL obj%meshCurve%GetNptrs_(nptrs=nptrs)
CASE (0)
  CALL obj%meshPoint%GetNptrs_(nptrs=nptrs)
END SELECT
END PROCEDURE obj_GetNptrs_

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
    nptrs(ii) = obj%kdresult(ii)%idx
  END DO
  RETURN
END IF

CALL Reallocate(nptrs0, tnodes)
CALL Reallocate(bools, tnodes)
DO CONCURRENT(ii=1:tnodes)
  nptrs0(ii) = obj%kdresult(ii)%idx
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
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
SELECT CASE (dim)
CASE (3)
  ans = obj%meshVolume%GetInternalNptrs()
CASE (2)
  ans = obj%meshSurface%GetInternalNptrs()
CASE (1)
  ans = obj%meshCurve%GetInternalNptrs()
CASE (0)
  ans = obj%meshPoint%GetInternalNptrs()
END SELECT
END PROCEDURE obj_GetInternalNptrs

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
INTEGER(I4B) :: dim0

dim0 = Input(default=obj%nsd, option=dim)
SELECT CASE (dim0)
CASE (0_I4B)
  ans = obj%meshPoint%GetBoundingBox(nodes=obj%nodeCoord)
CASE (1_I4B)
  ans = obj%meshCurve%GetBoundingBox(nodes=obj%nodeCoord)
CASE (2_I4B)
  ans = obj%meshSurface%GetBoundingBox(nodes=obj%nodeCoord)
CASE (3_I4B)
  ans = obj%meshVolume%GetBoundingBox(nodes=obj%nodeCoord)
END SELECT

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
SELECT CASE (dim)
CASE (3)
  ans = obj%meshVolume%GetTotalMaterial(globalElement=globalElement, &
                                        islocal=islocal)
CASE (2)
  ans = obj%meshSurface%GetTotalMaterial(globalElement=globalElement, &
                                         islocal=islocal)
CASE (1)
  ans = obj%meshCurve%GetTotalMaterial(globalElement=globalElement, &
                                       islocal=islocal)
CASE (0)
  ans = obj%meshPoint%GetTotalMaterial(globalElement=globalElement, &
                                       islocal=islocal)
END SELECT
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
IF (PRESENT(meshVolume)) meshVolume => obj%meshVolume
IF (PRESENT(meshSurface)) meshSurface => obj%meshSurface
IF (PRESENT(meshCurve)) meshCurve => obj%meshCurve
IF (PRESENT(meshPoint)) meshPoint => obj%meshPoint
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
