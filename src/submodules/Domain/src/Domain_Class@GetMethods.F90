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

SUBMODULE(Domain_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_IsNodePresent
ans = .TRUE.
IF (globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs) THEN
  ans = .FALSE.
ELSE IF (obj%local_nptrs(globalNode) .EQ. 0) THEN
  ans = .FALSE.
END IF
END PROCEDURE Domain_IsNodePresent

!----------------------------------------------------------------------------
!                                                          isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_IsElementPresent
CLASS(Mesh_), POINTER :: meshptr
INTEGER(I4B) :: dim0, entityNum

ans = .FALSE.

IF (PRESENT(dim)) THEN

  DO entityNum = 1, obj%GetTotalMesh(dim=dim)
    meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
    ans = meshptr%IsElementPresent(globalElement=globalElement)
    IF (ans) EXIT
  END DO

  NULLIFY (meshptr)

ELSE

  dimloop: DO dim0 = 0, obj%nsd
    DO entityNum = 1, obj%GetTotalMesh(dim=dim0)
      meshptr => obj%GetMeshPointer(dim=dim0, entityNum=entityNum)
      ans = meshptr%isElementPresent(globalElement=globalElement)
      IF (ans) EXIT dimloop
    END DO
  END DO dimloop

  NULLIFY (meshptr)

END IF

END PROCEDURE Domain_IsElementPresent

!----------------------------------------------------------------------------
!                                                          getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetConnectivity
CLASS(Mesh_), POINTER :: meshptr

! main
meshptr => NULL()
meshptr => obj%GetMeshPointer(globalElement=globalElement)

IF (ASSOCIATED(meshptr)) THEN
  ans = meshptr%GetConnectivity(globalElement)
ELSE
  ALLOCATE (ans(0))
END IF

NULLIFY (meshptr)
END PROCEDURE Domain_GetConnectivity

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetNodeToElements1
CLASS(Mesh_), POINTER :: meshptr
INTEGER(I4B) :: dim, entityNum
INTEGER(I4B), ALLOCATABLE :: ivec(:)
LOGICAL(LGT) :: isok

meshptr => NULL()
isok = obj%isNodePresent(globalNode=globalNode)

IF (isok) THEN
  dimloop: DO dim = 0, obj%nsd
    DO entityNum = 1, obj%GetTotalMesh(dim=dim)
      meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
      ivec = meshptr%GetNodeToElements(globalNode=globalNode)
      CALL Append(ans, ivec)
    END DO
  END DO dimloop
  meshptr => NULL()
  IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE Domain_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetNodeToElements2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii

DO ii = 1, SIZE(GlobalNode)
  ivec = obj%GetNodeToElements(GlobalNode=GlobalNode(ii))
  CALL Append(intvec, ivec)
END DO
ans = intvec
CALL DEALLOCATE (intvec)
IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
END PROCEDURE Domain_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                             getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalNodes
CHARACTER(*), PARAMETER :: myName = "Domain_GetTotalNodes()"
CLASS(Mesh_), POINTER :: meshPtr
INTEGER(I4B) :: ii
LOGICAL(LGT) :: case1, isEntityNum, isDim, problem, case2

isEntityNum = PRESENT(entityNum)
isDim = PRESENT(dim)

case1 = isEntityNum .AND. isDim

IF (case1) THEN

#ifdef DEBUG_VER
  problem = entityNum .GT. obj%tEntities(dim)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//'-'// &
    & '[INTERNAL ERROR] :: The the enitityNum='//tostring(entityNum) &
    & //" for dimension="//tostring(dim)// &
    & " is out of bound.")
    RETURN
  END IF
#endif

  meshPtr => NULL()
  meshPtr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

#ifdef DEBUG_VER
  problem = .NOT. ASSOCIATED(meshPtr)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//'-'// &
      & '[INTERNAL ERROR] :: There is some issue in getting pointer to mesh')
  END IF
#endif

  ans = meshPtr%GetTotalNodes()
  NULLIFY (meshPtr)

  RETURN

END IF

#ifdef DEBUG_VER

case2 = .NOT. isEntityNum .AND. isDim
IF (case2) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: both entityNum and dim  should be PRESENT.')
  RETURN
END IF

#endif

ans = obj%tNodes
END PROCEDURE Domain_GetTotalNodes

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tNodes1
ans = obj%GetTotalNodes(dim=opt(1), entityNum=opt(2))
END PROCEDURE Domain_tNodes1

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tNodes2
ans = obj%GetTotalNodes()
END PROCEDURE Domain_tNodes2

!----------------------------------------------------------------------------
!                                                           getTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalElements
CLASS(Mesh_), POINTER :: meshptr
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
END PROCEDURE Domain_GetTotalElements

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tElements1
ans = obj%GetTotalElements()
END PROCEDURE Domain_tElements1

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tElements2
ans = obj%GetTotalElements(dim=dim)
END PROCEDURE Domain_tElements2

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tElements3
ans = obj%GetTotalElements(dim=opt(1), entityNum=opt(2))
END PROCEDURE Domain_tElements3

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetLocalNodeNumber1
LOGICAL(LGT) :: isok

ans = 0
isok = obj%isNodePresent(globalNode)
IF (isok) THEN
  ans = obj%local_nptrs(globalNode)
END IF
END PROCEDURE Domain_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetLocalNodeNumber2
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

DO ii = 1, SIZE(globalNode)
  isok = obj%isNodePresent(globalNode(ii))
  ans(ii) = 0
  IF (isok) THEN
    ans(ii) = obj%local_nptrs(globalNode(ii))
  END IF
END DO
END PROCEDURE Domain_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetGlobalNodeNumber1
LOGICAL(LGT) :: isok

ans = 0
isok = localNode .LE. obj%tNodes
IF (isok) THEN
  ans = obj%global_nptrs(localNode)
END IF
END PROCEDURE Domain_GetGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetGlobalNodeNumber2
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

DO ii = 1, SIZE(localNode)
  ans(ii) = 0
  isok = localNode(ii) .LE. obj%tNodes
  IF (isok) THEN
    ans(ii) = obj%global_nptrs(localNode(ii))
  END IF
END DO
END PROCEDURE Domain_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                              getTotalMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalMesh
#ifdef DEBUG_VER
LOGICAL(LGT) :: problem
CHARACTER(*), PARAMETER :: myName = "Domain_GetTotalMesh()"

problem = dim .LT. 0 .OR. dim .GT. 3

IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: dim of the mesh should be in [0,1,2,3]")
END IF
#endif

ans = obj%tEntities(dim)
END PROCEDURE Domain_GetTotalMesh

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetMeshPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Domain_GetMeshPointer1()"
LOGICAL(LGT) :: problem

problem = entityNum .GT. obj%tEntities(dim)
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: entityNum are out of bound")
  RETURN
END IF
#endif

SELECT CASE (dim)
CASE (0)
  ans => obj%meshPoint(entityNum)%ptr
CASE (1)
  ans => obj%meshCurve(entityNum)%ptr
CASE (2)
  ans => obj%meshSurface(entityNum)%ptr
CASE (3)
  ans => obj%meshVolume(entityNum)%ptr
END SELECT

END PROCEDURE Domain_GetMeshPointer1

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetMeshPointer2
! CHARACTER(*), PARAMETER :: myname = "Domain_GetMeshPointer2()"
INTEGER(i4b) :: dim, entityNum
LOGICAL(LGT) :: abool

ans => NULL()

dimloop: DO dim = 0, obj%nsd

  DO entityNum = 1, obj%GetTotalMesh(dim=dim)

    ans => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
    abool = ans%isElementPresent(globalElement=globalElement)

    IF (abool) THEN
      EXIT dimloop
    END IF

  END DO

END DO dimloop

END PROCEDURE Domain_GetMeshPointer2

!----------------------------------------------------------------------------
!                                                           getDimEntityNum
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetDimEntityNum
INTEGER(i4b) :: dim, entityNum
CLASS(Mesh_), POINTER :: meshptr
! main
ans = 0
dimloop: DO dim = 0, obj%nsd
  DO entityNum = 1, obj%GetTotalMesh(dim=dim)
    meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
    IF (meshptr%isElementPresent(globalElement=globalElement)) THEN
      ans = [dim, entityNum]
      EXIT dimloop
    END IF
  END DO
END DO dimloop
END PROCEDURE Domain_GetDimEntityNum

!----------------------------------------------------------------------------
!                                                               getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetNodeCoord
CHARACTER(*), PARAMETER :: myName = "Domain_GetNodeCoord()"
CLASS(Mesh_), POINTER :: meshPtr
INTEGER(I4B) :: np, ii, jj

#ifdef DEBUG_VER

LOGICAL(LGT) :: problem
problem = .NOT. ALLOCATED(obj%nodeCoord)
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Nodecoord is not allocated.")
  RETURN
END IF

#endif

IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
  meshPtr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
  np = meshPtr%GetTotalNodes()
  CALL Reallocate(nodeCoord, 3_I4B, np)
  jj = SIZE(nodeCoord, 1)
  DO ii = 1, np
    nodeCoord(1:jj, ii) = obj%nodeCoord(1:jj, &
      & obj%GetLocalNodeNumber(globalNode= &
      & meshPtr%GetGlobalNodeNumber(localNode=ii)))
  END DO
  NULLIFY (meshPtr)
ELSE
  nodeCoord = obj%nodeCoord
END IF

END PROCEDURE Domain_GetNodeCoord

!----------------------------------------------------------------------------
!                                                       getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetNodeCoord2
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: nsd
localNode = obj%GetLocalNodeNumber(globalNode=globalNode)
nsd = SIZE(nodeCoord, 1)
nodeCoord = obj%nodeCoord(1:nsd, localNode)
END PROCEDURE Domain_GetNodeCoord2

!----------------------------------------------------------------------------
!                                                        getNodeCoordPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetNodeCoordPointer
ans => obj%nodeCoord
END PROCEDURE Domain_GetNodeCoordPointer

!----------------------------------------------------------------------------
!                                            getGlobalToLocalNodeNumPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetGlobalToLocalNodeNumPointer
ans => obj%local_nptrs
END PROCEDURE Domain_GetGlobalToLocalNodeNumPointer

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetNptrs
CHARACTER(*), PARAMETER :: myName = "Domain_GetNptrs()"
INTEGER(I4B) :: ii, tentity, tnodes
CLASS(Mesh_), POINTER :: meshptr
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
LOGICAL(LGT) :: problem

meshptr => NULL()
tentity = SIZE(entityNum)
DO ii = 1, tentity

  meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum(ii))

  problem = .NOT. ASSOCIATED(meshptr)
  IF (problem) CYCLE

  problem = meshptr%isEmpty()
  IF (problem) CYCLE

  nptrs = meshptr%GetNptrs()

  CALL APPEND(intvec, nptrs)

END DO

CALL RemoveDuplicates(intvec)

IF (isAllocated(intvec)) THEN
  ans = intvec
ELSE
  CALL reallocate(ans, 0)
END IF

CALL DEALLOCATE (intvec)

NULLIFY (meshptr)
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)

END PROCEDURE Domain_GetNptrs

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetInternalNptrs
INTEGER(I4B) :: ii
CLASS(Mesh_), POINTER :: meshptr
TYPE(IntVector_) :: intvec

meshptr => NULL()
DO ii = 1, SIZE(entityNum)
  meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum(ii))
  IF (ASSOCIATED(meshptr)) THEN
    CALL APPEND(intvec, meshptr%GetInternalNptrs())
  END IF
END DO
CALL RemoveDuplicates(intvec)
ans = intvec
CALL DEALLOCATE (intvec)
NULLIFY (meshptr)
END PROCEDURE Domain_GetInternalNptrs

!----------------------------------------------------------------------------
!                                                                     getNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetNSD
ans = obj%NSD
END PROCEDURE Domain_GetNSD

!----------------------------------------------------------------------------
!                                                                    getNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetOrder
INTEGER(I4B) :: ii
CLASS(Mesh_), POINTER :: meshptr

CALL Reallocate(ans, obj%GetTotalMesh(dim=dim))

DO ii = 1, SIZE(ans)
  meshptr => obj%GetMeshPointer(dim=dim, entityNum=ii)
  IF (meshptr%GetTotalElements() .EQ. 0_I4B) THEN
    ans(ii) = 0
  ELSE
    ans(ii) = meshptr%GetOrder()
  END IF
  meshptr => NULL()
END DO

END PROCEDURE Domain_GetOrder

!----------------------------------------------------------------------------
!                                                             getBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetBoundingBox
REAL(DFP) :: lim(6)
INTEGER(I4B) :: nsd
!> main
lim = 0.0_DFP
nsd = SIZE(obj%nodeCoord, 1)
lim(1:nsd * 2:2) = MINVAL(obj%nodeCoord(1:nsd, :), dim=2)
lim(2:nsd * 2:2) = MAXVAL(obj%nodeCoord(1:nsd, :), dim=2)
CALL Initiate(obj=ans, nsd=3_I4B, lim=lim)
END PROCEDURE Domain_GetBoundingBox

!----------------------------------------------------------------------------
!                                                     getTotalMeshFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalMeshFacetData
IF (PRESENT(imeshFacetData)) THEN
  IF (ALLOCATED(obj%meshFacetData)) THEN
    IF (obj%meshFacetData(imeshFacetData)%isInitiated()) THEN
      ans = obj%meshFacetData(imeshFacetData)%SIZE()
    ELSE
      ans = 0
    END IF
  ELSE
    ans = 0
  END IF
ELSE
  IF (ALLOCATED(obj%meshFacetData)) THEN
    ans = SIZE(obj%meshFacetData)
  ELSE
    ans = 0
  END IF
END IF
END PROCEDURE Domain_GetTotalMeshFacetData

!----------------------------------------------------------------------------
!                                                         GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalMaterial1
CLASS(mesh_), POINTER :: meshptr

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
ans = meshptr%GetTotalMaterial(globalElement=globalElement, &
                               islocal=islocal)
meshptr => NULL()
END PROCEDURE Domain_GetTotalMaterial1

!----------------------------------------------------------------------------
!                                                       Domain_GetElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetElemType
CHARACTER(*), PARAMETER :: myName = "Domain_GetElemType()"
CLASS(mesh_), POINTER :: meshptr
INTEGER(I4B) :: ii, tMesh, idim, nsd, jj

#ifdef DEBUG_VER

IF (dim .GT. 3) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Dim of the mesh should be in [0,1,2,3]"//  &
    & " given dim is equal to "//tostring(dim))
  RETURN
END IF

#endif

IF (dim .LT. 0) THEN
  tMesh = 0
  nsd = obj%GetNSD()
  jj = 0

  DO idim = 1, nsd
    tMesh = tMesh + obj%GetTotalMesh(dim=idim)
  END DO

  CALL Reallocate(ans, tMesh)

  DO idim = 1, nsd
    DO ii = 1, obj%GetTotalMesh(dim=idim)
      meshptr => obj%GetMeshPointer( &
        & dim=idim, &
        & entityNum=ii)
      jj = jj + 1
      CALL meshptr%GetParam(elemType=ans(jj))
    END DO
  END DO

  meshptr => NULL()
  RETURN
END IF

tMesh = obj%GetTotalMesh(dim=dim)
CALL Reallocate(ans, tMesh)

DO ii = 1, tMesh
  meshptr => obj%GetMeshPointer( &
    & dim=dim, &
    & entityNum=ii)
  CALL meshptr%GetParam(elemType=ans(ii))
END DO

meshptr => NULL()

END PROCEDURE Domain_GetElemType

!----------------------------------------------------------------------------
!                                                         GetUniqueElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetUniqueElemType
ans = obj%GetElemType(dim=dim)
CALL RemoveDuplicates(ans)
END PROCEDURE Domain_GetUniqueElemType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE GetMethods
