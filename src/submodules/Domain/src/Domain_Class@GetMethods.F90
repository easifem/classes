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
INTEGER(I4B) :: dim, entityNum
!> main
ans = .FALSE.
dimloop: DO dim = 0, obj%nsd
  DO entityNum = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
    ans = meshptr%isElementPresent(globalElement=globalElement)
    IF (ans) EXIT dimloop
  END DO
END DO dimloop
NULLIFY (meshptr)
END PROCEDURE Domain_IsElementPresent

!----------------------------------------------------------------------------
!                                                          getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetConnectivity
CLASS(Mesh_), POINTER :: meshptr

! main
meshptr => NULL()
meshptr => obj%getMeshPointer(globalElement=globalElement)

IF (ASSOCIATED(meshptr)) THEN
  ans = meshptr%getConnectivity(globalElement)
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

meshptr => NULL()
IF (obj%isNodePresent(globalNode=globalNode)) THEN
  dimloop: DO dim = 0, obj%nsd
    DO entityNum = 1, obj%getTotalMesh(dim=dim)
      meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
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
  ivec = obj%getNodeToElements(GlobalNode=GlobalNode(ii))
  CALL append(intvec, ivec)
END DO
ans = intvec
CALL DEALLOCATE (intvec)
IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
END PROCEDURE Domain_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                             getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalNodes
CHARACTER(*), PARAMETER :: myName = "Domain_GetTotalNodes"
CLASS(Mesh_), POINTER :: meshPtr
INTEGER(I4B) :: ii

IF (PRESENT(entityNum) .AND. PRESENT(dim)) THEN
  IF (obj%meshList(dim)%isEmpty()) &
    & CALL e%raiseError(modName//'::'//myName//'-'// &
    & 'The mesh of enitityNum='//TRIM(str(entityNum, .TRUE.))// &
    & " and dimension="//TRIM(str(dim, .TRUE.))//" is empty.")
  IF (entityNum .GT. obj%meshList(dim)%SIZE()) &
    & CALL e%raiseError(modName//'::'//myName//'-'// &
    & 'The the enitityNum='//TRIM(str(entityNum, .TRUE.)) &
    & //" for dimension="//TRIM(str(dim, .TRUE.))// &
    & " is out of bound.")
  meshPtr => NULL()
  meshPtr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
  IF (.NOT. ASSOCIATED(meshPtr)) THEN
    CALL e%raiseError(modName//'::'//myName//'-'// &
    & 'There is some issue in getting pointer to mesh')
  ELSE
    ans = meshPtr%getTotalNodes()
    NULLIFY (meshPtr)
  END IF
ELSEIF (PRESENT(dim) .AND. .NOT. PRESENT(entityNum)) THEN
  ans = 0
  DO ii = 1, obj%getTotalMesh(dim=dim)
    meshPtr => obj%getMeshPointer(dim=dim, entityNum=ii)
    ans = ans + meshPtr%getTotalNodes()
  END DO
  meshPtr => NULL()
ELSE
  ans = obj%tNodes
END IF
END PROCEDURE Domain_GetTotalNodes

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tNodes1
ans = obj%getTotalNodes(dim=opt(1), entityNum=opt(2))
END PROCEDURE Domain_tNodes1

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tNodes2
ans = obj%getTotalNodes(dim=dim)
END PROCEDURE Domain_tNodes2

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tNodes3
ans = obj%getTotalNodes()
END PROCEDURE Domain_tNodes3

!----------------------------------------------------------------------------
!                                                           getTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalElements
CLASS(Mesh_), POINTER :: meshptr
! main
IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
  meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
  ans = meshptr%getTotalElements()
  meshptr => NULL()
ELSE IF (PRESENT(dim) .AND. .NOT. PRESENT(entityNum)) THEN
  ans = obj%tElements(dim)
ELSE
  ans = SUM(obj%tElements)
END IF
END PROCEDURE Domain_GetTotalElements

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tElements1
ans = obj%getTotalElements()
END PROCEDURE Domain_tElements1

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tElements2
ans = obj%getTotalElements(dim=dim)
END PROCEDURE Domain_tElements2

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_tElements3
ans = obj%getTotalElements(dim=opt(1), entityNum=opt(2))
END PROCEDURE Domain_tElements3

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetLocalNodeNumber1
IF (obj%isNodePresent(globalNode)) THEN
  ans = obj%local_nptrs(globalNode)
ELSE
  ans = 0
END IF
END PROCEDURE Domain_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetLocalNodeNumber2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  IF (obj%isNodePresent(globalNode(ii))) THEN
    ans(ii) = obj%local_nptrs(globalNode(ii))
  ELSE
    ans(ii) = 0
  END IF
  ! ans(ii) = Domain_GetLocalNodeNumber1(obj, globalNode(ii))
END DO
END PROCEDURE Domain_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetGlobalNodeNumber1
IF (localNode .LE. obj%tNodes) THEN
  ans = obj%global_nptrs(localNode)
ELSE
  ans = 0
END IF
END PROCEDURE Domain_GetGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetGlobalNodeNumber2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localNode)
  IF (localNode(ii) .LE. obj%tNodes) THEN
    ans(ii) = obj%global_nptrs(localNode(ii))
  ELSE
    ans(ii) = 0
  END IF
END DO
END PROCEDURE Domain_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                              getTotalMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalMesh
CHARACTER(*), PARAMETER :: myName = "Domain_GetTotalMesh"
IF (dim .LT. 0 .OR. dim .GT. 3) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "dim of the mesh should be in [0,1,2,3]")
END IF
IF (obj%meshList(dim)%isEmpty()) THEN
  ans = 0
ELSE
  ans = obj%meshList(dim)%SIZE()
END IF
END PROCEDURE Domain_GetTotalMesh

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetMeshPointer1
CHARACTER(*), PARAMETER :: myName = "Domain_GetMeshPointer"
INTEGER(I4B) :: tsize, imesh
TYPE(MeshPointerIterator_) :: iterator
!> main
iterator%VALUE%ptr => NULL()
tsize = obj%getTotalMesh(dim=dim)
IF (entityNum .GT. tsize) &
  & CALL e%raiseError(modName//"::"//myName//" - "// &
  & "entityNum are out of bound")
iterator = obj%meshList(dim)%Begin()
DO imesh = 2, entityNum
  CALL iterator%Inc()
END DO
ans => iterator%VALUE%ptr
CALL iterator%DEALLOCATE()
END PROCEDURE Domain_GetMeshPointer1

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetMeshPointer2
CHARACTER(*), PARAMETER :: myname = "Domain_GetMeshPointer2"
INTEGER(i4b) :: dim, entityNum
! main
ans => NULL()
dimloop: DO dim = 0, obj%nsd
  DO entityNum = 1, obj%getTotalMesh(dim=dim)
    ans => obj%getMeshPointer(dim=dim, entityNum=entityNum)
    IF (ans%isElementPresent(globalElement=globalElement)) THEN
      EXIT dimloop
    ELSE
      ans => NULL()
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
  DO entityNum = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
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
CHARACTER(*), PARAMETER :: myName = "Domain_GetNodeCoord"
CLASS(Mesh_), POINTER :: meshPtr
INTEGER(I4B) :: np, ii, jj
!> main, check
IF (.NOT. ALLOCATED(obj%nodeCoord)) &
  & CALL e%raiseError(modName//"::"//myName//" - "// &
  & "Nodecoord is not allocated.")
IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
  meshPtr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
  np = meshPtr%getTotalNodes()
  CALL Reallocate(nodeCoord, 3_I4B, np)
  jj = SIZE(nodeCoord, 1)
  DO ii = 1, np
    nodeCoord(1:jj, ii) = obj%nodeCoord(1:jj, &
      & obj%getLocalNodeNumber(globalNode= &
      & meshPtr%getGlobalNodeNumber(localNode=ii)))
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
nodeCoord = nodeCoord(1:nsd, localNode)
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
INTEGER(I4B) :: ii
CLASS(Mesh_), POINTER :: meshptr
TYPE(IntVector_) :: intvec
meshptr => NULL()
DO ii = 1, SIZE(entityNum)
  meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum(ii))
  IF (ASSOCIATED(meshptr)) THEN
    CALL APPEND(intvec, meshptr%getNptrs())
  END IF
END DO
CALL RemoveDuplicates(intvec)
ans = intvec
CALL DEALLOCATE (intvec)
NULLIFY (meshptr)
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
    CALL APPEND(intvec, meshptr%getInternalNptrs())
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
!
CALL Reallocate(ans, obj%getTotalMesh(dim=dim))
!
DO ii = 1, SIZE(ans)
  meshptr => obj%getMeshPointer(dim=dim, entityNum=ii)
  IF (meshptr%getTotalElements() .EQ. 0_I4B) THEN
    ans(ii) = 0
  ELSE
    ans(ii) = meshptr%getOrder()
  END IF
  meshptr => NULL()
END DO
!
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
!                                                          getTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalMaterial1
INTEGER(I4B) :: ii, n
CLASS(Mesh_), POINTER :: meshptr
meshptr => NULL()
n = obj%GetTotalMesh(dim=dim)
ALLOCATE (ans(n))
DO ii = 1, n
  meshptr => obj%GetMeshPointer(dim=dim, entityNum=ii)
  ans(ii) = meshptr%GetTotalMaterial()
END DO
meshptr => NULL()
END PROCEDURE Domain_GetTotalMaterial1

!----------------------------------------------------------------------------
!                                                         GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetTotalMaterial2
CLASS(mesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
ans = meshptr%GetTotalMaterial()
meshptr => NULL()
END PROCEDURE Domain_GetTotalMaterial2

!----------------------------------------------------------------------------
!                                                       Domain_GetElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_GetElemType
CHARACTER(*), PARAMETER :: myName = "Domain_GetTotalMesh"
CLASS(mesh_), POINTER :: meshptr
INTEGER(I4B) :: ii, tMesh, idim, nsd, jj

IF (dim .GT. 3) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "[ARG ERROR] :: Dim of the mesh should be in [0,1,2,3]"//  &
    & " given dim is equal to "//tostring(dim))
  RETURN
END IF

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
