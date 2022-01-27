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

SUBMODULE(Domain_Class) getMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_isNodePresent
ans = .TRUE.
IF (globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs) THEN
  ans = .FALSE.
ELSE IF (obj%local_nptrs(globalNode) .EQ. 0) THEN
  ans = .FALSE.
END IF
END PROCEDURE Domain_isNodePresent

!----------------------------------------------------------------------------
!                                                          isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_isElementPresent
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
END PROCEDURE Domain_isElementPresent

!----------------------------------------------------------------------------
!                                                          getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getConnectivity
  CLASS(Mesh_), POINTER :: meshptr
  !!
  !! main
  !!
  meshptr => NULL()
  meshptr => obj%getMeshPointer(globalElement=globalElement)
  !!
  IF (ASSOCIATED(meshptr)) THEN
    ans = meshptr%getConnectivity(globalElement)
  ELSE
    ALLOCATE (ans(0))
  END IF
  !!
  NULLIFY (meshptr)
END PROCEDURE Domain_getConnectivity

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNodeToElements1
CLASS(Mesh_), POINTER :: meshptr
INTEGER(I4B) :: dim, entityNum
INTEGER(I4B), ALLOCATABLE :: ivec(:)
!> main
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
END PROCEDURE Domain_getNodeToElements1

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNodeToElements2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii
!> main
DO ii = 1, SIZE(GlobalNode)
  ivec = obj%getNodeToElements(GlobalNode=GlobalNode(ii))
  CALL append(intvec, ivec)
END DO
ans = intvec
CALL DEALLOCATE (intvec)
IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
END PROCEDURE Domain_getNodeToElements2

!----------------------------------------------------------------------------
!                                                             getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getTotalNodes
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_getTotalNodes"
CLASS(Mesh_), POINTER :: meshPtr
INTEGER(I4B) :: ii
!>
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
END PROCEDURE Domain_getTotalNodes

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

MODULE PROCEDURE Domain_getTotalElements
CLASS(Mesh_), POINTER :: meshptr
!! main
IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
  meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
  ans = meshptr%getTotalElements()
  meshptr => NULL()
ELSE IF (PRESENT(dim) .AND. .NOT. PRESENT(entityNum)) THEN
  ans = obj%tElements(dim)
ELSE
  ans = SUM(obj%tElements)
END IF
END PROCEDURE Domain_getTotalElements

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

MODULE PROCEDURE Domain_getLocalNodeNumber1
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_getLocalNodeNumber"
IF (obj%isNodePresent(globalNode)) THEN
  ans = obj%local_nptrs(globalNode)
ELSE
  ans = 0
END IF
END PROCEDURE Domain_getLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getLocalNodeNumber2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  ans(ii) = Domain_getLocalNodeNumber1(obj, globalNode(ii))
END DO
END PROCEDURE Domain_getLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getGlobalNodeNumber1
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_getGlobalNodeNumber"
IF (localNode .LE. obj%tNodes) THEN
  ans = getIndex(obj%local_nptrs, localNode)
ELSE
  ans = 0
END IF
END PROCEDURE Domain_getGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getGlobalNodeNumber2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localNode)
  ans(ii) = Domain_getGlobalNodeNumber1(obj, localNode(ii))
END DO
END PROCEDURE Domain_getGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                              getTotalMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getTotalMesh
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_getTotalMesh"
IF (dim .LT. 0 .OR. dim .GT. 3) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "dim of the mesh should be in [0,1,2,3]")
END IF
IF (obj%meshList(dim)%isEmpty()) THEN
  ans = 0
ELSE
  ans = obj%meshList(dim)%SIZE()
END IF
END PROCEDURE Domain_getTotalMesh

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getMeshPointer1
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_getMeshPointer"
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
END PROCEDURE Domain_getMeshPointer1

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getMeshPointer2
CHARACTER(len=*), PARAMETER :: myname = "Domain_getMeshPointer2"
INTEGER(i4b) :: dim, entityNum
!> main
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
END PROCEDURE Domain_getMeshPointer2

!----------------------------------------------------------------------------
!                                                           getDimEntityNum
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getDimEntityNum
INTEGER(i4b) :: dim, entityNum
CLASS(Mesh_), POINTER :: meshptr
!! main
ans = 0
!!
dimloop: DO dim = 0, obj%nsd
  DO entityNum = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
    IF (meshptr%isElementPresent(globalElement=globalElement)) THEN
      ans = [dim, entityNum]
      EXIT dimloop
    END IF
  END DO
END DO dimloop
END PROCEDURE Domain_getDimEntityNum

!----------------------------------------------------------------------------
!                                                               getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNodeCoord
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_getNodeCoord"
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
END PROCEDURE Domain_getNodeCoord

!----------------------------------------------------------------------------
!                                                        getNodeCoordPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNodeCoordPointer
  ans => obj%nodeCoord
END PROCEDURE Domain_getNodeCoordPointer

!----------------------------------------------------------------------------
!                                            getGlobalToLocalNodeNumPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getGlobalToLocalNodeNumPointer
ans => obj%local_nptrs
END PROCEDURE Domain_getGlobalToLocalNodeNumPointer

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNptrs
  INTEGER(I4B) :: ii
  CLASS(Mesh_), POINTER :: meshptr
  TYPE(IntVector_) :: intvec
  !!
  !!
  !!
  meshptr => NULL()
  DO ii = 1, SIZE(entityNum)
    meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum(ii))
    IF (ASSOCIATED(meshptr)) THEN
      CALL APPEND(intvec, meshptr%getNptrs())
    END IF
  END DO
  !!
  CALL RemoveDuplicates(intvec)
  !!
  ans = intvec
  !!
  CALL DEALLOCATE (intvec)
  NULLIFY (meshptr)
  !!
END PROCEDURE Domain_getNptrs

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getInternalNptrs
  INTEGER(I4B) :: ii
  CLASS(Mesh_), POINTER :: meshptr
  TYPE(IntVector_) :: intvec
  !!
  !!
  !!
  meshptr => NULL()
  DO ii = 1, SIZE(entityNum)
    meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum(ii))
    IF (ASSOCIATED(meshptr)) THEN
      CALL APPEND(intvec, meshptr%getInternalNptrs())
    END IF
  END DO
  !!
  CALL RemoveDuplicates(intvec)
  !!
  ans = intvec
  !!
  CALL DEALLOCATE (intvec)
  NULLIFY (meshptr)
  !!
END PROCEDURE Domain_getInternalNptrs

!----------------------------------------------------------------------------
!                                                                     getNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNSD
ans = obj%NSD
END PROCEDURE Domain_getNSD

!----------------------------------------------------------------------------
!                                                                     getNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getOrder
  INTEGER( I4B ) :: ii
  CLASS(Mesh_), POINTER :: meshptr
  !!
  CALL Reallocate( ans, obj%getTotalMesh(dim=dim) )
  !!
  DO ii = 1, SIZE( ans )
    meshptr => obj%getMeshPointer(dim=dim, entityNum=ii)
    ans( ii ) = meshptr%getOrder()
    meshptr => NULL()
  END DO
  !!
END PROCEDURE Domain_getOrder

!----------------------------------------------------------------------------
!                                                            getBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getBoundingBox
REAL(DFP) :: lim(6)
INTEGER(I4B) :: nsd
!> main
lim = 0.0_DFP
nsd = SIZE(obj%nodeCoord, 1)
lim(1:nsd * 2:2) = MINVAL(obj%nodeCoord(1:nsd, :), dim=2)
lim(2:nsd * 2:2) = MAXVAL(obj%nodeCoord(1:nsd, :), dim=2)
CALL Initiate(obj=ans, nsd=3_I4B, lim=lim)
END PROCEDURE Domain_getBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE getMethods
