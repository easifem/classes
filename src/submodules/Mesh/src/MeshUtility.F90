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
!

MODULE MeshUtility
USE BaseType
USE BaseMethod
USE Mesh_Class
IMPLICIT NONE
PRIVATE
PUBLIC :: SetSparsity1
PUBLIC :: SetSparsity2
PUBLIC :: SetSparsity3
PUBLIC :: SetSparsity4

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetSparsity1(obj, mat, localNodeNumber, lbound, &
  & ubound)
  CLASS(Mesh_), INTENT(INOUT) :: obj
  ! [[Mesh_]] class
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  ! [[CSRMatrix_]] object
  INTEGER(I4B), INTENT(IN) :: lbound
  INTEGER(I4B), INTENT(IN) :: ubound
  INTEGER(I4B), INTENT(IN) :: localNodeNumber(lbound:ubound)
  ! Global to local node number map
  !
  !
  INTEGER(I4B) :: i, j, k, tNodes
  INTEGER(I4B), ALLOCATABLE :: n2n(:)

  tNodes = obj%getTotalNodes()

  DO i = 1, tNodes
    j = obj%getGlobalNodeNumber(LocalNode=i)
    k = localNodeNumber(j)
    IF (k .NE. 0) THEN
      n2n = localNodeNumber( &
        & obj%getNodeToNodes(GlobalNode=j, IncludeSelf=.TRUE.))
      CALL SetSparsity(obj=Mat, Row=k, Col=n2n)
    END IF
  END DO
  IF (ALLOCATED(n2n)) DEALLOCATE (n2n)
END SUBROUTINE SetSparsity1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetSparsity2(obj, mat)
  CLASS(Mesh_), INTENT(INOUT) :: obj
  ! Mesh_ class
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  ! CSRMatrix object
  !
  INTEGER(I4B) :: i, j, tNodes
  INTEGER(I4B), ALLOCATABLE :: n2n(:)

  tNodes = obj%getTotalNodes()

  DO i = 1, tNodes
    j = obj%getGlobalNodeNumber(LocalNode=i)
    n2n = obj%getNodeToNodes(GlobalNode=j, IncludeSelf=.TRUE.)
    CALL SetSparsity(obj=Mat, Row=j, Col=n2n)
  END DO

  IF (ALLOCATED(n2n)) DEALLOCATE (n2n)

END SUBROUTINE SetSparsity2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetSparsity3(obj, colMesh, nodeToNode, mat, &
  & ivar, jvar)
  CLASS(Mesh_), INTENT(INOUT) :: obj
  ! [[Mesh_]] class
  CLASS(Mesh_), INTENT(INOUT) :: colMesh
  ! [[Mesh_]] class
  INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
  ! Node to node connectivity between obj and colMesh
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  ! [[CSRMatrix_]] object
  INTEGER(I4B), INTENT(IN) :: ivar
  INTEGER(I4B), INTENT(IN) :: jvar

  INTEGER(I4B) :: ii
  INTEGER(I4B), ALLOCATABLE :: temp(:)
  LOGICAL(LGT), ALLOCATABLE :: maskVec(:)

  DO ii = obj%minNptrs, obj%maxNptrs
    IF (.NOT. obj%isNodePresent(globalNode=ii)) CYCLE
    temp = nodeToNode(obj%getNodeToNodes(GlobalNode=ii, IncludeSelf=.TRUE.))
    maskVec = colMesh%isNodePresent(globalNode=temp)

    IF (ANY(maskVec)) THEN
      CALL SetSparsity( &
        & obj=Mat, &
        & Row=ii, &
        & Col=PACK(temp, maskVec), &
        & ivar=ivar,  &
        & jvar=jvar)
    END IF
  END DO

  IF (ALLOCATED(temp)) DEALLOCATE (temp)
  IF (ALLOCATED(maskVec)) DEALLOCATE (maskVec)
END SUBROUTINE SetSparsity3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetSparsity4(obj, colMesh, nodeToNode, mat, &
  & rowGlobalToLocalNodeNum, rowLBOUND, rowUBOUND, &
  & colGlobalToLocalNodeNum, &
  & colLBOUND, colUBOUND, ivar, jvar)
  CLASS(Mesh_), INTENT(INOUT) :: obj
  ! [[Mesh_]] class
  CLASS(Mesh_), INTENT(INOUT) :: colMesh
  ! [[Mesh_]] class
  INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
  ! node to node connectivity between obj and colMesh
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  ! [[CSRMatrix_]] object
  INTEGER(I4B), INTENT(IN) :: rowLBOUND
  INTEGER(I4B), INTENT(IN) :: rowUBOUND
  INTEGER(I4B), INTENT(IN) :: rowGlobalToLocalNodeNum( &
    & rowLBOUND:rowUBOUND)
  ! Global to local node number map
  INTEGER(I4B), INTENT(IN) :: colLBOUND
  INTEGER(I4B), INTENT(IN) :: colUBOUND
  INTEGER(I4B), INTENT(IN) :: colGlobalToLocalNodeNum( &
    & colLBOUND:colUBOUND)
  INTEGER(I4B), INTENT(IN) :: ivar
  INTEGER(I4B), INTENT(IN) :: jvar

  INTEGER(I4B) :: ii
  INTEGER(I4B), ALLOCATABLE :: temp(:)
  LOGICAL(LGT), ALLOCATABLE :: maskVec(:)

  DO ii = obj%minNptrs, obj%maxNptrs

    IF (.NOT. obj%isNodePresent(globalNode=ii)) CYCLE

    temp = nodeToNode(obj%getNodeToNodes(GlobalNode=ii, IncludeSelf=.TRUE.))
    maskVec = colMesh%isNodePresent(globalNode=temp)

    IF (ANY(maskVec)) THEN
      CALL SetSparsity( &
        & obj=Mat, &
        & Row=rowGlobalToLocalNodeNum(ii), &
        & Col=colGlobalToLocalNodeNum(PACK(temp, maskVec)), &
        & ivar=ivar,  &
        & jvar=jvar)
    END IF

  END DO

  IF (ALLOCATED(temp)) DEALLOCATE (temp)
  IF (ALLOCATED(maskVec)) DEALLOCATE (maskVec)
END SUBROUTINE SetSparsity4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Old_SetSparsity3(obj, colMesh, nodeToNode, mat, &
  & ivar, jvar)
  CLASS(Mesh_), INTENT(INOUT) :: obj
  ! [[Mesh_]] class
  CLASS(Mesh_), INTENT(INOUT) :: colMesh
  ! [[Mesh_]] class
  INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
  ! Node to node connectivity between obj and colMesh
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  ! [[CSRMatrix_]] object
  INTEGER(I4B), INTENT(IN) :: ivar
  INTEGER(I4B), INTENT(IN) :: jvar

  INTEGER(I4B) :: ii, jj
  INTEGER(I4B), ALLOCATABLE :: n2n(:)

  DO ii = obj%minNptrs, obj%maxNptrs
    IF (.NOT. obj%isNodePresent(globalNode=ii)) CYCLE
    jj = nodeToNode(ii)
    IF (jj .EQ. 0) CYCLE
    IF (colMesh%isNodePresent(GlobalNode=jj)) THEN
      n2n = colMesh%getNodeToNodes(GlobalNode=jj, IncludeSelf=.TRUE.)
      CALL SetSparsity(obj=Mat, Row=ii, Col=n2n, ivar=ivar,  &
        & jvar=jvar)
    END IF
  END DO

  IF (ALLOCATED(n2n)) DEALLOCATE (n2n)
END SUBROUTINE Old_SetSparsity3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE old_SetSparsity4(obj, colMesh, nodeToNode, mat, &
  & rowGlobalToLocalNodeNum, rowLBOUND, rowUBOUND, &
  & colGlobalToLocalNodeNum, &
  & colLBOUND, colUBOUND, ivar, jvar)
  CLASS(Mesh_), INTENT(INOUT) :: obj
  ! [[Mesh_]] class
  CLASS(Mesh_), INTENT(INOUT) :: colMesh
  ! [[Mesh_]] class
  INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
  ! node to node connectivity between obj and colMesh
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  ! [[CSRMatrix_]] object
  INTEGER(I4B), INTENT(IN) :: rowLBOUND
  INTEGER(I4B), INTENT(IN) :: rowUBOUND
  INTEGER(I4B), INTENT(IN) :: rowGlobalToLocalNodeNum( &
    & rowLBOUND:rowUBOUND)
  ! Global to local node number map
  INTEGER(I4B), INTENT(IN) :: colLBOUND
  INTEGER(I4B), INTENT(IN) :: colUBOUND
  INTEGER(I4B), INTENT(IN) :: colGlobalToLocalNodeNum( &
    & colLBOUND:colUBOUND)
  INTEGER(I4B), INTENT(IN) :: ivar
  INTEGER(I4B), INTENT(IN) :: jvar

  INTEGER(I4B) :: ii, jj, kk
  INTEGER(I4B), ALLOCATABLE :: n2n(:)
  LOGICAL(LGT) :: chk

  DO ii = obj%minNptrs, obj%maxNptrs
    jj = nodeToNode(ii)
    kk = rowGlobalToLocalNodeNum(ii)
    chk = (  &
      & .NOT. obj%isNodePresent(globalNode=ii))  &
      & .OR. (jj .EQ. 0)  &
      & .OR. (.NOT. colMesh%isNodePresent(globalNode=jj))  &
      & .OR. (kk .EQ. 0)
    IF (chk) CYCLE
    n2n = colGlobalToLocalNodeNum( &
      & colMesh%getNodeToNodes(GlobalNode=jj, IncludeSelf=.TRUE.))
    CALL SetSparsity(obj=Mat, Row=kk, Col=n2n, ivar=ivar,  &
      & jvar=jvar)
  END DO
  IF (ALLOCATED(n2n)) DEALLOCATE (n2n)
END SUBROUTINE old_SetSparsity4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MeshUtility
