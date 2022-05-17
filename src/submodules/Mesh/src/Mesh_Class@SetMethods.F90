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

SUBMODULE(Mesh_Class) setMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setBoundingBox1
  obj%minX = (.Xmin.box)
  obj%minY = (.Ymin.box)
  obj%minZ = (.Zmin.box)
  obj%maxX = (.Xmax.box)
  obj%maxY = (.Ymax.box)
  obj%maxZ = (.Zmax.box)
END PROCEDURE mesh_setBoundingBox1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setBoundingBox2
  TYPE(BoundingBox_) :: box
  Box = obj%getBoundingBox(nodes=nodes, local_nptrs=local_nptrs)
  CALL obj%setBoundingBox(box=box)
  CALL DEALLOCATE (box)
END PROCEDURE mesh_setBoundingBox2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setSparsity1
  CHARACTER(LEN=*), PARAMETER :: myName = "mesh_setSparsity1"
  INTEGER(I4B) :: i, j, k
  INTEGER(I4B), ALLOCATABLE :: n2n(:)
  !> main
  IF (.NOT. obj%isInitiated) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF
  !> main
  DO i = 1, obj%tNodes
    j = obj%getGlobalNodeNumber(LocalNode=i)
    k = localNodeNumber(j)
    IF (k .NE. 0) THEN
      n2n = localNodeNumber( &
        & obj%getNodeToNodes(GlobalNode=j, IncludeSelf=.TRUE.))
      CALL SetSparsity(obj=Mat, Row=k, Col=n2n)
    END IF
  END DO
  IF (ALLOCATED(n2n)) DEALLOCATE (n2n)
END PROCEDURE mesh_setSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setSparsity2
  CHARACTER(LEN=*), PARAMETER :: myName = "mesh_setSparsity2"
  INTEGER(I4B) :: i, j
  INTEGER(I4B), ALLOCATABLE :: n2n(:)
  !> main
  IF (.NOT. obj%isInitiated) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF
  DO i = 1, obj%tNodes
    j = obj%getGlobalNodeNumber(LocalNode=i)
    n2n = obj%getNodeToNodes(GlobalNode=j, IncludeSelf=.TRUE.)
    CALL SetSparsity(obj=Mat, Row=j, Col=n2n)
  END DO
  IF (ALLOCATED(n2n)) DEALLOCATE (n2n)
END PROCEDURE mesh_setSparsity2

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_SetSparsity3
  CHARACTER(LEN=*), PARAMETER :: myName = "mesh_setSparsity3"
  INTEGER(I4B) :: ii, jj
  INTEGER(I4B), ALLOCATABLE :: n2n(:)
  !> main
  !> check
  IF (.NOT. obj%isInitiated) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF
  !> check
  IF (.NOT. colMesh%isInitiated) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "colMesh data is not initiated, first initiate")
  END IF
  !> check
  IF (SIZE(nodeToNode) .NE. obj%maxNptrs) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "SIZE( nodeToNode ) .NE. obj%maxNptrs [easifemClasses ISSUE#63]")
  END IF
  !>
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
END PROCEDURE mesh_SetSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setSparsity4
  CHARACTER(LEN=*), PARAMETER :: myName = "mesh_setSparsity4"
  INTEGER(I4B) :: ii, jj, kk
  INTEGER(I4B), ALLOCATABLE :: n2n(:)
  LOGICAL(LGT) :: chk
  !> main
  IF (.NOT. obj%isInitiated) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF
  !> check
  IF (.NOT. colMesh%isInitiated) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "colMesh data is not initiated, first initiate")
  END IF
  !> check
  IF (SIZE(nodeToNode) .LT. obj%maxNptrs) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "SIZE( nodeToNode ) .LT. obj%maxNptrs [easifemClasses ISSUE#63]")
  END IF
  !> main
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
END PROCEDURE mesh_setSparsity4

!----------------------------------------------------------------------------
!                                                           setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setTotalMaterial
  CALL reallocate(obj%material, n)
END PROCEDURE mesh_setTotalMaterial

!----------------------------------------------------------------------------
!                                                                setMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setMaterial
  obj%material(medium) = material
END PROCEDURE mesh_setMaterial

!----------------------------------------------------------------------------
!                                                        setFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setFacetElementType1
  INTEGER( I4B ) :: localElem
  localElem = obj%getLocalElemNumber( globalElement=globalElement )
  obj%facetElementType( iface, localElem ) = facetElementType
  obj%elementData( localElem )%elementType = facetElementType
END PROCEDURE mesh_setFacetElementType1

!----------------------------------------------------------------------------
!                                                        setFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setFacetElementType2
  obj%facetData( facetElement )%elementType = facetElementType
END PROCEDURE mesh_setFacetElementType2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE setMethods
