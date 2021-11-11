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

SUBMODULE(DomainConnectivity_Class) NodeMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          setNodeToNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateNodeToNodeData1
CHARACTER(LEN=*), PARAMETER :: myName = "dc_InitiateNodeToNodeData1"
CLASS(Mesh_), POINTER :: mesh1 => NULL()
CLASS(Mesh_), POINTER :: mesh2 => NULL()
TYPE(BoundingBox_) :: Box
INTEGER(I4B), ALLOCATABLE :: nptrs1(:), nptrs2(:)
INTEGER(I4B) :: ii, jj, nsd
REAL(DFP) :: X(3)
REAL(DFP), POINTER :: node1(:, :)
REAL(DFP), POINTER :: node2(:, :)
!> main
!> check domain1 initiated
IF (.NOT. domain1%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Domain-1 is not initiated, first initiate")
END IF
!> check domain2 initiated
IF (.NOT. domain2%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Domain-2 is not initiated, first initiate")
END IF
!> check
IF (obj%isNodeToNode) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "It seems, obj%nodeToNode data is already initiated")
END IF
!> get mesh pointer
mesh1 => domain1%GetMeshPointer(dim=dim1, entityNum=entityNum1)
mesh2 => domain2%GetMeshPointer(dim=dim2, entityNum=entityNum2)
!! TODO
!! is it possible to have bounds of obj%NodeToNode from
!! mesh1%minNptrs to mesh1%maxNptrs, it will save the space
CALL Reallocate(obj%NodeToNode, mesh1%maxNptrs)
obj%isNodeToNode = .TRUE.
!> make intersection box
IF ((mesh1%GetBoundingBox()) .isIntersect. (mesh2%GetBoundingBox())) THEN
  Box = (mesh1%GetBoundingBox()) .INTERSECTION. (mesh2%GetBoundingBox())
ELSE
  CALL e%RaiseError(modName//"::"//myName//" - "// &
  & 'The two mesh does not overlap each other.')
END IF
! now we get Nptrs in Box for node1, node2
node1 => domain1%GetNodeCoordPointer()
node2 => domain2%GetNodeCoordPointer()
nptrs1 = Box.Nptrs.node1; nptrs2 = Box.Nptrs.node2
  !! Note nptrs1 and nptrs2 are local node numbers in domain1 and domain2
nsd = SIZE(node1, 1)
DO ii = 1, SIZE(nptrs1)
  X(1:nsd) = node1(1:nsd, nptrs1(ii))
  DO jj = 1, SIZE(nptrs2)
    IF (ALL(X(1:nsd) .APPROXEQ.node2(1:nsd, nptrs2(jj)))) THEN
      obj%NodeToNode(domain1%GetGlobalNodeNumber(nptrs1(ii)))  &
        & = domain2%GetGlobalNodeNumber(nptrs2(jj))
      EXIT
    END IF
  END DO
END DO
IF (ALLOCATED(nptrs1)) DEALLOCATE (nptrs1)
IF (ALLOCATED(nptrs2)) DEALLOCATE (nptrs2)
NULLIFY (node1, node2, mesh1, mesh2)
END PROCEDURE dc_InitiateNodeToNodeData1

!----------------------------------------------------------------------------
!                                                          setNodeToNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateNodeToNodeData2
CHARACTER(LEN=*), PARAMETER :: myName = "dc_InitiateNodeToNodeData2"
TYPE(BoundingBox_) :: Box
INTEGER(I4B), ALLOCATABLE :: nptrs1(:), nptrs2(:)
INTEGER(I4B) :: ii, jj, nsd
REAL(DFP) :: X(3)
REAL(DFP), POINTER :: node1(:, :)
REAL(DFP), POINTER :: node2(:, :)
!> main
!> check domain1 initiated
IF (.NOT. domain1%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Domain-1 is not initiated, first initiate")
END IF
!> check domain2 initiated
IF (.NOT. domain2%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Domain-2 is not initiated, first initiate")
END IF
!! TODO
!! is it possible to have bounds of obj%NodeToNode from
!! domain1%minNptrs to domain1%maxNptrs, it will save the space
CALL Reallocate(obj%NodeToNode, domain1%maxNptrs)
!> make intersection box
IF ((domain1%GetBoundingBox()) &
  & .isIntersect.  &
  & (domain2%GetBoundingBox())) THEN
  Box = (domain1%GetBoundingBox()) .INTERSECTION. (domain2%GetBoundingBox())
ELSE
  CALL e%RaiseError(modName//"::"//myName//" - "// &
  & 'The two domain does not overlap each other.')
END IF
! now we get Nptrs in Box for node1, node2
node1 => domain1%GetNodeCoordPointer()
node2 => domain2%GetNodeCoordPointer()
nptrs1 = Box.Nptrs.node1; nptrs2 = Box.Nptrs.node2
  !! Note nptrs1 and nptrs2 are local node numbers in domain1 and domain2
nsd = SIZE(node1, 1)
DO ii = 1, SIZE(nptrs1)
  X(1:nsd) = node1(1:nsd, nptrs1(ii))
  DO jj = 1, SIZE(nptrs2)
    IF (ALL(X(1:nsd) .APPROXEQ.node2(1:nsd, nptrs2(jj)))) THEN
      obj%NodeToNode(domain1%GetGlobalNodeNumber(nptrs1(ii)))  &
        & = domain2%GetGlobalNodeNumber(nptrs2(jj))
      EXIT
    END IF
  END DO
END DO
IF (ALLOCATED(nptrs1)) DEALLOCATE (nptrs1)
IF (ALLOCATED(nptrs2)) DEALLOCATE (nptrs2)
NULLIFY (node1, node2)
END PROCEDURE dc_InitiateNodeToNodeData2

!----------------------------------------------------------------------------
!                                                       getNodeToNodePointer
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_getNodeToNodePointer
ans => obj%NodeToNode
END PROCEDURE dc_getNodeToNodePointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE NodeMethods
