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
CHARACTER(*), PARAMETER :: myName = "dc_InitiateNodeToNodeData1"
CLASS(Mesh_), POINTER :: mesh1 => NULL()
CLASS(Mesh_), POINTER :: mesh2 => NULL()
TYPE(BoundingBox_) :: box, box1, box2
LOGICAL(LGT) :: isvar, problem, isok
INTEGER(I4B), ALLOCATABLE :: nptrs1(:), nptrs2(:)
INTEGER(I4B) :: ii, jj, nsd
REAL(DFP) :: X(3)
REAL(DFP), POINTER :: node1(:, :)
REAL(DFP), POINTER :: node2(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

isok = domain1%isInit()
! check domain1 initiated
IF (.NOT. isok) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Domain-1 is not initiated, first initiate")
END IF

IF (.NOT. domain2%isInit()) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Domain-2 is not initiated, first initiate")
END IF

IF (obj%isNodeToNode) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "It seems, obj%nodeToNode data is already initiated")
END IF

mesh1 => domain1%GetMeshPointer(dim=dim1, entityNum=entityNum1)
mesh2 => domain2%GetMeshPointer(dim=dim2, entityNum=entityNum2)
! TODO
! is it possible to have bounds of obj%NodeToNode from
! mesh1%minNptrs to mesh1%maxNptrs, it will save the space
CALL Reallocate(obj%NodeToNode, mesh1%GetMaxNodeNumber())
obj%isNodeToNode = .TRUE.

!> make intersection box

box1 = mesh1%GetBoundingBox()
box2 = mesh2%GetBoundingBox()
isvar = box1.isIntersect.box2

IF (isvar) THEN
  box = box1.INTERSECTION.box2
ELSE
  CALL Display(box1, "box1 = ", unitno=stderr)
  CALL Display(box2, "box2 = ", unitno=stderr)
  CALL e%RaiseError(modName//"::"//myName//" - "// &
  & 'The two mesh does not overlap each other.')
END IF
! now we Get Nptrs in Box for node1, node2
node1 => domain1%GetNodeCoordPointer()
node2 => domain2%GetNodeCoordPointer()
nptrs1 = Box.Nptrs.node1; nptrs2 = Box.Nptrs.node2
! Note nptrs1 and nptrs2 are local node numbers in domain1 and domain2
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE dc_InitiateNodeToNodeData1

!----------------------------------------------------------------------------
!                                                          setNodeToNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateNodeToNodeData2
CHARACTER(*), PARAMETER :: myName = "dc_InitiateNodeToNodeData2"
TYPE(BoundingBox_) :: Box
! TYPE(BoundingBox_) :: searchBox
INTEGER(I4B), ALLOCATABLE :: nptrs1(:), nptrs2(:), global_nptrs1(:),&
& global_nptrs2(:)
INTEGER(I4B) :: ii, jj, nsd
REAL(DFP) :: X(3)
! REAL(DFP) :: search_box_lim(6), search_box_len(3)
REAL(DFP), POINTER :: node1(:, :)
REAL(DFP), POINTER :: node2(:, :)

REAL(DFP) :: debug_t1, debug_t2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. domain1%isInit()) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
  & "Domain-1 is not initiated, first initiate")
END IF

IF (.NOT. domain2%isInit()) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
  & "Domain-2 is not initiated, first initiate")
END IF

IF (obj%isNodeToNode) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
  & 'NodeToNode data is already initiated!')
END IF

! TODO:
! is it possible to have bounds of obj%NodeToNode from
! domain1%minNptrs to domain1%maxNptrs, it will save the space

CALL Reallocate(obj%NodeToNode, domain1%maxNptrs)
obj%isNodeToNode = .TRUE.

CALL Display("Make intersection box ")

IF ((domain1%GetBoundingBox()) &
  & .isIntersect.  &
  & (domain2%GetBoundingBox())) THEN
  Box = (domain1%GetBoundingBox()) .INTERSECTION. (domain2%GetBoundingBox())
ELSE
  CALL e%RaiseError(modName//"::"//myName//" - "// &
  & 'The two domain does not overlap each other.')
END IF

CALL Display("Get Nptrs in Box for node1 node2")

node1 => domain1%GetNodeCoordPointer()
node2 => domain2%GetNodeCoordPointer()

CALL Display("DomainConnectivity_::obj%NodeToNode() = domain2%GetGlobalNodeNumber()")
IF (ASSOCIATED(node1, node2)) THEN
  CALL Display("node1 and node2 are same...")
  !
  ! Note nptrs1 and nptrs2 are local node numbers in domain1 and domain2
  !
  nptrs1 = Box.Nptrs.node1

  CALL Display("Get global Nptrs in Box for node1")
  global_nptrs1 = domain1%GetGlobalNodeNumber(nptrs1)

  DO ii = 1, SIZE(nptrs1)
    obj%NodeToNode(global_nptrs1(ii)) = global_nptrs1(ii)
  END DO

ELSE
  ! CALL CPU_TIME(debug_t1)
  !
  ! Note nptrs1 and nptrs2 are local node numbers in domain1 and domain2
  !
  nptrs1 = Box.Nptrs.node1
  nptrs2 = Box.Nptrs.node2

  CALL Display("Get global Nptrs in Box for node1 node2")
  global_nptrs1 = domain1%GetGlobalNodeNumber(nptrs1)
  global_nptrs2 = domain2%GetGlobalNodeNumber(nptrs2)
  nsd = SIZE(node1, 1)

  ! search_box_lim = 0.0_DFP
  ! search_box_len(1) = 0.01_DFP * ((.Xmax.Box) - (.Xmin.Box))
  ! search_box_len(2) = 0.01_DFP * ((.Ymax.Box) - (.Ymin.Box))
  ! search_box_len(3) = 0.01_DFP * ((.Zmax.Box) - (.Zmin.Box))

  DO ii = 1, SIZE(nptrs1)
    X(1:nsd) = node1(1:nsd, nptrs1(ii))
    ! search_box_lim(1:nsd) = X(1:nsd) - search_box_len(1:nsd)
    ! search_box_lim(nsd + 1:nsd + nsd) = X(1:nsd) + search_box_len(1:nsd)
    ! CALL Initiate(obj=searchBox, nsd=nsd, lim=search_box_lim)
    !
    ! nptrs2 = searchBox.Nptrs.node2
    ! global_nptrs2 = domain2%GetGlobalNodeNumber(nptrs2)
    DO jj = 1, SIZE(nptrs2)
      IF (ALL(X(1:nsd) .APPROXEQ.node2(1:nsd, nptrs2(jj)))) THEN
        obj%NodeToNode(global_nptrs1(ii))  &
          & = global_nptrs2(jj)
        EXIT
      END IF
    END DO
  END DO
  ! CALL CPU_TIME(debug_t2)
  ! CALL Display(debug_t2 - debug_t1, "debug time = ")
  ! STOP
END IF
!
IF (ALLOCATED(nptrs1)) DEALLOCATE (nptrs1)
IF (ALLOCATED(nptrs2)) DEALLOCATE (nptrs2)
IF (ALLOCATED(global_nptrs1)) DEALLOCATE (global_nptrs1)
IF (ALLOCATED(global_nptrs2)) DEALLOCATE (global_nptrs2)
NULLIFY (node1, node2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE dc_InitiateNodeToNodeData2

!----------------------------------------------------------------------------
!                                                       GetNodeToNodePointer
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_GetNodeToNodePointer
ans => obj%NodeToNode
END PROCEDURE dc_GetNodeToNodePointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE NodeMethods
