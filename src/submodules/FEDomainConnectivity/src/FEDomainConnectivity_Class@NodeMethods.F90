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

SUBMODULE(FEDomainConnectivity_Class) NodeMethods
USE BaseType, ONLY: BoundingBox_

USE BoundingBox_Method, ONLY: OPERATOR(.isIntersect.), &
                              OPERATOR(.INTERSECTION.)

USE ReallocateUtility, ONLY: Reallocate
USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
USE Display_Method, ONLY: Display, ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    InitiateNodeToNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToNodeData1
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToNodeData1()"

TYPE(BoundingBox_) :: box, box1, box2
LOGICAL(LGT) :: isvar, isok
INTEGER(I4B), ALLOCATABLE :: nptrs1(:)
INTEGER(I4B) :: ii, jj, nsd, tnodes1, node1, node2
REAL(DFP) :: x1(3), x2(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL domain1%GetParam(isInitiated=isok)
CALL AssertError1(isok, myName, &
               'AbstractDomain_::domain1 is not initiated, first initiate it')

CALL domain2%GetParam(isInitiated=isok)
CALL AssertError1(isok, myName, &
               'AbstractDomain_::domain2 is not initiated, first initiate it')

isok = obj%isNodeToNode
IF (isok) THEN
  CALL e%RaiseInformation(modName//"::"//myName//" - "// &
               "[INFO] :: It seems, obj%nodeToNode data is already initiated")
  RETURN
END IF

#endif

isok = obj%isNodeToNode
IF (isok) RETURN

ii = domain1%GetTotalNodes()
CALL Reallocate(obj%nodeToNode, ii)
obj%isNodeToNode = .TRUE.

box1 = domain1%GetBoundingBox()
box2 = domain2%GetBoundingBox()
isvar = box1.isIntersect.box2

CALL AssertError1(isvar, myName, &
         'The two domains (domain1 and domain2) does not overlap each other.')

box = box1.INTERSECTION.box2

CALL domain1%GetNptrsInBox(nptrs=nptrs1, box=box, isStrict=.FALSE.)
nsd = domain1%GetNSD()
tnodes1 = SIZE(nptrs1)

DO ii = 1, tnodes1
  node1 = nptrs1(ii)
  CALL domain1%GetNodeCoord(globalNode=node1, nodeCoord=x1, &
                            islocal=.TRUE.)

  CALL domain2%GetNearestNode(qv=x1, x=x2, globalNode=node2)

  isok = ALL(x1.APPROXEQ.x2)
  ! jj = domain1%GetGlobalNodeNumber(node1)

  IF (isok) obj%nodeToNode(node1) = node2
END DO

IF (ALLOCATED(nptrs1)) DEALLOCATE (nptrs1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateNodeToNodeData1

!----------------------------------------------------------------------------
!                                                    InitiateNodeToNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToNodeData2
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToNodeData2()"
TYPE(BoundingBox_) :: box, box1, box2
LOGICAL(LGT) :: isvar, isok
INTEGER(I4B), ALLOCATABLE :: nptrs1(:)
INTEGER(I4B) :: ii, jj, nsd, tnodes1, node1(1), node2(2), nrow, ncol
REAL(DFP) :: x1(3, 1), x2(3, 1)

#ifdef DEBUG_VER

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL mesh1%GetParam(isInitiated=isok)
! check mesh1 initiated
CALL AssertError1(isok, myName, &
                  'The mesh1 is not initiated, first initiate it')

CALL mesh2%GetParam(isInitiated=isok)
CALL AssertError1(isok, myName, &
                  'The mesh2 is not initiated, first initiate it')

isok = obj%isNodeToNode
IF (isok) THEN
  CALL e%RaiseInformation(modName//"::"//myName//" - "// &
               "[INFO] :: It seems, obj%nodeToNode data is already initiated")
  RETURN
END IF

#endif

isok = obj%isNodeToNode
IF (isok) RETURN

ii = mesh1%GetTotalNodes()
CALL Reallocate(obj%nodeToNode, ii)
obj%isNodeToNode = .TRUE.

box1 = mesh1%GetBoundingBox()
box2 = mesh2%GetBoundingBox()
isvar = box1.isIntersect.box2

CALL AssertError1(isvar, myName, &
                  'The two mesh does not overlap each other.')

box = box1.INTERSECTION.box2

! INFO: These nptrs are global node number in mesh1
CALL mesh1%GetNptrsInBox(nptrs=nptrs1, box=box, isStrict=.FALSE.)
nsd = mesh1%GetNSD()

tnodes1 = SIZE(nptrs1)

DO ii = 1, tnodes1
  node1(1) = nptrs1(ii)
  CALL mesh1%GetNodeCoord(globalNode=node1, nodeCoord=x1, &
                          islocal=.FALSE., nrow=nrow, ncol=ncol)

  !INFO: node2 is global node number in mesh2
  CALL mesh2%GetNearestNode(qv=x1(:, 1), x=x2(:, 1), &
                            globalNode=node2(1))

  isok = ALL(x1.APPROXEQ.x2)
  jj = mesh1%GetLocalNodeNumber(node1(1))

  IF (isok) obj%nodeToNode(jj) = node2(1)

END DO

IF (ALLOCATED(nptrs1)) DEALLOCATE (nptrs1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateNodeToNodeData2

!----------------------------------------------------------------------------
!                                                       GetNodeToNodePointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodePointer
ans => obj%NodeToNode
END PROCEDURE obj_GetNodeToNodePointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE NodeMethods
