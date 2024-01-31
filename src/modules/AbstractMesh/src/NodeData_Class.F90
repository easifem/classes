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

MODULE NodeData_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE Display_Method, ONLY: Display
IMPLICIT NONE
PRIVATE

PUBLIC :: NodeData_
PUBLIC :: TypeNode
PUBLIC :: NodeDataSet
PUBLIC :: NodeData_Pointer
PUBLIC :: NodeData_Display
PUBLIC :: Display
PUBLIC :: NodeData_Deallocate
PUBLIC :: NodeData_lt
PUBLIC :: NodeData_eq
PUBLIC :: NodeData_SetID
PUBLIC :: NodeData_Copy

INTEGER(I4B), PARAMETER, PUBLIC :: INTERNAL_NODE = 1
INTEGER(I4B), PARAMETER, PUBLIC :: BOUNDARY_NODE = -1
INTEGER(I4B), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_NODE = -2
INTEGER(I4B), PARAMETER, PUBLIC :: GHOST_NODE = -3

INTERFACE Display
  MODULE PROCEDURE NodeData_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                                 NodeData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Data type for storing node data

TYPE :: NodeData_
  INTEGER(I4B) :: globalNodeNum = 0
    !! Global node number
  INTEGER(I4B) :: localNodeNum = 0
    !! local node number
  INTEGER(I4B) :: nodeType = INTERNAL_NODE
    !! node type; INTERNAL_NODE, BOUNDARY_NODE, DOMAIN_BOUNDARY_NODE
  INTEGER(I4B), ALLOCATABLE :: globalNodes(:)
    !! It contains the global node number surrouding the node
    !! It does not contain self global node number
  INTEGER(I4B), ALLOCATABLE :: globalElements(:)
    !! It contains the global element number surrounding the node
  INTEGER(I4B), ALLOCATABLE :: extraGlobalNodes(:)
    !! These global nodes required in facet-element-data
END TYPE NodeData_

!----------------------------------------------------------------------------
!                                                             NodeDataType_
!----------------------------------------------------------------------------

TYPE :: NodeDataType_
  INTEGER(I4B) :: internal = INTERNAL_NODE
  INTEGER(I4B) :: boundary = BOUNDARY_NODE
  INTEGER(I4B) :: domainBoundary = DOMAIN_BOUNDARY_NODE
  INTEGER(I4B) :: ghost = GHOST_NODE
END TYPE NodeDataType_

TYPE(NodeDataType_), PARAMETER :: TypeNode = NodeDataType_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2024-01-31
! summary:  Copy

SUBROUTINE NodeData_Copy(obj1, obj2)
  TYPE(NodeData_), INTENT(INOUT) :: obj1
  TYPE(NodeData_), INTENT(IN) :: obj2

  obj1%globalNodeNum = obj2%globalNodeNum
  obj1%localNodeNum = obj2%localNodeNum
  obj1%nodeType = obj2%nodeType
  IF (ALLOCATED(obj2%globalNodes)) obj1%globalNodes = obj2%globalNodes
  IF (ALLOCATED(obj2%globalElements)) obj1%globalElements  &
    & = obj2%globalElements
  IF (ALLOCATED(obj2%extraGlobalNodes)) obj1%extraGlobalNodes  &
    & = obj2%extraGlobalNodes

END SUBROUTINE NodeData_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-29
! summary: Display a single instance of NodeData_

SUBROUTINE NodeData_Display(obj, msg, unitno)
  TYPE(NodeData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(msg, unitno=unitno)
  CALL Display(obj%globalNodeNum, msg="globalNodeNum: ", unitno=unitno)
  CALL Display(obj%localNodeNum, msg="localNodeNum: ", unitno=unitno)

  SELECT CASE (obj%nodeType)
  CASE (INTERNAL_NODE)
    CALL Display("nodeType: INTERNAL_NODE", unitno=unitno)
  CASE (BOUNDARY_NODE)
    CALL Display("nodeType: BOUNDARY_NODE", unitno=unitno)
  CASE (DOMAIN_BOUNDARY_NODE)
    CALL Display("nodeType: DOMAIN_BOUNDARY_NODE", unitno=unitno)
  CASE (GHOST_NODE)
    CALL Display("nodeType: GHOST_NODE", unitno=unitno)
  END SELECT

  CALL Display(obj%localNodeNum, msg="localNodeNum: ", unitno=unitno)

! globalNodes
  IF (ALLOCATED(obj%globalNodes)) THEN
    CALL Display(obj%globalNodes, msg="globalNodes: ", unitno=unitno)
  END IF

! extraGlobalNodes
  IF (ALLOCATED(obj%extraGlobalNodes)) THEN
    CALL Display(obj%extraGlobalNodes, msg="extraGlobalNodes: ", &
      & unitno=unitno)
  END IF

! globalElements
  IF (ALLOCATED(obj%globalElements)) THEN
    CALL Display(obj%globalElements, msg="globalElements: ", unitno=unitno)
  END IF
END SUBROUTINE NodeData_Display

!----------------------------------------------------------------------------
!                                                       NodeData_Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  Deallocate data

SUBROUTINE NodeData_Deallocate(obj)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  obj%globalNodeNum = 0
  obj%localNodeNum = 0
  obj%nodeType = INTERNAL_NODE
  IF (ALLOCATED(obj%globalNodes)) DEALLOCATE (obj%globalNodes)
  IF (ALLOCATED(obj%globalElements)) DEALLOCATE (obj%globalElements)
  IF (ALLOCATED(obj%extraGlobalNodes)) DEALLOCATE (obj%extraGlobalNodes)
END SUBROUTINE NodeData_Deallocate

!----------------------------------------------------------------------------
!                                                              NodeData_lt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  lessar than operator

FUNCTION NodeData_lt(obj, obj2) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  TYPE(NodeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%globalNodeNum .LT. obj2%globalNodeNum
END FUNCTION NodeData_lt

!----------------------------------------------------------------------------
!                                                              NodeData_eq
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  equality operator

FUNCTION NodeData_eq(obj, obj2) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  TYPE(NodeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%globalNodeNum .EQ. obj2%globalNodeNum
END FUNCTION NodeData_eq

!----------------------------------------------------------------------------
!                                                                  SetID
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  Set the local node number

SUBROUTINE NodeData_SetID(obj, id)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  obj%localNodeNum = id
END SUBROUTINE NodeData_SetID

!----------------------------------------------------------------------------
!                                                         NodeData_Pointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  Returns pointer to NodeData_

FUNCTION NodeData_Pointer() RESULT(ans)
  CLASS(NodeData_), POINTER :: ans
  ALLOCATE (NodeData_ :: ans)
END FUNCTION NodeData_Pointer

!----------------------------------------------------------------------------
!                                                               NodeDataSet
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  Set node data

SUBROUTINE NodeDataSet(obj, globalNodeNum, localNodeNum,  &
  & nodeType, globalNodes, globalElements, extraGlobalNodes)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNodeNum
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: localNodeNum
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: nodeType
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNodes(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElements(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: extraGlobalNodes(:)

  IF (PRESENT(globalNodeNum)) obj%globalNodeNum = globalNodeNum
  IF (PRESENT(localNodeNum)) obj%localNodeNum = localNodeNum
  IF (PRESENT(nodeType)) obj%nodeType = nodeType
  IF (PRESENT(globalNodes)) obj%globalNodes = globalNodes
  IF (PRESENT(globalElements)) obj%globalElements = globalElements
  IF (PRESENT(extraGlobalNodes)) obj%extraGlobalNodes = extraGlobalNodes
END SUBROUTINE NodeDataSet

END MODULE NodeData_Class
