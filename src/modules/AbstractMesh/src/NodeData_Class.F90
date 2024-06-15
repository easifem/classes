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
USE AppendUtility, ONLY: Expand

IMPLICIT NONE

PRIVATE

PUBLIC :: NodeData_
PUBLIC :: NodeDataPointer_
PUBLIC :: NodeData_Pointer
PUBLIC :: NodeData_Display
PUBLIC :: NodeData_Deallocate
PUBLIC :: NodeData_lt
PUBLIC :: NodeData_eq
PUBLIC :: NodeData_SetID
PUBLIC :: NodeData_Copy
PUBLIC :: NodeData_GetGlobalNodeNum
PUBLIC :: NodeData_GetLocalNodeNum
PUBLIC :: NodeData_GetNodeType
PUBLIC :: NodeData_GetTotalGlobalNodes
PUBLIC :: NodeData_GetTotalExtraGlobalNodes
PUBLIC :: NodeData_GetTotalGlobalElements
PUBLIC :: NodeData_GetNodeCoord
PUBLIC :: NodeData_GetGlobalNodes
PUBLIC :: NodeData_GetExtraGlobalNodes
PUBLIC :: NodeData_GetGlobalNodes2
PUBLIC :: NodeData_GetGlobalElements
PUBLIC :: NodeData_SetGlobalNodeNum
PUBLIC :: NodeData_SetLocalNodeNum
PUBLIC :: NodeData_SetNodeType
PUBLIC :: NodeData_SetTotalGlobalNodes
PUBLIC :: NodeData_SetTotalExtraGlobalNodes
PUBLIC :: NodeData_SetTotalGlobalElements
PUBLIC :: NodeData_SetNodeCoord
PUBLIC :: NodeData_SetGlobalNodes
PUBLIC :: NodeData_SetExtraGlobalNodes
PUBLIC :: NodeData_SetGlobalElements
PUBLIC :: NodeData_Set
PUBLIC :: NodeData_ExpandGlobalElements
PUBLIC :: NodeData_ExpandGlobalNodes
PUBLIC :: NodeData_GetPointerToGlobalElements

PUBLIC :: TypeNode
PUBLIC :: Display

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
  PRIVATE
  INTEGER(I4B) :: globalNodeNum = 0
  !! Global node number
  INTEGER(I4B) :: localNodeNum = 0
  !! local node number
  INTEGER(I4B) :: nodeType = INTERNAL_NODE
  !! node type; INTERNAL_NODE, BOUNDARY_NODE, DOMAIN_BOUNDARY_NODE
  REAL(DFP) :: nodeCoord(3) = 0.0_DFP
  !! node coordinates
  INTEGER(I4B), ALLOCATABLE :: globalNodes(:)
    !! It contains the global node number surrouding the node
    !! It does not contain self global node number
  INTEGER(I4B), ALLOCATABLE :: extraGlobalNodes(:)
    !! These global nodes required in facet-element-data
  INTEGER(I4B), ALLOCATABLE :: globalElements(:)
    !! It contains the global element number surrounding the node
END TYPE NodeData_

!----------------------------------------------------------------------------
!                                                         NodeDataPointer_
!----------------------------------------------------------------------------

TYPE :: NodeDataPointer_
  CLASS(NodeData_), POINTER :: ptr => NULL()
END TYPE NodeDataPointer_

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
!                                                           GetGlobalNodeNum
!----------------------------------------------------------------------------

PURE FUNCTION NodeData_GetGlobalNodeNum(obj) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%globalNodeNum
END FUNCTION NodeData_GetGlobalNodeNum

!----------------------------------------------------------------------------
!                                                           GetGlobalNodeNum
!----------------------------------------------------------------------------

PURE FUNCTION NodeData_GetLocalNodeNum(obj) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%localNodeNum
END FUNCTION NodeData_GetLocalNodeNum

!----------------------------------------------------------------------------
!                                                           GetGlobalNodeNum
!----------------------------------------------------------------------------

PURE FUNCTION NodeData_GetNodeNum(obj, islocal) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  LOGICAL(LGT), INTENT(IN) :: islocal
  INTEGER(I4B) :: ans

  IF (islocal) THEN
    ans = obj%localNodeNum
  ELSE
    ans = obj%globalNodeNum
  END IF

END FUNCTION NodeData_GetNodeNum

!----------------------------------------------------------------------------
!                                                           GetGlobalNodeNum
!----------------------------------------------------------------------------

PURE FUNCTION NodeData_GetNodeType(obj) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%nodeType
END FUNCTION NodeData_GetNodeType

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

PURE FUNCTION NodeData_GetTotalGlobalNodes(obj) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalNodes)
END FUNCTION NodeData_GetTotalGlobalNodes

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

PURE FUNCTION NodeData_GetTotalExtraGlobalNodes(obj) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%extraGlobalNodes)
END FUNCTION NodeData_GetTotalExtraGlobalNodes

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

PURE FUNCTION NodeData_GetTotalGlobalElements(obj) RESULT(ans)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalElements)
END FUNCTION NodeData_GetTotalGlobalElements

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_GetNodeCoord(obj, ans, tsize)
  TYPE(NodeData_), INTENT(IN) :: obj
  REAL(DFP), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize
  tsize = 3
  ans(1:tsize) = obj%nodeCoord(1:tsize)
END SUBROUTINE NodeData_GetNodeCoord

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_GetGlobalNodes(obj, ans, tsize)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalNodes)
  DO ii = 1, tsize
    ans(ii) = obj%globalNodes(ii)
  END DO
END SUBROUTINE NodeData_GetGlobalNodes

!----------------------------------------------------------------------------
!                                                       GetExtraGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_GetExtraGlobalNodes(obj, ans, tsize)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%extraGlobalNodes)
  DO ii = 1, tsize
    ans(ii) = obj%extraGlobalNodes(ii)
  END DO
END SUBROUTINE NodeData_GetExtraGlobalNodes

!----------------------------------------------------------------------------
!                                                       GetExtraGlobalNodes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-15
! summary: returns both the global nodes and extra global nodes

PURE SUBROUTINE NodeData_GetGlobalNodes2(obj, ans, tsize)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii, tsize0

  tsize0 = SIZE(obj%globalNodes)

  DO ii = 1, tsize0
    ans(ii) = obj%globalNodes(ii)
  END DO

  tsize = SIZE(obj%extraGlobalNodes)
  DO ii = 1, tsize
    ans(ii + tsize0) = obj%extraGlobalNodes(ii)
  END DO

  tsize = tsize0 + tsize
END SUBROUTINE NodeData_GetGlobalNodes2

!----------------------------------------------------------------------------
!                                                           GetGlobalElements
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_GetGlobalElements(obj, ans, tsize)
  TYPE(NodeData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalElements)
  DO ii = 1, tsize
    ans(ii) = obj%globalElements(ii)
  END DO
END SUBROUTINE NodeData_GetGlobalElements

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
  obj1%nodeCoord = obj2%nodeCoord
  IF (ALLOCATED(obj2%globalNodes)) obj1%globalNodes = obj2%globalNodes
  IF (ALLOCATED(obj2%globalElements)) obj1%globalElements = &
    obj2%globalElements
  IF (ALLOCATED(obj2%extraGlobalNodes)) obj1%extraGlobalNodes = &
    obj2%extraGlobalNodes

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

  CALL Display(obj%nodeCoord(1), msg="xCoord: ", unitno=unitno)
  CALL Display(obj%nodeCoord(2), msg="yCoord: ", unitno=unitno)
  CALL Display(obj%nodeCoord(3), msg="zCoord: ", unitno=unitno)

! globalNodes
  IF (ALLOCATED(obj%globalNodes)) THEN
    CALL Display(obj%globalNodes, msg="globalNodes: ", unitno=unitno)
  END IF

! extraGlobalNodes
  IF (ALLOCATED(obj%extraGlobalNodes)) THEN
    CALL Display(obj%extraGlobalNodes, msg="extraGlobalNodes: ", &
                 unitno=unitno)
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
  obj%nodeCoord = 0.0_DFP
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

SUBROUTINE NodeData_Set(obj, globalNodeNum, localNodeNum, &
           nodeType, globalNodes, globalElements, extraGlobalNodes, nodeCoord)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNodeNum
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: localNodeNum
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: nodeType
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNodes(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElements(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: extraGlobalNodes(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: nodeCoord(3)

  IF (PRESENT(globalNodeNum)) obj%globalNodeNum = globalNodeNum
  IF (PRESENT(localNodeNum)) obj%localNodeNum = localNodeNum
  IF (PRESENT(nodeType)) obj%nodeType = nodeType
  IF (PRESENT(globalNodes)) obj%globalNodes = globalNodes
  IF (PRESENT(globalElements)) obj%globalElements = globalElements
  IF (PRESENT(extraGlobalNodes)) obj%extraGlobalNodes = extraGlobalNodes
  IF (PRESENT(nodeCoord)) obj%nodeCoord = nodeCoord
END SUBROUTINE NodeData_Set

!----------------------------------------------------------------------------
!                                                           SetGlobalNodeNum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-15
! summary: Set the global node number

PURE SUBROUTINE NodeData_SetGlobalNodeNum(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans
  obj%globalNodeNum = ans
END SUBROUTINE NodeData_SetGlobalNodeNum

!----------------------------------------------------------------------------
!                                                           SetLocalNodeNum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-15
! summary: Set the lobal node number

PURE SUBROUTINE NodeData_SetLocalNodeNum(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans
  obj%localNodeNum = ans
END SUBROUTINE NodeData_SetLocalNodeNum

!----------------------------------------------------------------------------
!                                                           GetGlobalNodeNum
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_SetNodeType(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans
  obj%nodeType = ans
END SUBROUTINE NodeData_SetNodeType

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_SetTotalGlobalNodes(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans
  ALLOCATE (obj%globalNodes(ans))
END SUBROUTINE NodeData_SetTotalGlobalNodes

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_SetTotalExtraGlobalNodes(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans
  ALLOCATE (obj%extraGlobalNodes(ans))
END SUBROUTINE NodeData_SetTotalExtraGlobalNodes

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_SetTotalGlobalElements(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans
  ALLOCATE (obj%globalElements(ans))
END SUBROUTINE NodeData_SetTotalGlobalElements

!----------------------------------------------------------------------------
!                                                               SetNodeCoord
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_SetNodeCoord(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: ans(3)
  obj%nodeCoord = ans
END SUBROUTINE NodeData_SetNodeCoord

!----------------------------------------------------------------------------
!                                                             SetGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_SetGlobalNodes(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans(:)

  INTEGER(I4B) :: tsize, ii

  tsize = SIZE(obj%globalNodes)
  DO ii = 1, tsize
    obj%globalNodes(ii) = ans(ii)
  END DO
END SUBROUTINE NodeData_SetGlobalNodes

!----------------------------------------------------------------------------
!                                                             SetGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_SetExtraGlobalNodes(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans(:)

  INTEGER(I4B) :: tsize, ii

  tsize = SIZE(obj%extraGlobalNodes)
  DO ii = 1, tsize
    obj%extraGlobalNodes(ii) = ans(ii)
  END DO
END SUBROUTINE NodeData_SetExtraGlobalNodes

!----------------------------------------------------------------------------
!                                                             SetGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_SetGlobalElements(obj, ans)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: ans(:)

  INTEGER(I4B) :: tsize, ii

  tsize = SIZE(obj%globalElements)
  DO ii = 1, tsize
    obj%globalElements(ii) = ans(ii)
  END DO
END SUBROUTINE NodeData_SetGlobalElements

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
!                                                       ExpandGlobalElements
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_ExpandGlobalElements(obj, n, chunk_size, val, &
                                              finished)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `vec`.
    !! must be initialized to `size(vec)`
    !! (or 0 if not allocated) before first call
  INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `vec` in blocks of this size (>0)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: val
    !! the value to add to `vec`
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `vec`
    !! as its correct size (`n`)

  CALL Expand(vec=obj%globalElements, n=n, chunk_size=chunk_size, &
              val=val, finished=finished)

END SUBROUTINE NodeData_ExpandGlobalElements

!----------------------------------------------------------------------------
!                                                       ExpandGlobalElements
!----------------------------------------------------------------------------

PURE SUBROUTINE NodeData_ExpandGlobalNodes(obj, n, chunk_size, val, &
                                           finished)
  TYPE(NodeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `vec`.
    !! must be initialized to `size(vec)`
    !! (or 0 if not allocated) before first call
  INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `vec` in blocks of this size (>0)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: val
    !! the value to add to `vec`
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `vec`
    !! as its correct size (`n`)

  CALL Expand(vec=obj%globalNodes, n=n, chunk_size=chunk_size, &
              val=val, finished=finished)

END SUBROUTINE NodeData_ExpandGlobalNodes

!----------------------------------------------------------------------------
!                                                GetPointerToGlobalElements
!----------------------------------------------------------------------------

FUNCTION NodeData_GetPointerToGlobalElements(obj) RESULT(ans)
  TYPE(NodeData_), TARGET, INTENT(IN) :: obj
  INTEGER(I4B), POINTER :: ans(:)
  ans => obj%globalElements
END FUNCTION NodeData_GetPointerToGlobalElements

END MODULE NodeData_Class
