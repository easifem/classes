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
USE GlobalData, ONLY: I4B
USE Display_Method, ONLY: Display
IMPLICIT NONE
PRIVATE

PUBLIC :: NodeData_
PUBLIC :: TypeNode
PUBLIC :: NodeData_Display

INTEGER(I4B), PARAMETER, PUBLIC :: INTERNAL_NODE = 1
INTEGER(I4B), PARAMETER, PUBLIC :: BOUNDARY_NODE = -1
INTEGER(I4B), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_NODE = -2
INTEGER(I4B), PARAMETER, PUBLIC :: GHOST_NODE = -3

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
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Display a single instance of NodeData_

SUBROUTINE NodeData_Display(obj, msg, unitno)
  TYPE(NodeData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(TRIM(msg), unitno=unitno)
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

END MODULE NodeData_Class
