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

SUBMODULE(DynamicMesh_Class) NodeDataMethods
USE IntList_Class
USE IntSet_Class
USE Display_Method
USE ReallocateUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, globalElemNum, jj, localNodeNum, globalNodeNum,  &
  & tNodes, tsize
TYPE(ElemDataListIterator_) :: elemdata_iter, elemdata_end
TYPE(ElemData_), POINTER :: elemdata_ptr
TYPE(IntList_), ALLOCATABLE :: node_to_elem_list_vec(:)
TYPE(IntListIterator_) :: intList_iter
TYPE(NodeDataListIterator_) :: nodeDataList_iter, nodeDataList_end
TYPE(NodeData_), POINTER :: nodedata_ptr
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"

#ifdef DEBUG_VER

IF (obj%isNodeToElementsInitiated) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'isNodeToElementsInitiated is True, Nothing to do.')
  RETURN
END IF

#else

IF (obj%isNodeToElementsInitiated) RETURN

#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

obj%isNodeToElementsInitiated = .TRUE.

elemdata_iter = obj%elementDataList%Begin()
elemdata_end = obj%elementDataList%END()

tNodes = obj%GetTotalNodes()
ALLOCATE (node_to_elem_list_vec(tNodes))

DO ii = 1, tNodes
  CALL node_to_elem_list_vec(ii)%Initiate()
END DO

DO WHILE (elemdata_iter .NE. elemdata_end)
  elemdata_ptr => elemdata_iter%VALUE
  isok = ASSOCIATED(elemdata_ptr)
  IF (.NOT. isok) CYCLE

  globalElemNum = elemdata_ptr%globalElemNum
  DO jj = 1, SIZE(elemdata_ptr%globalNodes)
    globalNodeNum = elemdata_ptr%globalNodes(jj)
    localNodeNum = obj%local_Nptrs(globalNodeNum)
    CALL node_to_elem_list_vec(localNodeNum)%Add(globalElemNum)
  END DO

  CALL elemdata_iter%Inc()
END DO

nodeDataList_iter = obj%nodeDataList%Begin()
nodeDataList_end = obj%nodeDataList%END()

DO
  isok = nodeDataList_iter .EQ. nodeDataList_end
  IF (isok) EXIT

  nodedata_ptr => nodeDataList_iter%VALUE
  isok = ASSOCIATED(nodedata_ptr)
  IF (.NOT. isok) CYCLE

  localNodeNum = nodedata_ptr%localNodeNum

  tsize = node_to_elem_list_vec(localNodeNum)%SIZE()
  CALL Reallocate(nodedata_ptr%globalElements, tsize)

  intList_iter = node_to_elem_list_vec(localNodeNum)%Begin()
  tsize = node_to_elem_list_vec(localNodeNum)%SIZE()

  DO ii = 1, tsize
    nodedata_ptr%globalElements(ii) = intList_iter%VALUE
    CALL intList_iter%Inc()
  END DO

  CALL nodeDataList_iter%Inc()
END DO

NULLIFY (nodedata_ptr)

DO ii = 1, tNodes
  CALL node_to_elem_list_vec(ii)%DEALLOCATE()
END DO

DEALLOCATE (node_to_elem_list_vec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodetoNodes
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodetoNodes()"
INTEGER(I4B) :: ii, jj, tsize
INTEGER(I4B), PARAMETER :: max_node_around_a_node = 50
LOGICAL(LGT) :: isok
TYPE(NodeDataListIterator_) :: nodeDataList_iter, nodeDataList_end
TYPE(NodeData_), POINTER :: nodedata_ptr
TYPE(ElemData_) :: elemdata
TYPE(ElemData_), POINTER :: elemdata_ptr
TYPE(IntSet_) :: intset
TYPE(IntSetIterator_) :: intset_iter

#ifdef DEBUG_VER
IF (obj%isNodeToNodesInitiated) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'isNodeToNodesInitiated is TRUE, Nothing TODO.')
  RETURN
END IF
#else
IF (obj%isNodeToNodesInitiated) RETURN
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

obj%isNodeToNodesInitiated = .TRUE.

nodeDataList_iter = obj%nodeDataList%Begin()
nodeDataList_end = obj%nodeDataList%END()

DO
  isok = nodeDataList_iter .EQ. nodeDataList_end
  IF (isok) EXIT

  nodedata_ptr => nodeDataList_iter%VALUE
  isok = ASSOCIATED(nodedata_ptr)
  IF (.NOT. isok) CYCLE

  CALL intset%Initiate(max_node_around_a_node)

  DO ii = 1, SIZE(nodedata_ptr%globalElements)
    elemdata%globalElemNum = nodedata_ptr%globalElements(ii)
    elemdata_ptr => obj%elementDataBinaryTree%GetValuePointer(elemdata)

    DO jj = 1, SIZE(elemdata_ptr%globalNodes)
      CALL intset%Set(key=elemdata_ptr%globalNodes(jj), VALUE=.TRUE.)
    END DO
  END DO

  ! NOTE:
  ! We do not want to include the self node, so we are removing this node
  CALL intset%Remove(key=nodedata_ptr%globalNodeNum, success=isok)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Could not remove key for self globalNodeNum')
  END IF

  tsize = intset%SIZE()
  CALL Reallocate(nodedata_ptr%globalNodes, tsize)
  CALL intset_iter%Begin(intset)

  DO ii = 1, tsize
    CALL intset_iter%next(nodedata_ptr%globalNodes(ii), isok)
  END DO

  CALL intset%DEALLOCATE()

  CALL nodeDataList_iter%Inc()
END DO

nodedata_ptr => NULL()
elemdata_ptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateNodetoNodes

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

!! nodeToNodes
!! nodeToElements
!! elementToElements

! MODULE PROCEDURE obj_InitiateExtraNodetoNodes
! ! Define internal  variables
! INTEGER(I4B) :: iel, iel2, iLocalNode, iGlobalNode
! INTEGER(I4B), ALLOCATABLE :: n2n(:), e2e(:, :), n2e(:), &
!   & indx(:)
! LOGICAL(LGT), ALLOCATABLE :: mask_elem(:), mask_nptrs(:)
! CHARACTER(LEN=*), PARAMETER :: myName = "obj_InitiateExtraNodetoNodes"
!
! IF (obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1) RETURN
!
! IF (obj%isExtraNodeToNodesInitiated) THEN
!   CALL e%raiseWarning(modName//"::"//myName//" - "// &
!     & "Node to node information is already initiated. If you want to &
!     & Reinitiate it then deallocate nodeData, first!!")
!   RETURN
! END IF
!
! IF (.NOT. obj%isNodeToNodesInitiated) &
!   & CALL obj%InitiateNodeToNodes()
!
! IF (.NOT. obj%isNodeToElementsInitiated) &
!   & CALL obj%InitiateNodeToElements()
!
! IF (.NOT. obj%isElementToElementsInitiated) &
!   & CALL obj%InitiateElementToElements()
!
! DO iLocalNode = 1, obj%tNodes
!   iGlobalNode = obj%GetGlobalNodeNumber(iLocalNode)
!   n2n = obj%GetNodeToNodes(globalNode=iGlobalNode, IncludeSelf=.FALSE.)
!   n2e = obj%GetNodeToElements(globalNode=iGlobalNode)
!
!   DO iel = 1, SIZE(n2e)
!     e2e = obj%GetElementToElements(globalElement=n2e(iel), &
!       & onlyElements=.TRUE.)
!
!     mask_elem = .NOT. (e2e(:, 1) .ISIN.n2e)
!
!     DO iel2 = 1, SIZE(mask_elem)
!
!       IF (mask_elem(iel2)) THEN
!
!         indx = obj%GetConnectivity(globalElement=e2e(iel2, 1))
!         mask_nptrs = .NOT. (indx.ISIN.n2n)
!         CALL APPEND(obj%nodeData(iLocalNode)%extraGlobalNodes, &
!           & indx, mask_nptrs)
!
!       END IF
!
!     END DO
!
!     CALL RemoveDuplicates(obj%nodeData(iLocalNode)%extraGlobalNodes)
!
!   END DO
!
! END DO
!
! obj%isExtraNodeToNodesInitiated = .TRUE.
!
! IF (ALLOCATED(n2n)) DEALLOCATE (n2n)
! IF (ALLOCATED(n2e)) DEALLOCATE (n2e)
! IF (ALLOCATED(e2e)) DEALLOCATE (e2e)
! IF (ALLOCATED(indx)) DEALLOCATE (indx)
! IF (ALLOCATED(mask_elem)) DEALLOCATE (mask_elem)
! IF (ALLOCATED(mask_nptrs)) DEALLOCATE (mask_nptrs)
!
! END PROCEDURE obj_InitiateExtraNodetoNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE NodeDataMethods
