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
USE Display_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
LOGICAL(LGT) :: isok
TYPE(ElemDataListIterator_) :: elemdata_iter, elemdata_end
CLASS(ElemData_), POINTER :: elemdata_ptr
! TYPE(NodeData_) :: nodedata
! TYPE(NodeData_), POINTER :: nodedata_ptr
INTEGER(I4B) :: ii, globalElemNum, jj, localNodeNum, globalNodeNum, tNodes
TYPE(IntList_), ALLOCATABLE :: node_to_elem_list_vec(:)

elemdata_iter = obj%elementDataList%Begin()
elemdata_end = obj%elementDataList%END()
ii = 0

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

DO ii = 1, tNodes
 CALL node_to_elem_list_vec(ii)%Display("nodeToElements("//tostring(ii)//"):")
  CALL BlankLines(nol=2)
END DO

END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_InitiateNodetoNodes
! ! Define internal  variables
! INTEGER(I4B) :: iel, iLocalNode, iGlobalNode
! INTEGER(I4B), ALLOCATABLE :: globalNodes(:), NearElements(:)
! CHARACTER(LEN=*), PARAMETER :: myName = "obj_InitiateNodetoNodes()"
!
! IF (obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1) RETURN
!
! IF (obj%isNodeToNodesInitiated) THEN
!   CALL e%raiseWarning(modName//"::"//myName//" - "// &
!     & "Node to node information is already initiated. If you want to &
!     & Reinitiate it then deallocate nodeData, first!!")
!   RETURN
! END IF
!
! IF (.NOT. obj%isNodeToElementsInitiated) &
!   & CALL obj%InitiateNodeToElements()
!
! obj%isNodeToNodesInitiated = .TRUE.
! DO iLocalNode = 1, obj%tNodes
!   iGlobalNode = obj%getGlobalNodeNumber(iLocalNode)
!   NearElements = obj%getNodeToElements(iGlobalNode)
!   DO iel = 1, SIZE(NearElements)
!     globalNodes = obj%getConnectivity(NearElements(iel))
!     globalNodes = PACK(globalNodes, globalNodes .NE. iGlobalNode)
!     CALL Append(obj%nodeData(iLocalNode)%globalNodes, globalNodes)
!   END DO
!   CALL RemoveDuplicates(obj%nodeData(iLocalNode)%globalNodes)
! END DO
! IF (ALLOCATED(globalNodes)) DEALLOCATE (globalNodes)
! IF (ALLOCATED(NearElements)) DEALLOCATE (NearElements)
! END PROCEDURE obj_InitiateNodetoNodes

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
!   iGlobalNode = obj%getGlobalNodeNumber(iLocalNode)
!   n2n = obj%getNodeToNodes(globalNode=iGlobalNode, IncludeSelf=.FALSE.)
!   n2e = obj%getNodeToElements(globalNode=iGlobalNode)
!
!   DO iel = 1, SIZE(n2e)
!     e2e = obj%getElementToElements(globalElement=n2e(iel), &
!       & onlyElements=.TRUE.)
!
!     mask_elem = .NOT. (e2e(:, 1) .ISIN.n2e)
!
!     DO iel2 = 1, SIZE(mask_elem)
!
!       IF (mask_elem(iel2)) THEN
!
!         indx = obj%getConnectivity(globalElement=e2e(iel2, 1))
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
