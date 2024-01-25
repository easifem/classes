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

SUBMODULE(Mesh_Class) NodeDataMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
! Define internal  variables
INTEGER(I4B) :: ii, jj, globalElemNum
INTEGER(I4B), ALLOCATABLE :: local_nptrs(:)
CHARACTER(LEN=*), PARAMETER :: myName = "obj_InitiateNodeToElements()"

IF (obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1) RETURN
IF (obj%isNodeToElementsInitiated) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "NodeToElements information is already initiated. If you want to &
    & Reinitiate it then deallocate nodeData, first!!")
  RETURN
END IF

obj%isNodeToElementsInitiated = .TRUE.
DO ii = 1, obj%tElements
  globalElemNum = obj%getGlobalElemNumber(ii)
  local_nptrs = obj%getLocalNodeNumber(obj%getConnectivity(globalElemNum))
  DO jj = 1, SIZE(local_nptrs)
    CALL Append(obj%nodeData(local_nptrs(jj))%globalElements, &
      & globalElemNum)
  END DO
END DO
IF (ALLOCATED(local_nptrs)) DEALLOCATE (local_nptrs)
END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodetoNodes
! Define internal  variables
INTEGER(I4B) :: iel, iLocalNode, iGlobalNode
INTEGER(I4B), ALLOCATABLE :: globalNodes(:), NearElements(:)
CHARACTER(LEN=*), PARAMETER :: myName = "obj_InitiateNodetoNodes()"

IF (obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1) RETURN

IF (obj%isNodeToNodesInitiated) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "Node to node information is already initiated. If you want to &
    & Reinitiate it then deallocate nodeData, first!!")
  RETURN
END IF

IF (.NOT. obj%isNodeToElementsInitiated) &
  & CALL obj%InitiateNodeToElements()

obj%isNodeToNodesInitiated = .TRUE.
DO iLocalNode = 1, obj%tNodes
  iGlobalNode = obj%getGlobalNodeNumber(iLocalNode)
  NearElements = obj%getNodeToElements(iGlobalNode)
  DO iel = 1, SIZE(NearElements)
    globalNodes = obj%getConnectivity(NearElements(iel))
    globalNodes = PACK(globalNodes, globalNodes .NE. iGlobalNode)
    CALL Append(obj%nodeData(iLocalNode)%globalNodes, globalNodes)
  END DO
  CALL RemoveDuplicates(obj%nodeData(iLocalNode)%globalNodes)
END DO
IF (ALLOCATED(globalNodes)) DEALLOCATE (globalNodes)
IF (ALLOCATED(NearElements)) DEALLOCATE (NearElements)
END PROCEDURE obj_InitiateNodetoNodes

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

!! nodeToNodes
!! nodeToElements
!! elementToElements

MODULE PROCEDURE obj_InitiateExtraNodetoNodes
! Define internal  variables
INTEGER(I4B) :: iel, iel2, iLocalNode, iGlobalNode
INTEGER(I4B), ALLOCATABLE :: n2n(:), e2e(:, :), n2e(:), &
  & indx(:)
LOGICAL(LGT), ALLOCATABLE :: mask_elem(:), mask_nptrs(:)
CHARACTER(LEN=*), PARAMETER :: myName = "obj_InitiateExtraNodetoNodes"

IF (obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1) RETURN

IF (obj%isExtraNodeToNodesInitiated) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "Node to node information is already initiated. If you want to &
    & Reinitiate it then deallocate nodeData, first!!")
  RETURN
END IF

IF (.NOT. obj%isNodeToNodesInitiated) &
  & CALL obj%InitiateNodeToNodes()

IF (.NOT. obj%isNodeToElementsInitiated) &
  & CALL obj%InitiateNodeToElements()

IF (.NOT. obj%isElementToElementsInitiated) &
  & CALL obj%InitiateElementToElements()

DO iLocalNode = 1, obj%tNodes
  iGlobalNode = obj%getGlobalNodeNumber(iLocalNode)
  n2n = obj%getNodeToNodes(globalNode=iGlobalNode, IncludeSelf=.FALSE.)
  n2e = obj%getNodeToElements(globalNode=iGlobalNode)

  DO iel = 1, SIZE(n2e)
    e2e = obj%getElementToElements(globalElement=n2e(iel), &
      & onlyElements=.TRUE.)

    mask_elem = .NOT. (e2e(:, 1) .ISIN.n2e)

    DO iel2 = 1, SIZE(mask_elem)

      IF (mask_elem(iel2)) THEN

        indx = obj%getConnectivity(globalElement=e2e(iel2, 1))
        mask_nptrs = .NOT. (indx.ISIN.n2n)
        CALL APPEND(obj%nodeData(iLocalNode)%extraGlobalNodes, &
          & indx, mask_nptrs)

      END IF

    END DO

    CALL RemoveDuplicates(obj%nodeData(iLocalNode)%extraGlobalNodes)

  END DO

END DO

obj%isExtraNodeToNodesInitiated = .TRUE.

IF (ALLOCATED(n2n)) DEALLOCATE (n2n)
IF (ALLOCATED(n2e)) DEALLOCATE (n2e)
IF (ALLOCATED(e2e)) DEALLOCATE (e2e)
IF (ALLOCATED(indx)) DEALLOCATE (indx)
IF (ALLOCATED(mask_elem)) DEALLOCATE (mask_elem)
IF (ALLOCATED(mask_nptrs)) DEALLOCATE (mask_nptrs)

END PROCEDURE obj_InitiateExtraNodetoNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE NodeDataMethods
