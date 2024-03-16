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
CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodetoNodes"

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
