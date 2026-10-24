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

SUBMODULE(AbstractMesh_Class) NodeDataMethods
USE Display_Method, ONLY: Display, tostring
USE IntegerUtility, ONLY: RemoveDuplicates, OPERATOR(.ISIN.)
USE GlobalData, ONLY: stdout
USE AppendUtility, ONLY: Append, Expand
USE NodeData_Class, ONLY: NodeData_ExpandGlobalElements
USE NodeData_Class, ONLY:NodeData_GetTotalGlobalElements
USE NodeData_Class, ONLY:NodeData_GetPointerToGlobalElements
USE NodeData_Class, ONLY:NodeData_ExpandGlobalNodes
USE NodeData_Class, ONLY: NodeData_Set
USE Kdtree2_Module, ONLY: Kdtree2_create
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMesh_Class@NodeMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                    InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
  INTEGER(I4B) :: ii, jj, globalElemNum, nn, localNodeNum, &
    globalNodeNum, nodewise_size(obj%tNodes)
  TYPE(CPUTime_) :: TypeCPUTime
  INTEGER(I4B), PARAMETER :: chunk_size = 32

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (obj%isNodeToElementsInitiated) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF
  obj%isNodeToElementsInitiated = .TRUE.

  IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

  nodewise_size = 0

  DO ii = 1, obj%tElements
    globalElemNum = obj%elementData(ii)%ptr%globalElemNum

    nn = SIZE(obj%elementData(ii)%ptr%globalNodes)

    DO jj = 1, nn

      globalNodeNum = obj%elementData(ii)%ptr%globalNodes(jj)
      localNodeNum = obj%local_nptrs(globalNodeNum)

      CALL NodeData_ExpandGlobalElements(obj=obj%nodeData(localNodeNum)%ptr, &
                                         n=nodewise_size(localNodeNum), &
                                         chunk_size=chunk_size, &
                                         val=globalElemNum)

    END DO

  END DO

! Now we have to fix the size of `nodeData%globalElements`
  DO ii = 1, obj%tNodes
    CALL NodeData_ExpandGlobalElements(obj=obj%nodeData(ii)%ptr, &
                                       n=nodewise_size(ii), &
                                       chunk_size=chunk_size, finished=.TRUE.)
  END DO

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName // " : " // myName // &
                 " : time : " // &
                 tostring(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodetoNodes
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodetoNodes()"
  INTEGER(I4B) :: inode, nodewise_size, telem, iel, global_elem_num, &
    local_elem_num, tnode, ii, global_node_num, local_node_num
  INTEGER(I4B), PARAMETER :: chunk_size = 64
  LOGICAL(LGT) :: found(obj%tNodes), skip
  TYPE(CPUTime_) :: TypeCPUTime
  INTEGER(I4B), POINTER :: globalElements(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (obj%isNodeToNodesInitiated) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

  IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

  obj%isNodeToNodesInitiated = .TRUE.

  DO inode = 1, obj%tNodes
    nodewise_size = 0
    found = .FALSE.
    telem = NodeData_GetTotalGlobalElements(obj%nodeData(inode)%ptr)
    globalElements => &
      NodeData_GetPointerToGlobalElements(obj%nodeData(inode)%ptr)

    DO iel = 1, telem
      global_elem_num = globalElements(iel)
      local_elem_num = obj%GetLocalElemNumber(global_elem_num)

      tnode = SIZE(obj%elementData(local_elem_num)%ptr%globalNodes)
      DO ii = 1, tnode

        global_node_num = obj%elementData(local_elem_num)%ptr%globalNodes(ii)
        local_node_num = obj%GetLocalNodeNumber(global_node_num)

        skip = found(local_node_num) .OR. (inode .EQ. local_node_num)
        IF (.NOT. skip) THEN
          CALL NodeData_ExpandGlobalNodes(obj=obj%nodeData(inode)%ptr, &
                                          n=nodewise_size, &
                                          chunk_size=chunk_size, &
                                          val=global_node_num)
          found(local_node_num) = .TRUE.
        END IF

      END DO

    END DO

    CALL NodeData_ExpandGlobalNodes(obj=obj%nodeData(inode)%ptr, &
                                    n=nodewise_size, chunk_size=chunk_size, &
                                    finished=.TRUE.)

  END DO

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName // " : " // myName // &
                 " : time : " // &
                 tostring(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

  globalElements => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_InitiateNodetoNodes

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateExtraNodetoNodes
! Define internal  variables
  INTEGER(I4B) :: iel, iel2, iLocalNode, iGlobalNode
  INTEGER(I4B), ALLOCATABLE :: n2n(:), e2e(:, :), n2e(:), &
    indx(:), extraGlobalNodes(:)
  LOGICAL(LGT), ALLOCATABLE :: mask_elem(:), mask_nptrs(:)
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodetoNodes()"
  LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

! problem = obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1
! IF (problem) RETURN

  problem = obj%isExtraNodeToNodesInitiated
  IF (problem) THEN

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  IF (.NOT. obj%isNodeToNodesInitiated) CALL obj%InitiateNodeToNodes()
  IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()
  IF (.NOT. obj%isElementToElementsInitiated) CALL &
    obj%InitiateElementToElements()

  nodeLoop: DO iLocalNode = 1, obj%tNodes

    iGlobalNode = obj%GetGlobalNodeNumber(iLocalNode)
    n2n = obj%GetNodeToNodes(globalNode=iGlobalNode, IncludeSelf=.FALSE.)
    n2e = obj%GetNodeToElements(globalNode=iGlobalNode)

    ! extra nodes of all the elements around the node
    CALL Reallocate(extraGlobalNodes, 0_I4B)

    n2eLoop: DO iel = 1, SIZE(n2e)
      e2e = obj%GetElementToElements(globalElement=n2e(iel), &
                                     onlyElements=.TRUE.)

      mask_elem = .NOT. (e2e(:, 1) .ISIN.n2e)

      maskLoop: DO iel2 = 1, SIZE(mask_elem)

        IF (mask_elem(iel2)) THEN

          indx = obj%GetConnectivity(globalElement=e2e(iel2, 1))
          mask_nptrs = .NOT. (indx.ISIN.n2n)

          CALL APPEND(extraGlobalNodes, indx, mask_nptrs)

        END IF

      END DO maskLoop

    END DO n2eLoop

    CALL RemoveDuplicates(extraGlobalNodes)

    ! NodeData_Set allocates extraGlobalNodes of the node data
    CALL NodeData_Set(obj=obj%nodeData(iLocalNode)%ptr, &
                      extraGlobalNodes=extraGlobalNodes)

  END DO nodeLoop

  obj%isExtraNodeToNodesInitiated = .TRUE.

  IF (ALLOCATED(n2n)) DEALLOCATE(n2n)
  IF (ALLOCATED(n2e)) DEALLOCATE(n2e)
  IF (ALLOCATED(e2e)) DEALLOCATE(e2e)
  IF (ALLOCATED(indx)) DEALLOCATE(indx)
  IF (ALLOCATED(mask_elem)) DEALLOCATE(mask_elem)
  IF (ALLOCATED(mask_nptrs)) DEALLOCATE(mask_nptrs)
  IF (ALLOCATED(extraGlobalNodes)) DEALLOCATE(extraGlobalNodes)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_InitiateExtraNodetoNodes

!----------------------------------------------------------------------------
!                                                            InitiateKdtree
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateKdtree
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateKdtree()"
#endif
  INTEGER(I4B) :: nsd, nrow, ncol
  TYPE(CPUTime_) :: TypeCPUTime
  REAL(DFP), ALLOCATABLE :: nodeCoord(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  IF (obj%showTime) CALL TypeCPUTime%SetStartTime()
#endif

  CALL obj%DeallocateKdtree()

  nsd = obj%GetNSD()

  ncol = obj%GetTotalNodes()
  ALLOCATE(nodeCoord(3, ncol))
  CALL obj%GetNodeCoord(nodeCoord=nodeCoord, nrow=nrow, ncol=ncol)

  ! FUNCTION Kdtree2_create(input_data, dim, sort, rearrange) RESULT(mr)
  obj%kdtree => Kdtree2_Create(input_data=nodeCoord(1:nsd, :), &
                               DIM=nsd, sort=.FALSE., rearrange=.TRUE.)

  ALLOCATE(obj%kdresult(ncol))

  DEALLOCATE(nodeCoord)

#ifdef DEBUG_VER
  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName // " : " // myName // &
                 " : time : " // &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_InitiateKdtree

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE NodeDataMethods
