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
USE Display_Method
USE IntegerUtility
USE GlobalData, ONLY: stdout
USE AppendUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
INTEGER(I4B) :: ii, jj, globalElemNum, nn, localNodeNum,  &
  & globalNodeNum, nodewise_size(obj%tNodes)
TYPE(CPUTime_) :: TypeCPUTime
INTEGER(I4B), PARAMETER :: chunk_size = 32

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isNodeToElementsInitiated) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "NodeToElements is already initiated.")
  RETURN
END IF

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

obj%isNodeToElementsInitiated = .TRUE.

nodewise_size = 0

DO ii = 1, obj%tElements
  globalElemNum = obj%elementData(ii)%globalElemNum

  nn = SIZE(obj%elementData(ii)%globalNodes)

  DO jj = 1, nn
    globalNodeNum = obj%elementData(ii)%globalNodes(jj)
    localNodeNum = obj%local_nptrs(globalNodeNum)

    CALL Expand(vec=obj%nodeData(localNodeNum)%globalElements, &
      & n=nodewise_size(localNodeNum), chunk_size=chunk_size,  &
      & val=globalElemNum)
  END DO

END DO

! Now we have to fix the size of `nodeData%globalElements`
DO ii = 1, obj%tNodes
  CALL Expand(vec=obj%nodeData(ii)%globalElements, &
    & n=nodewise_size(ii), chunk_size=chunk_size, finished=.TRUE.)
END DO

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL Display(modName//" : "//myName//  &
    & " : time : "//  &
    & tostring(TypeCPUTime%GetTime()), unitno=stdout)
END IF

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
INTEGER(I4B) :: inode, nodewise_size, telem, iel, global_elem_num,  &
  & local_elem_num, tnode, ii, global_node_num, local_node_num
INTEGER(I4B), PARAMETER :: chunk_size = 64
LOGICAL(LGT) :: found(obj%tNodes), skip
INTEGER(I4B) :: temp(128 * 2)
TYPE(CPUTime_) :: TypeCPUTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isNodeToNodesInitiated) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "Node to node information is already initiated.")
  RETURN
END IF

IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

obj%isNodeToNodesInitiated = .TRUE.

DO inode = 1, obj%tNodes
  nodewise_size = 0
  found = .FALSE.
  telem = SIZE(obj%nodeData(inode)%globalElements)

  DO iel = 1, telem
    global_elem_num = obj%nodeData(inode)%globalElements(iel)
    local_elem_num = obj%GetLocalElemNumber(global_elem_num)

    tnode = SIZE(obj%elementData(local_elem_num)%globalNodes)
    DO ii = 1, tnode

      global_node_num = obj%elementData(local_elem_num)%globalNodes(ii)
      local_node_num = obj%GetLocalNodeNumber(global_node_num)

      skip = found(local_node_num) .OR. (inode .EQ. local_node_num)
      IF (.NOT. skip) THEN
        CALL Expand(vec=obj%nodeData(inode)%globalNodes, &
          & n=nodewise_size, chunk_size=chunk_size, &
          & val=global_node_num)
        found(local_node_num) = .TRUE.
      END IF

    END DO

  END DO

  CALL Expand(vec=obj%nodeData(inode)%globalNodes, &
    & n=nodewise_size, chunk_size=chunk_size, &
    & finished=.TRUE.)

END DO

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL Display(modName//" : "//myName//  &
    & " : time : "//  &
    & tostring(TypeCPUTime%GetTime()), unitno=stdout)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateNodetoNodes

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateExtraNodetoNodes
CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodetoNodes()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_InitiateExtraNodetoNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE NodeDataMethods
