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
  & globalNodeNum
TYPE(CPUTime_) :: TypeCPUTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isNodeToElementsInitiated) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: NodeToElements is already initiated.")
  RETURN
END IF

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

obj%isNodeToElementsInitiated = .TRUE.

DO ii = 1, obj%tElements
  globalElemNum = obj%elementData(ii)%globalElemNum

  nn = SIZE(obj%elementData(ii)%globalNodes)

  DO jj = 1, nn
    globalNodeNum = obj%elementData(ii)%globalNodes(jj)
    localNodeNum = obj%local_nptrs(globalNodeNum)

    CALL Append(obj%nodeData(localNodeNum)%globalElements, &
      & globalElemNum)

  END DO

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
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
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
