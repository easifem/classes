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

SUBMODULE(Mesh_Class) SetMethods
USE BaseMethod
USE MeshUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setSparsity1
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity1()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (.NOT. obj%isInit()) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "Mesh data is not initiated, first initiate")
END IF

! if the mesh is empty then return
IF (obj%getTotalElements() .EQ. 0_I4B) THEN
  CALL e%RaiseWarning(modName//'::'//myName//' - '// &
  & 'Empty mesh found, returning')
  RETURN
END IF

! check
IF (.NOT. obj%isNodeToNodes()) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'In mesh NodeToNodeData is not initiated')
END IF

! Call from MeshUtility
CALL SetSparsity1(obj=obj, mat=mat, localNodeNumber=localNodeNumber, &
  & lbound=lbound, ubound=ubound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_setSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setSparsity2
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity2()"

! check
IF (.NOT. obj%isInit()) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "Mesh data is not initiated, first initiate")
END IF

! check
IF (.NOT. obj%isNodeToNodes()) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'In mesh NodeToNodeData is not initiated')
END IF

! Call from MeshUtility
CALL SetSparsity2(obj=obj, mat=mat)

END PROCEDURE obj_setSparsity2

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity3()"

! check
IF (.NOT. obj%isInit()) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
END IF

! check
IF (.NOT. colMesh%isInit()) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: colMesh data is not initiated, first initiate")
END IF

! check
IF (SIZE(nodeToNode) .NE. obj%GetMaxNodeNumber() ) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "SIZE( nodeToNode ) .NE. obj%maxNptrs [easifemClasses ISSUE#63]")
END IF

! check
IF (.NOT. obj%isNodeToNodes() ) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'In mesh NodeToNodeData is not initiated')
END IF

! Call from MeshUtility
SELECT TYPE (colMesh)
CLASS IS (Mesh_)
  CALL SetSparsity3(obj=obj, colMesh=colMesh, nodeToNode=nodeToNode, &
    & mat=mat, ivar=ivar, jvar=jvar)
CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for the type of colMesh')
END SELECT

END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setSparsity4
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity4()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (.NOT. obj%isInit() ) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
END IF

IF (.NOT. colMesh%isInit() ) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: colMesh data is not initiated, first initiate")
END IF

IF (SIZE(nodeToNode) .LT. obj%GetMaxNodeNumber() ) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: SIZE( nodeToNode ) .LT. obj%maxNptrs "//  &
    & "[easifemClasses ISSUE#63]")
END IF

IF (.NOT. obj%isNodeToNodes() ) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: In mesh NodeToNodeData is not initiated')
END IF

! NOTE: Call from MeshUtility
SELECT TYPE (colMesh)
CLASS IS (Mesh_)
  CALL SetSparsity4(obj=obj, colMesh=colMesh, nodeToNode=nodeToNode, &
    & mat=mat, rowGlobalToLocalNodeNum=rowGlobalToLocalNodeNum, &
    & colGlobalToLocalNodeNum=colGlobalToLocalNodeNum, &
    & rowLBOUND=rowLBOUND, rowUBOUND=rowUBOUND, &
    & colLBOUND=colLBOUND, colUBOUND=colUBOUND, &
    & ivar=ivar, jvar=jvar)
CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for given type of colMesh')
END SELECT

END PROCEDURE obj_setSparsity4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
