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
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setBoundingBox1
obj%minX = (.Xmin.box)
obj%minY = (.Ymin.box)
obj%minZ = (.Zmin.box)
obj%maxX = (.Xmax.box)
obj%maxY = (.Ymax.box)
obj%maxZ = (.Zmax.box)
END PROCEDURE mesh_setBoundingBox1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setBoundingBox2
TYPE(BoundingBox_) :: box
Box = obj%getBoundingBox(nodes=nodes, local_nptrs=local_nptrs)
CALL obj%setBoundingBox(box=box)
CALL DEALLOCATE (box)
END PROCEDURE mesh_setBoundingBox2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setSparsity1
CHARACTER(LEN=*), PARAMETER :: myName = "mesh_setSparsity1"
!!
!! check
!!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Mesh data is not initiated, first initiate")
END IF
!!
!! check
!!
IF (.NOT. obj%isNodeToNodesInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'In mesh NodeToNodeData is not initiated')
END IF
!!
!! Call from MeshUtility
!!
CALL SetSparsity1(obj=obj, mat=mat, localNodeNumber=localNodeNumber, &
  & lbound=lbound, ubound=ubound)
!!
END PROCEDURE mesh_setSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setSparsity2
CHARACTER(LEN=*), PARAMETER :: myName = "mesh_setSparsity2"
!!
!! check
!!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Mesh data is not initiated, first initiate")
END IF
!!
!! check
!!
IF (.NOT. obj%isNodeToNodesInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'In mesh NodeToNodeData is not initiated')
END IF
!!
!! Call from MeshUtility
!!
CALL SetSparsity2(obj=obj, mat=mat)
!!
END PROCEDURE mesh_setSparsity2

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_SetSparsity3
CHARACTER(LEN=*), PARAMETER :: myName = "mesh_setSparsity3"
!!
!! check
!!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Mesh data is not initiated, first initiate")
END IF
!!
!! check
!!
IF (.NOT. colMesh%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "colMesh data is not initiated, first initiate")
END IF
!!
!! check
!!
IF (SIZE(nodeToNode) .NE. obj%maxNptrs) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "SIZE( nodeToNode ) .NE. obj%maxNptrs [easifemClasses ISSUE#63]")
END IF
!!
!! Call from MeshUtility
!!
CALL SetSparsity3(obj=obj, colMesh=colMesh, nodeToNode=nodeToNode, &
  & mat=mat, ivar=ivar, jvar=jvar)
!!
END PROCEDURE mesh_SetSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setSparsity4
CHARACTER(LEN=*), PARAMETER :: myName = "mesh_setSparsity4"
!!
!! Check
!!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Mesh data is not initiated, first initiate")
END IF
!!
!! Check
!!
IF (.NOT. colMesh%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "colMesh data is not initiated, first initiate")
END IF
!!
!! Check
!!
IF (SIZE(nodeToNode) .LT. obj%maxNptrs) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "SIZE( nodeToNode ) .LT. obj%maxNptrs [easifemClasses ISSUE#63]")
END IF
!!
!! Call from MeshUtility
!!
CALL SetSparsity4(obj=obj, colMesh=colMesh, nodeToNode=nodeToNode, &
  & mat=mat, rowGlobalToLocalNodeNum=rowGlobalToLocalNodeNum, &
  & colGlobalToLocalNodeNum=colGlobalToLocalNodeNum, &
  & rowLBOUND=rowLBOUND, rowUBOUND=rowUBOUND, &
  & colLBOUND=colLBOUND, colUBOUND=colUBOUND, &
  & ivar=ivar, jvar=jvar)
!!
END PROCEDURE mesh_setSparsity4

!----------------------------------------------------------------------------
!                                                           setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setTotalMaterial
CALL reallocate(obj%material, n)
END PROCEDURE mesh_setTotalMaterial

!----------------------------------------------------------------------------
!                                                                setMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setMaterial
obj%material(medium) = material
END PROCEDURE mesh_setMaterial

!----------------------------------------------------------------------------
!                                                        setFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setFacetElementType
INTEGER(I4B) :: localElem
localElem = obj%getLocalElemNumber(globalElement=globalElement)
obj%facetElementType(iface, localElem) = facetElementType
obj%elementData(localElem)%elementType = facetElementType
END PROCEDURE mesh_setFacetElementType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
