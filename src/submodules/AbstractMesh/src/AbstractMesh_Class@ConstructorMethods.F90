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

SUBMODULE(AbstractMesh_Class) ConstructorMethods
USE Display_Method
USE GlobalData, ONLY: stdout
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL obj%IMPORT(hdf5=hdf5, group=group, dim=dim, entities=entities)

obj%readFromFile = .TRUE.
obj%isInitiated = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%readFromFile = .FALSE.
obj%isInitiated = .FALSE.
obj%isNodeToElementsInitiated = .FALSE.
obj%isNodeToNodesInitiated = .FALSE.
obj%isExtraNodeToNodesInitiated = .FALSE.
obj%isElementToElementsInitiated = .FALSE.
obj%isBoundaryDataInitiated = .FALSE.
obj%isFacetDataInitiated = .FALSE.
obj%isEdgeConnectivityInitiated = .FALSE.
obj%isFaceConnectivityInitiated = .FALSE.
obj%uid = 0
obj%tElements_topology_wise = 0
obj%tElemTopologies = 0
obj%elemTopologies = 0
obj%nsd = 0
obj%xidim = 0
obj%maxNNE = 0
obj%maxNptrs = 0
obj%minNptrs = 0
obj%maxElemNum = 0
obj%minElemNum = 0
obj%tNodes = 0
obj%tFaces = 0
obj%tEdges = 0
obj%tElements = 0
obj%minX = 0.0_DFP
obj%maxX = 0.0_DFP
obj%minY = 0.0_DFP
obj%maxY = 0.0_DFP
obj%minZ = 0.0_DFP
obj%maxZ = 0.0_DFP
obj%X = 0.0_DFP
obj%Y = 0.0_DFP
obj%Z = 0.0_DFP
IF (ALLOCATED(obj%boundingEntity)) DEALLOCATE (obj%boundingEntity)
IF (ALLOCATED(obj%local_elemNumber)) DEALLOCATE (obj%local_elemNumber)
IF (ALLOCATED(obj%local_Nptrs)) DEALLOCATE (obj%local_Nptrs)
IF (ALLOCATED(obj%quality)) DEALLOCATE (obj%quality)
IF (ALLOCATED(obj%facetElementType)) DEALLOCATE (obj%facetElementType)

IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
IF (ALLOCATED(obj%elementData)) DEALLOCATE (obj%elementData)
IF (ALLOCATED(obj%internalFacetData)) DEALLOCATE (obj%internalFacetData)
IF (ALLOCATED(obj%boundaryFacetData)) DEALLOCATE (obj%boundaryFacetData)

CALL obj%elementDataList%DEALLOCATE()
CALL obj%elementDataBinaryTree%DEALLOCATE()
CALL obj%nodeDataList%DEALLOCATE()
CALL obj%nodeDataBinaryTree%DEALLOCATE()
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    isEmpty
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isEmpty
ans = .FALSE.
IF (obj%tElements .LE. 0_I4B) THEN
  ans = .TRUE.
END IF

IF (obj%tNodes .LE. 0_I4B) THEN
  ans = .TRUE.
END IF
END PROCEDURE obj_isEmpty

!----------------------------------------------------------------------------
!                                           InitiateDynamicDataStructure
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateDynamicDataStructure
CHARACTER(*), PARAMETER :: myName = "obj_InitiateDynamicDataStructure()"
INTEGER(I4B) :: ii
TYPE(ElemData_), POINTER :: elemdata_ptr
TYPE(NodeData_), POINTER :: nodedata_ptr
TYPE(CPUTime_) :: TypeCPUTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractMesh_ is not initiated.')
  RETURN
END IF

IF (obj%tElements .EQ. 0) RETURN

CALL obj%elementDataList%Initiate()
CALL obj%elementDataBinaryTree%Initiate()
CALL obj%nodeDataList%Initiate()
CALL obj%nodeDataBinaryTree%Initiate()

IF (obj%showTime) THEN
  CALL Display("Showing Time States of InitiateDynamicDataStructure",  &
    & unitno=stdout)
  CALL EqualLine(unitno=stdout)
  CALL TypeCPUTime%SetStartTime()
END IF

DO ii = 1, obj%tElements
  elemdata_ptr => ElemData_Pointer()
  CALL ElemData_Copy(elemdata_ptr, obj%elementData(ii))
  CALL obj%elementDataList%Add(elemdata_ptr)
  CALL obj%elementDataBinaryTree%Insert(elemdata_ptr)
END DO

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL Display(modName//" : "//myName//  &
    & " : time for making elementDataList: "//  &
    & tostring(TypeCPUTime%GetTime()), unitno=stdout)
END IF

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

DO ii = 1, obj%tNodes
  nodedata_ptr => NodeData_Pointer()
  CALL NodeData_Copy(nodedata_ptr, obj%nodeData(ii))
  CALL obj%nodeDataList%Add(nodedata_ptr)
  CALL obj%nodeDataBinaryTree%Insert(nodedata_ptr)
END DO

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL Display(modName//" : "//myName//  &
    & " : time for making nodeDataList: "//  &
    & tostring(TypeCPUTime%GetTime()), unitno=stdout)
  CALL EqualLine(unitno=stdout)
END IF

elemdata_ptr => NULL()
nodedata_ptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateDynamicDataStructure

END SUBMODULE ConstructorMethods
