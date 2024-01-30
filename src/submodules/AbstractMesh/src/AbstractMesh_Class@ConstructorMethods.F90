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

obj%readFromFile = .TRUE.
obj%isInitiated = .TRUE.

CALL obj%IMPORT(hdf5=hdf5, group=group)

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
obj%uid = 0
obj%tElements_topology_wise = 0
obj%tElemTopologies = 0
obj%elemTopologies = 0
obj%nsd = 0
obj%maxNptrs = 0
obj%minNptrs = 0
obj%maxElemNum = 0
obj%minElemNum = 0
obj%tNodes = 0
obj%tIntNodes = 0
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
IF (ALLOCATED(obj%physicalTag)) DEALLOCATE (obj%physicalTag)
IF (ALLOCATED(obj%material)) DEALLOCATE (obj%material)
IF (ALLOCATED(obj%boundingEntity)) DEALLOCATE (obj%boundingEntity)
IF (ALLOCATED(obj%local_elemNumber)) DEALLOCATE (obj%local_elemNumber)
IF (ALLOCATED(obj%local_Nptrs)) DEALLOCATE (obj%local_Nptrs)
IF (ALLOCATED(obj%quality)) DEALLOCATE (obj%quality)
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

END SUBMODULE ConstructorMethods
