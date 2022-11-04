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

SUBMODULE(Mesh_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_addSurrogate
CALL e%addSurrogate(userObj)
END PROCEDURE mesh_addSurrogate

!----------------------------------------------------------------------------
!                                                             addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE addSurrogate_mesh
CALL e%addSurrogate(userObj)
END PROCEDURE addSurrogate_mesh

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiate
  !!
CHARACTER(LEN=*), PARAMETER :: myName = "mesh_initiate"
  !!
  !!
  !!
obj%readFromFile = .TRUE.
obj%isInitiated = .TRUE.
  !!
  !! Import
  !!
CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & '[START] Importing mesh')
  !!
  !! Import
  !!
CALL obj%Import(hdf5, group)
  !!
  !! raiseInformation
  !!
CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Mesh imported')
  !!
  !!
  !!
IF (obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1) THEN
  RETURN
ELSE
    !!
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'InitiateNodeToElements()')
  CALL obj%InitiateNodeToElements()
    !!
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'InitiateNodeToNodes()')
  CALL obj%InitiateNodeToNodes()
    !!
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'InitiateElementToElements()')
  CALL obj%InitiateElementToElements()
    !!
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'InitiateBoundaryData()')
  CALL obj%InitiateBoundaryData()
    !!
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'InitiateFacetElements()')
  CALL obj%InitiateFacetElements()
    !!
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'InitiateExtraNodeToNodes()')
  CALL obj%InitiateExtraNodeToNodes()
    !!
END IF
  !!
  !!
  !!
CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & '[END] Importing mesh [OK!]')
  !!
END PROCEDURE mesh_initiate

!----------------------------------------------------------------------------
!                                                                      Mesh
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Constructor1
CALL ans%Initiate(hdf5, group)
END PROCEDURE mesh_Constructor1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Constructor_1
ALLOCATE (Mesh_ :: ans)
CALL ans%Initiate(hdf5, group)
END PROCEDURE mesh_Constructor_1

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Deallocate
CHARACTER(LEN=*), PARAMETER :: myName = "mesh_Deallocate"
  !!
obj%readFromFile = .FALSE.
obj%isInitiated = .FALSE.
obj%isNodeToElementsInitiated = .FALSE.
obj%isNodeToNodesInitiated = .FALSE.
obj%isExtraNodeToNodesInitiated = .FALSE.
obj%isElementToElementsInitiated = .FALSE.
obj%isBoundaryDataInitiated = .FALSE.
obj%isFacetDataInitiated = .FALSE.
obj%uid = 0
obj%xidim = 0
obj%elemType = 0
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
IF (ALLOCATED(obj%boundingEntity)) DEALLOCATE (obj%boundingEntity)
IF (ALLOCATED(obj%local_elemNumber)) DEALLOCATE (obj%local_elemNumber)
IF (ALLOCATED(obj%Local_Nptrs)) DEALLOCATE (obj%Local_Nptrs)
IF (ALLOCATED(obj%material)) DEALLOCATE (obj%material)
IF (ALLOCATED(obj%FacetElements)) DEALLOCATE (obj%FacetElements)
IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
IF (ALLOCATED(obj%elementData)) DEALLOCATE (obj%elementData)
IF (ALLOCATED(obj%internalFacetData)) DEALLOCATE (obj%internalFacetData)
IF (ALLOCATED(obj%boundaryFacetData)) DEALLOCATE (obj%boundaryFacetData)
obj%refelem => NULL()
obj%ipType = Equidistance
CALL Deallocate (obj%quadForTime)
CALL Deallocate (obj%linTimeElemSD)
CALL Deallocate (obj%timeElemSD)
obj%quadTypeForTime = ""
obj%continuityTypeForTime = ""
obj%interpolTypeForTime = ""
obj%orderTime = 0
CALL Deallocate (obj%quadForSpace)
CALL Deallocate (obj%linSpaceElemSD)
CALL Deallocate (obj%spaceElemSD)
IF (ALLOCATED(obj%stelemsd)) DEALLOCATE (obj%stelemsd)
obj%quadTypeForSpace = ""
obj%continuityTypeForSpace = ""
obj%interpolTypeForSpace = ""
obj%orderSpace = 0
IF (ALLOCATED(obj%quadForFacet)) DEALLOCATE (obj%quadForFacet)
IF (ALLOCATED(obj%quadForFacetCell)) DEALLOCATE (obj%quadForFacetCell)
IF (ALLOCATED(obj%linFacetElemSD)) DEALLOCATE (obj%linFacetElemSD)
IF (ALLOCATED(obj%linFacetCellElemSD)) DEALLOCATE (obj%linFacetCellElemSD)
IF (ALLOCATED(obj%facetElemSD)) DEALLOCATE (obj%facetElemSD)
IF (ALLOCATED(obj%facetCellElemSD)) DEALLOCATE (obj%facetCellElemSD)
IF (ALLOCATED(obj%facetSTelemsd)) DEALLOCATE (obj%facetSTelemsd)
obj%quadTypeForFacet = ""
obj%continuityTypeForFacet = ""
obj%interpolTypeForFacet = ""
obj%orderFacet = 0
! CALL e%reset()
END PROCEDURE mesh_Deallocate

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_final
CALL obj%Deallocate()
END PROCEDURE mesh_final

END SUBMODULE ConstructorMethods
