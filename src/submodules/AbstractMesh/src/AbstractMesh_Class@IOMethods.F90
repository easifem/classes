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

SUBMODULE(AbstractMesh_Class) IOMethods
USE GlobalData, ONLY: stdout
USE Display_Method
USE ReallocateUtility
! USE HDF5File_Method, ONLY: HDF5ReadScalar, HDF5ReadVector, HDF5ReadMatrix
USE HDF5File_Method, ONLY: HDF5GetEntities
USE AbstractMeshUtility, ONLY: MeshImportFromGroup, MeshImportFromDim
USE ArangeUtility
USE InputUtility
USE ReferenceElement_Method, ONLY: ElementName
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool
INTEGER(I4B) :: ii

CALL Display(msg, unitno=unitno)

CALL Display(obj%isInitiated, "Mesh object initiated: ", &
  & unitno=unitno)

IF (.NOT. obj%isInitiated) RETURN

CALL Display(obj%showTime, "showTime: ", unitno=unitno)

CALL Display(obj%readFromFile, "readFromFile: ", unitno=unitno)

CALL Display(obj%isNodeToElementsInitiated, "isNodeToElementsInitiated: ", &
  & unitno=unitno)

CALL Display(obj%isNodeToNodesInitiated, "isNodeToNodesInitiated: ", &
  & unitno=unitno)

CALL Display(obj%isElementToElementsInitiated,&
  & "isElementToElementsInitiated: ", unitno=unitno)

CALL Display(obj%isBoundaryDataInitiated,  &
  & "isBoundaryDataInitiated: ", unitno=unitno)

CALL Display(obj%isFacetDataInitiated, "isFacetDataInitiated: ",  &
  & unitno=unitno)

CALL Display(obj%uid, "uid: ", unitno=unitno)

CALL Display("Total elements (topology wise)", unitno=unitno)
CALL EqualLine(unitno=unitno)
CALL Display(obj%tElements_topology_wise(1), "point: ", &
  & unitno=unitno)
CALL Display(obj%tElements_topology_wise(2), "line: ", &
  & unitno=unitno)
CALL Display(obj%tElements_topology_wise(3), "triangle: ", &
  & unitno=unitno)
CALL Display(obj%tElements_topology_wise(4), "quadrangle: ",&
  & unitno=unitno)
CALL Display(obj%tElements_topology_wise(5), "tetrahedron: ",&
  & unitno=unitno)
CALL Display(obj%tElements_topology_wise(6), "hexahedron: ", &
  & unitno=unitno)
CALL Display(obj%tElements_topology_wise(7), "prism: ",  &
  & unitno=unitno)
CALL Display(obj%tElements_topology_wise(8), "pyramid: ",  &
  & unitno=unitno)
CALL BlankLines(unitno=unitno)

CALL Display(obj%tElemTopologies, "Total topologies: ",  &
  & unitno=unitno)

DO ii = 1, obj%tElemTopologies
  CALL Display("  Topologies("//tostring(ii)//"): "//  &
    & ElementName(obj%elemTopologies(ii)), unitno=unitno)
END DO

CALL Display(obj%nsd, "nsd: ", unitno=unitno)
CALL Display(obj%xidim, "xidim: ", unitno=unitno)

CALL Display(obj%maxNptrs, "maxNptrs: ", unitno=unitno)

CALL Display(obj%minNptrs, "minNptrs: ", unitno=unitno)

CALL Display(obj%maxElemNum, "maxElemNum: ", unitno=unitno)

CALL Display(obj%minElemNum, "minElemNum: ", unitno=unitno)

CALL Display(obj%tNodes, "tNodes: ", unitno=unitno)
CALL Display(obj%tEdges, "tEdges: ", unitno=unitno)
CALL Display(obj%tFaces, "tFaces: ", unitno=unitno)

CALL Display(obj%tElements, "tElements: ", unitno=unitno)

CALL Display(obj%minX, "minX: ", unitno=unitno)

CALL Display(obj%maxX, "maxX: ", unitno=unitno)

CALL Display(obj%minY, "minY: ", unitno=unitno)

CALL Display(obj%maxY, "maxY: ", unitno=unitno)

CALL Display(obj%minZ, "minZ: ", unitno=unitno)

CALL Display(obj%maxZ, "maxZ: ", unitno=unitno)

CALL Display(obj%X, "X: ", unitno=unitno)

CALL Display(obj%Y, "Y: ", unitno=unitno)

CALL Display(obj%Z, "Z: ", unitno=unitno)

abool = ALLOCATED(obj%material)
CALL Display(abool, "materialALLOCATED: ", unitno=unitno)
IF (abool) THEN
  CALL Display(obj%material, "material: ", unitno=unitno)
END IF

abool = ALLOCATED(obj%boundingEntity)
CALL Display(abool, "boundingEntity ALLOCATED: ", unitno=unitno)
IF (abool) THEN
  CALL Display(obj%boundingEntity, "boundingEntity: ", unitno=unitno)
END IF

abool = ALLOCATED(obj%local_elemNumber)
CALL Display(abool, "local_elemNumber ALLOCATED: ", unitno=unitno)

abool = ALLOCATED(obj%local_nptrs)
CALL Display(abool, "local_nptrs ALLOCATED: ", unitno=unitno)

abool = ALLOCATED(obj%facetElementType)
CALL Display(abool, "facetElementType ALLOCATED: ", unitno=unitno)

abool = ALLOCATED(obj%nodeData)
CALL Display(abool, "nodeData ALLOCATED: ", unitno=unitno)

abool = ALLOCATED(obj%elementData)
CALL Display(abool, "elementData ALLOCATED: ", unitno=unitno)

abool = ALLOCATED(obj%internalFacetData)
CALL Display(abool, "internalFacetData ALLOCATED: ", unitno=unitno)

abool = ALLOCATED(obj%boundaryFacetData)
CALL Display(abool, "boundaryFacetData ALLOCATED: ", unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
LOGICAL(LGT) :: cases(3), isArg(3)
INTEGER(I4B), ALLOCATABLE :: entities0(:)
CHARACTER(:), ALLOCATABLE :: group0
INTEGER(I4B) :: tEntities

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

group0 = input(option=group, default="")

CALL obj%DEALLOCATE()

isArg = [PRESENT(group), PRESENT(dim), PRESENT(entities)]

cases(1) = (.NOT. isArg(2)) .AND. (.NOT. isArg(3))
cases(2) = ALL(isArg(2:3))
cases(3) = isArg(2) .AND. (.NOT. isArg(3))

IF (cases(1)) THEN
  CALL MeshImportFromGroup(obj, hdf5, group0)

ELSEIF (cases(2)) THEN
  CALL MeshImportFromDim(obj, hdf5, group0, dim, entities, SIZE(entities))

ELSEIF (cases(3)) THEN
  CALL HDF5GetEntities(hdf5=hdf5, group=group0, dim=dim,  &
    & tEntities=tEntities, myName=myName, modName=modName)
  IF (tEntities .GT. 0_I4B) THEN
    entities0 = arange(1_I4B, tEntities)
    CALL MeshImportFromDim(obj, hdf5, group0, dim, entities0, tEntities)
  END IF

ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found')
  RETURN
END IF

IF (ALLOCATED(entities0)) DEALLOCATE (entities0)
group0 = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                              GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord()"
CHARACTER(:), ALLOCATABLE :: dsetname
INTEGER(I4B) :: ii, jj
REAL(DFP), ALLOCATABLE :: xij(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

! main
dsetname = TRIM(group)

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file does not have read permission')
END IF

IF (.NOT. hdf5%pathExists(dsetname)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: '//dsetname//' path does not exists')
END IF

! build nodeCoord
CALL hdf5%READ(dsetname, xij)
CALL Reallocate(nodeCoord, 3_I4B, obj%GetTotalNodes())
jj = SIZE(xij, 1)
DO ii = 1, SIZE(nodeCoord, 2)
  nodeCoord(1:jj, ii) = xij(1:jj, obj%GetGlobalNodeNumber(ii))
END DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_GetNodeCoord

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[WIP]: This routine has not been implemented yet.")
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                        DisplayElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayElementData
INTEGER(I4B) :: ii, telements

CALL Display(msg, unitno=unitno)
telements = obj%GetTotalElements()

DO ii = 1, telements
  CALL elemData_Display(obj=obj%elementData(ii),  &
    & msg="elementData("//tostring(ii)//"): ", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END DO
END PROCEDURE obj_DisplayElementData

!----------------------------------------------------------------------------
!                                                            DisplayNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayNodeData
INTEGER(I4B) :: ii, tNodes
tNodes = obj%GetTotalNodes()
CALL Display(msg, unitno=unitno)
DO ii = 1, tNodes
  CALL nodeData_Display(obj%nodeData(ii),  &
    & msg="nodeData("//tostring(ii)//"): ", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END DO
END PROCEDURE obj_DisplayNodeData

!----------------------------------------------------------------------------
!                                                  DisplayInternalFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayInternalFacetData
INTEGER(I4B) :: ii, n
LOGICAL(LGT) :: abool

CALL Display(msg, unitno=unitno)

abool = ALLOCATED(obj%internalFacetData)
IF (abool) THEN; n = SIZE(obj%internalFacetData); ELSE; n = 0; END IF

CALL Display(abool, "internalFacetData ALLOCATED: ", unitno=unitno)

DO ii = 1, n

  CALL InternalFacetData_Display(obj=obj%internalFacetData(ii),  &
    & msg="internalFacetData("//tostring(ii)//"): ", unitno=unitno)

  CALL BlankLines(nol=1, unitno=unitno)

END DO
END PROCEDURE obj_DisplayInternalFacetData

!----------------------------------------------------------------------------
!                                                   DisplayBoundaryFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayBoundaryFacetData
INTEGER(I4B) :: ii, n
LOGICAL(LGT) :: abool

abool = ALLOCATED(obj%boundaryFacetData)
IF (abool) THEN; n = SIZE(obj%boundaryFacetData); ELSE; n = 0; END IF

CALL Display(msg, unitno=unitno)
CALL Display(abool, "boundaryFacetData ALLOCATED: ", unitno=unitno)

DO ii = 1, n

  CALL BoundaryFacetData_Display(obj=obj%boundaryFacetData(ii),  &
    & msg="boundaryFacetData("//tostring(ii)//"): ", unitno=unitno)

  CALL BlankLines(nol=1, unitno=unitno)

END DO
END PROCEDURE obj_DisplayBoundaryFacetData

!----------------------------------------------------------------------------
!                                                      DisplayFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayFacetElements
CHARACTER(*), PARAMETER :: myName = "obj_DisplayFacetElements()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_DisplayFacetElements

!----------------------------------------------------------------------------
!                                                           DisplayMeshInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayMeshInfo
CALL Display(msg, unitno=unitno)
CALL EqualLine(unitno=unitno)
CALL Display(obj%GetTotalNodes(), "total nodes: ", unitno=unitno)
CALL Display(obj%GetTotalElements(), "total elements: ", unitno=unitno)
CALL EqualLine(unitno=unitno)
END PROCEDURE obj_DisplayMeshInfo

END SUBMODULE IOMethods
