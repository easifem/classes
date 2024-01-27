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
USE Display_Method
USE ReallocateUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL Display(msg, unitno=unitno)

CALL Display(obj%isInitiated, "Mesh object initiated: ", &
  & unitno=unitno)

IF (.NOT. obj%isInitiated) RETURN

CALL Display(obj%readFromFile, "readFromFile: ", unitno=unitno)

CALL Display(obj%isNodeToElementsInitiated, "isNodeToElementsInitiated: ", &
  & unitno=unitno)

CALL Display(obj%isNodeToNodesInitiated, "isNodeToNodesInitiated: ", &
  & unitno=unitno)

CALL Display(obj%isElementToElementsInitiated,&
  & "isElementToElementsInitiated: ", unitno=unitno)

CALL Display(obj%isBoundaryDataInitiated,  &
  & "isBoundaryDataInitiated: ", unitno=unitno)

CALL Display(obj%isFacetDataInitiated,  &
  & "isFacetDataInitiated: ", unitno=unitno)

CALL Display(obj%uid,  &
  & "uid: ", unitno=unitno)

CALL Display(obj%tElements_topology_wise, "tElements Topology wise: ",  &
  & unitno=unitno)

CALL Display(obj%tElemTopologies, "total Element Topoglies in Mesh: ",  &
  & unitno=unitno)

IF (obj%tElemTopologies .GT. 0) THEN
  CALL Display(obj%elemTopologies(1:obj%tElemTopologies),  &
    & "element Topologies in mesh: ", unitno=unitno)
END IF

CALL Display(obj%nsd, "nsd: ", unitno=unitno)

CALL Display(obj%maxNptrs, "maxNptrs: ", unitno=unitno)

CALL Display(obj%minNptrs, "minNptrs: ", unitno=unitno)

CALL Display(obj%maxElemNum, "maxElemNum: ", unitno=unitno)

CALL Display(obj%minElemNum, "minElemNum: ", unitno=unitno)

CALL Display(obj%tNodes, "tNodes: ", unitno=unitno)

CALL Display(obj%tIntNodes, "tIntNodes: ", unitno=unitno)

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

abool = ALLOCATED(obj%physicalTag)
CALL Display(abool, "physicalTag ALLOCATED: ", unitno=unitno)
IF (abool) THEN
  CALL Display(obj%physicalTag, "physicalTag: ", unitno=unitno)
END IF

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

! CALL Display(ALLOCATED(obj%facetElements),  "local_nptrs ALLOCATED: ",  &
!   & unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
CHARACTER(:), ALLOCATABLE :: dsetname
INTEGER(I4B) :: ii, dummy
LOGICAL(LGT) :: isok
INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

dsetname = TRIM(group)

isok = hdf5%isOpen()
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR]:: HDF5 file is not opened')
  RETURN
END IF

isok = hdf5%isRead()
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR]:: HDF5 file does not have read permission')
  RETURN
END IF

isok = hdf5%isGroup(dsetname)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR]:: '//dsetname//  &
    & ' is not a group; it should be a group which contains the meshEntity')
  RETURN
END IF

isok = hdf5%pathExists(dsetname)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR]:: '//dsetname//' path does not exists')
  RETURN
END IF

CALL read_scalar("uid", obj%uid)
CALL read_scalar("nsd", obj%nsd)
CALL read_scalar("tIntNodes", obj%tIntNodes)
CALL read_scalar("tElements", obj%tElements)
CALL read_scalar("minX", obj%minX)
CALL read_scalar("minY", obj%minY)
CALL read_scalar("minZ", obj%minZ)
CALL read_scalar("maxX", obj%maxX)
CALL read_scalar("maxY", obj%maxY)
CALL read_scalar("maxZ", obj%maxZ)
CALL read_scalar("x", obj%x)
CALL read_scalar("y", obj%y)
CALL read_scalar("z", obj%z)
CALL read_int_vector("physicalTag", obj%physicalTag)
CALL read_int_vector("elemNumber", elemNumber)

IF (ALLOCATED(elemNumber) .AND. SIZE(elemNumber) .NE. 0) THEN
  obj%maxElemNum = MAXVAL(elemNumber)
  obj%minElemNum = MINVAL(elemNumber)
ELSE
  obj%maxElemNum = 0
  obj%minElemNum = 0
END IF

CALL Reallocate(obj%local_elemNumber, obj%maxElemNum)

DO CONCURRENT(ii=1:obj%tElements)
  obj%local_elemNumber(elemNumber(ii)) = ii
END DO

isok = hdf5%pathExists(dsetname//"/connectivity")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR]:: '//dsetname//'/connectivity path does not exists')
  RETURN
END IF

CALL hdf5%READ(dsetname//"/connectivity", connectivity)
obj%maxNptrs = MAXVAL(connectivity)
obj%minNptrs = MINVAL(connectivity)
CALL Reallocate(obj%local_nptrs, obj%maxNptrs)

DO CONCURRENT(ii=1:obj%tElements)
  obj%local_nptrs(connectivity(:, ii)) = connectivity(:, ii)
END DO

obj%tNodes = COUNT(obj%local_nptrs .NE. 0)
dummy = 0

DO ii = 1, obj%maxNptrs
  IF (obj%local_nptrs(ii) .NE. 0) THEN
    dummy = dummy + 1
    obj%local_nptrs(ii) = dummy
  END IF
END DO

!> reading boundingEntity
! CALL Display('reading boundingEntity', stdout)
isok = hdf5%pathExists(dsetname//"/boundingEntity")
IF (isok) THEN
  CALL hdf5%READ(dsetname//"/boundingEntity", obj%boundingEntity)
END IF

IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

CONTAINS
SUBROUTINE read_scalar(fieldname, VALUE)
  CHARACTER(*), INTENT(IN) :: fieldname
  CLASS(*), INTENT(INOUT) :: VALUE

  LOGICAL(LGT) :: isok0
  CHARACTER(:), ALLOCATABLE :: astr

  astr = dsetname//"/"//fieldname
  isok0 = hdf5%pathExists(astr)
  IF (.NOT. isok0) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//astr//' path does not exists.')
    RETURN
  END IF

  SELECT TYPE (VALUE)
  TYPE is (INTEGER(I4B))
    CALL hdf5%READ(astr, VALUE)

  TYPE is (REAL(DFP))

    CALL hdf5%READ(astr, VALUE)
  END SELECT
END SUBROUTINE read_scalar

SUBROUTINE read_int_vector(fieldname, VALUE)
  CHARACTER(*), INTENT(IN) :: fieldname
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)

  LOGICAL(LGT) :: isok0
  CHARACTER(:), ALLOCATABLE :: astr

  astr = dsetname//"/"//fieldname
  isok0 = hdf5%pathExists(astr)
  IF (.NOT. isok0) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//astr//' path does not exists.')
    RETURN
  END IF
  CALL hdf5%READ(astr, VALUE)

END SUBROUTINE read_int_vector

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

IF (.NOT. hdf5%isOpen()) &
  & CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: HDF5 file is not opened')

IF (.NOT. hdf5%isRead()) &
  & CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: HDF5 file does not have read permission')

IF (.NOT. hdf5%pathExists(dsetname)) &
  & CALL e%RaiseError(modName//'::'//myName//" - "// &
  & TRIM(dsetname)//' path does not exists')

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

! CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
! LOGICAL(LGT) :: OpenTag_, CloseTag_, Content_
! INTEGER(INT8) :: vtkType
! INTEGER(INT8), ALLOCATABLE :: types(:)
! INTEGER(I4B) :: nCells, nPoints, ii, jj, nne
! INTEGER(I4B), ALLOCATABLE :: vtkIndx(:), connectivity(:), &
!   & offsets(:), localNptrs(:)
!
! ! main
! IF (.NOT. vtkFile%isInitiated) THEN
!   IF (.NOT. PRESENT(filename)) THEN
!     CALL e%RaiseError(modName//"::"//myName//" - "// &
!     & "VTKFile_ is not initiated, and filename is not present.")
!   ELSE
!     CALL vtkFile%InitiateVTKFile(filename=filename, &
!       & mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
!       & DataStructureType=VTK_UnStructuredGrid)
!   END IF
! END IF
!
! nCells = obj%GetTotalElements()
! nPoints = obj%GetTotalNodes()
! OpenTag_ = INPUT(default=.TRUE., option=OpenTag)
! CloseTag_ = INPUT(default=.TRUE., option=CloseTag)
! Content_ = INPUT(default=.TRUE., option=Content)
! ! Write piece information if OpenTag is true
! IF (OpenTag_) CALL vtkFile%WritePiece(nPoints=nPoints, nCells=nCells)
! ! Write Points information
! IF (PRESENT(nodeCoord)) THEN
!   IF (ANY(SHAPE(nodeCoord) .NE. [3, nPoints])) &
!       & CALL e%RaiseError(modName//"::"//myName//" - "// &
!       & "Shape of nodeCoord should be [3, nPoints]")
!   CALL vtkFile%WritePoints(x=nodeCoord)
! END IF
! ! Write Cells
! IF (Content_) THEN
!   CALL GetVTKelementType(elemType=obj%elemType, &
!     & vtk_type=vtkType, Nptrs=vtkIndx)
!   nne = SIZE(vtkIndx)
!   ALLOCATE (types(nCells), offsets(nCells), &
!     & connectivity(nne * nCells))
!   types = vtkType; offsets(1) = nne; jj = 0
!   DO ii = 2, nCells
!     offsets(ii) = offsets(ii - 1) + nne
!   END DO
!   DO ii = obj%minElemNum, obj%maxElemNum
!     IF (obj%isElementPresent(ii)) THEN
!       jj = jj + 1
!       localNptrs = obj%GetLocalNodeNumber( &
!         & obj%GetConnectivity(globalElement=ii))
!      connectivity(offsets(jj) - nne + 1:offsets(jj)) = localNptrs(vtkIndx) - 1
!     END IF
!   END DO
!   CALL vtkFile%WriteCells(connectivity=connectivity, offsets=offsets, &
!     & types=types)
! END IF
! IF (CloseTag_) CALL vtkFile%WritePiece()
! ! clean up
! IF (ALLOCATED(types)) DEALLOCATE (types)
! IF (ALLOCATED(vtkIndx)) DEALLOCATE (vtkIndx)
! IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
! IF (ALLOCATED(offsets)) DEALLOCATE (offsets)
! IF (ALLOCATED(localNptrs)) DEALLOCATE (localNptrs)
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                        DisplayElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayElementData
CHARACTER(*), PARAMETER :: myName = "obj_DisplayElementData()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_DisplayElementData

!----------------------------------------------------------------------------
!                                                            DisplayNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayNodeData
CHARACTER(*), PARAMETER :: myName = "obj_DisplayNodeData()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_DisplayNodeData

!----------------------------------------------------------------------------
!                                                  DisplayInternalFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayInternalFacetData
CHARACTER(*), PARAMETER :: myName = "obj_DisplayInternalFacetData()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_DisplayInternalFacetData

!----------------------------------------------------------------------------
!                                                   DisplayBoundaryFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayBoundaryFacetData
CHARACTER(*), PARAMETER :: myName = "obj_DisplayBoundaryFacetData()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_DisplayBoundaryFacetData

!----------------------------------------------------------------------------
!                                                      DisplayFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayFacetElements
CHARACTER(*), PARAMETER :: myName = "obj_DisplayFacetElements()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_DisplayFacetElements

END SUBMODULE IOMethods
