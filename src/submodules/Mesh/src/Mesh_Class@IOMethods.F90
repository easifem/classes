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

SUBMODULE(Mesh_Class) IOMethods
USE Display_Method
USE ReallocateUtility
USE ReferenceElement_Method
USE InputUtility
USE HDF5File_Method, ONLY: HDF5ReadScalar, HDF5ReadVector,  &
  & HDF5ReadMatrix

USE NodeData_Class, ONLY: NodeData_Display
USE ElemData_Class, ONLY: ElemData_Display
USE FacetData_Class, ONLY: BoundaryFacetData_Display,  &
& InternalFacetData_Display
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL AbstractMeshDisplay(obj=obj, msg=msg, unitno=unitno)

CALL Display(obj%elemType, "elemType: ", unitno=unitno)
abool = ASSOCIATED(obj%refElem)
CALL Display(abool, "refElem ASSOCIATED: ", unitno=unitno)
abool = ALLOCATED(obj%facetElements)
CALL Display(abool, "facetElements ALLOCATED: ", unitno=unitno)

abool = ALLOCATED(obj%material)
CALL Display(abool, "materialALLOCATED: ", unitno=unitno)
IF (abool) THEN
  CALL Display(obj%material, "material: ", unitno=unitno)
END IF
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
CHARACTER(:), ALLOCATABLE :: dsetname
LOGICAL(LGT) :: isok
INTEGER(I4B) :: temp4(4)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

dsetname = TRIM(group)
CALL AbstractMeshImport(obj=obj, hdf5=hdf5, group=group)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%elemType, group=dsetname,  &
  & fieldname="elemType", myname=myname, modname=modname, check=.TRUE.)

obj%refelem => ReferenceElement_Pointer(xidim=obj%xidim, &
  & nsd=obj%nsd, elemType=obj%elemType, ipType=Equidistance)

isok = obj%xidim .GT. 0
IF (isok) THEN
  temp4 = TotalEntities(obj%elemType)
  SELECT CASE (ElementTopology(obj%elemType))
  CASE (Line)
    temp4(1) = 2
  END SELECT
  ALLOCATE (obj%facetElements(temp4(obj%xidim)))
  CALL GetFacetElements(refelem=obj%refelem, ans=obj%facetElements)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
LOGICAL(LGT) :: OpenTag_, CloseTag_, Content_
INTEGER(INT8) :: vtkType
INTEGER(INT8), ALLOCATABLE :: types(:)
INTEGER(I4B) :: nCells, nPoints, ii, jj, nne
INTEGER(I4B), ALLOCATABLE :: vtkIndx(:), connectivity(:), &
  & offsets(:), localNptrs(:)

! main
IF (.NOT. vtkFile%isInitiated) THEN
  IF (.NOT. PRESENT(filename)) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "VTKFile_ is not initiated, and filename is not present.")
  ELSE
    CALL vtkFile%InitiateVTKFile(filename=filename, &
      & mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
      & DataStructureType=VTK_UnStructuredGrid)
  END IF
END IF

nCells = obj%GetTotalElements()
nPoints = obj%GetTotalNodes()
OpenTag_ = INPUT(default=.TRUE., option=OpenTag)
CloseTag_ = INPUT(default=.TRUE., option=CloseTag)
Content_ = INPUT(default=.TRUE., option=Content)

! Write piece information if OpenTag is true
IF (OpenTag_) CALL vtkFile%WritePiece(nPoints=nPoints, nCells=nCells)

!  Write Points information
IF (PRESENT(nodeCoord)) THEN
  IF (ANY(SHAPE(nodeCoord) .NE. [3, nPoints])) &
      & CALL e%RaiseError(modName//"::"//myName//" - "// &
      & "Shape of nodeCoord should be [3, nPoints]")
  CALL vtkFile%WritePoints(x=nodeCoord)
END IF

! Write Cells
IF (Content_) THEN
  CALL GetVTKelementType(elemType=obj%elemType, vtk_type=vtkType,  &
  & Nptrs=vtkIndx)

  nne = SIZE(vtkIndx)

  ALLOCATE (types(nCells), offsets(nCells), connectivity(nne * nCells))

  types = vtkType; offsets(1) = nne; jj = 0

  DO ii = 2, nCells
    offsets(ii) = offsets(ii - 1) + nne
  END DO

  DO ii = obj%minElemNum, obj%maxElemNum
    IF (obj%isElementPresent(ii)) THEN
      jj = jj + 1
      localNptrs = obj%GetLocalNodeNumber( &
        & obj%GetConnectivity(globalElement=ii))
     connectivity(offsets(jj) - nne + 1:offsets(jj)) = localNptrs(vtkIndx) - 1
    END IF
  END DO

  CALL vtkFile%WriteCells(connectivity=connectivity, offsets=offsets, &
    & types=types)
END IF
IF (CloseTag_) CALL vtkFile%WritePiece()
! clean up
IF (ALLOCATED(types)) DEALLOCATE (types)
IF (ALLOCATED(vtkIndx)) DEALLOCATE (vtkIndx)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(offsets)) DEALLOCATE (offsets)
IF (ALLOCATED(localNptrs)) DEALLOCATE (localNptrs)
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                      DisplayFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayFacetElements
INTEGER(I4B) :: ii, n
LOGICAL(LGT) :: abool

abool = ALLOCATED(obj%facetElements)
IF (abool) THEN; n = SIZE(obj%facetElements); ELSE; n = 0; END IF

CALL Display(msg, unitno=unitno)
CALL Display(abool, "boundaryFacetData ALLOCATED: ", unitno=unitno)

DO ii = 1, n

  CALL Display(obj%facetElements(ii), &
    & "obj%facetElements("//tostring(ii)//"): ", unitno=unitno)

  CALL BlankLines(nol=1, unitno=unitno)

END DO
END PROCEDURE obj_DisplayFacetElements

END SUBMODULE IOMethods
