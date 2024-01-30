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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL AbstractMeshDisplay(obj=obj, msg=msg, unitno=unitno)

CALL Display(obj%xidim, "xidim: ", unitno=unitno)
CALL Display(obj%elemType, "elemType: ", unitno=unitno)

abool = ALLOCATED(obj%facetElements)
CALL Display(abool, "facetElements ALLOCATED: ", unitno=unitno)

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

abool = ASSOCIATED(obj%refElem)
CALL Display(abool, "refElem ASSOCIATED: ", unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:),  &
  & internalNptrs(:)
CHARACTER(:), ALLOCATABLE :: dsetname
INTEGER(I4B) :: ii, dummy, jj
LOGICAL(LGT) :: isok, abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL obj%DEALLOCATE()

dsetname = TRIM(group)
CALL AbstractMeshImport(obj=obj, hdf5=hdf5, group=group)

CALL read_scalar("xidim", obj%xidim)
CALL read_scalar("elemType", obj%elemType)

IF (obj%tElements .NE. 0) THEN
  ALLOCATE (obj%elementData(obj%tElements))
END IF

CALL read_int_vector("elemNumber", elemNumber)

DO CONCURRENT(ii=1:obj%tElements)
  obj%elementData(ii)%globalElemNum = elemNumber(ii)
  obj%elementData(ii)%localElemNum = ii
END DO

! CALL Display('reading connectivity', stdout)
isok = hdf5%pathExists(TRIM(dsetname)//"/connectivity")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR]:: '//dsetname//'/connectivity path does not exists')
  RETURN
END IF

CALL hdf5%READ(TRIM(dsetname)//"/connectivity", connectivity)
isok = (obj%elemType .EQ. Point1) .OR. (obj%elemType .EQ. 0)
IF (isok) THEN
  obj%tNodes = 1
  IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
  ALLOCATE (obj%nodeData(obj%tNodes))
  obj%nodeData(1)%globalNodeNum = 1
  obj%nodeData(1)%localNodeNum = 1
  obj%nodeData(1)%nodeType = BOUNDARY_NODE
ELSE
  obj%maxNptrs = MAXVAL(connectivity)
  obj%minNptrs = MINVAL(connectivity)
  CALL Reallocate(obj%local_Nptrs, obj%maxNptrs)

  DO CONCURRENT(ii=1:obj%tElements)
    obj%elementData(ii)%globalNodes = connectivity(:, ii)
    obj%local_Nptrs(connectivity(:, ii)) = connectivity(:, ii)
  END DO

  obj%tNodes = COUNT(obj%local_Nptrs .NE. 0)
  IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
  ALLOCATE (obj%nodeData(obj%tNodes))
  dummy = 0

  DO ii = 1, obj%maxNptrs
    IF (obj%local_Nptrs(ii) .NE. 0) THEN
      dummy = dummy + 1
      obj%nodeData(dummy)%globalNodeNum = obj%local_Nptrs(ii)
      obj%nodeData(dummy)%localNodeNum = dummy
      obj%nodeData(dummy)%nodeType = BOUNDARY_NODE
      ! The above step is unusual, but we know the position of
      ! internal nptrs, so later we will set the
      ! those nodes as INTERNAL_NODE, in this way we can
      ! identify the boundary nodes
      obj%local_Nptrs(ii) = dummy
    END IF
  END DO

END IF

!> reading internalNptrs, nodeData%globalNodeNumber,
!> nodeData%localNodeNumber, nodeData%nodeType
!> mark INTERNAL_NODE

isok = hdf5%pathExists(TRIM(dsetname)//"/intNodeNumber")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR]:: '//TRIM(dsetname)// &
    & '/intNodeNumber path does not exists')
  RETURN
END IF

CALL hdf5%READ(TRIM(dsetname)//"/intNodeNumber", internalNptrs)
abool = obj%elemType .EQ. Point1 .OR. obj%elemType .EQ. 0
IF (abool) THEN
  obj%nodeData(1)%globalNodeNum = internalNptrs(1)
  obj%nodeData(1)%nodeType = INTERNAL_NODE
ELSE
  DO ii = 1, SIZE(internalNptrs)
    jj = obj%GetLocalNodeNumber(internalNptrs(ii))
    obj%nodeData(jj)%globalNodeNum = internalNptrs(ii)
    obj%nodeData(jj)%nodeType = INTERNAL_NODE
  END DO
END IF

isok = obj%tElements .GT. 0
abool = obj%xidim .GT. 0
IF (isok) THEN
  obj%refelem => ReferenceElement_Pointer(xidim=obj%xidim, &
    & nsd=obj%nsd, elemType=obj%elemType, ipType=Equidistance)

  IF (abool) obj%facetElements = FacetElements(obj%refelem)
END IF

IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(internalNptrs)) DEALLOCATE (internalNptrs)

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
