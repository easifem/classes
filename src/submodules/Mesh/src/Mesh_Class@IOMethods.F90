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

MODULE PROCEDURE mesh_display
  IF (.NOT. obj%isInitiated) THEN
    CALL Display("Mesh object is empty, noting to display", msg, &
      & unitno=unitno)
  END IF
  CALL Display(msg, unitno=unitno)
  CALL Display(obj%readFromFile, "# readFromFile : ", unitno=unitno)
  CALL Display(obj%isNodeToElementsInitiated,  &
    & "# isNodeToElementsInitiated : ", unitno=unitno)
  CALL Display(obj%isNodeToNodesInitiated,  &
    & "# isNodeToNodesInitiated : ", unitno=unitno)
  CALL Display(obj%isElementToElementsInitiated,  &
    & "# isElementToElementsInitiated : ", unitno=unitno)
  CALL Display(obj%isBoundaryDataInitiated,  &
    & "# isBoundaryDataInitiated : ", unitno=unitno)
  CALL Display(obj%uid,  &
    & "# uid : ", unitno=unitno)
  CALL Display(obj%xidim,  &
    & "# xidim : ", unitno=unitno)
  CALL Display(obj%elemType,  &
    & "# elemType : ", unitno=unitno)
  CALL Display(obj%nsd,  &
    & "# nsd : ", unitno=unitno)
  CALL Display(obj%maxNptrs,  &
    & "# maxNptrs : ", unitno=unitno)
  CALL Display(obj%minNptrs,  &
    & "# minNptrs : ", unitno=unitno)
  CALL Display(obj%maxElemNum,  &
    & "# maxElemNum : ", unitno=unitno)
  CALL Display(obj%minElemNum,  &
    & "# minElemNum : ", unitno=unitno)
  CALL Display(obj%tNodes,  &
    & "# tNodes : ", unitno=unitno)
  CALL Display(obj%tIntNodes,  &
    & "# tIntNodes : ", unitno=unitno)
  CALL Display(obj%tElements,  &
    & "# tElements : ", unitno=unitno)
  CALL Display(obj%minX,  &
    & "# minX : ", unitno=unitno)
  CALL Display(obj%maxX,  &
    & "# maxX : ", unitno=unitno)
  CALL Display(obj%minY,  &
    & "# minY : ", unitno=unitno)
  CALL Display(obj%maxY,  &
    & "# maxY : ", unitno=unitno)
  CALL Display(obj%minZ,  &
    & "# minZ : ", unitno=unitno)
  CALL Display(obj%maxZ,  &
    & "# maxZ : ", unitno=unitno)
  CALL Display(obj%X,  &
    & "# X : ", unitno=unitno)
  CALL Display(obj%Y,  &
    & "# Y : ", unitno=unitno)
  CALL Display(obj%Z,  &
    & "# Z : ", unitno=unitno)
  IF (ALLOCATED(obj%physicalTag)) THEN
    CALL Display(obj%physicalTag,  &
      & "# physicalTag : ", unitno=unitno)
  ELSE
    CALL Display("# physicalTag : NOT ALLOCATED", &
      & unitno=unitno)
  END IF
  IF (ALLOCATED(obj%physicalTag)) THEN
    CALL Display(obj%physicalTag,  &
      & "# physicalTag : ", unitno=unitno)
  ELSE
    CALL Display("# physicalTag : NOT ALLOCATED", &
      & unitno=unitno)
  END IF
  IF (ALLOCATED(obj%Local_Nptrs)) THEN
    CALL Display(obj%Local_Nptrs,  &
      & "# Local_Nptrs : ", unitno=unitno)
  ELSE
    CALL Display("# Local_Nptrs : NOT ALLOCATED", &
      & unitno=unitno)
  END IF
  IF (ASSOCIATED(obj%refElem)) THEN
    CALL Display("# refElem : ASSOCIATED", unitno=unitno)
  ELSE
    CALL Display("# refElem : NOT ASSOCIATED", &
      & unitno=unitno)
  END IF
  IF (ALLOCATED(obj%nodeData)) THEN
    CALL Display("# nodeData : ALLOCATED", unitno=unitno)
  ELSE
    CALL Display("# nodeData : NOT ALLOCATED", &
      & unitno=unitno)
  END IF
  IF (ALLOCATED(obj%elementData)) THEN
    CALL Display("# elementData : ALLOCATED", unitno=unitno)
  ELSE
    CALL Display("# elementData : NOT ALLOCATED", &
      & unitno=unitno)
  END IF
END PROCEDURE mesh_display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Import
  CHARACTER(LEN=*), PARAMETER :: myName = "mesh_Import"
  INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:), &
    & InternalNptrs(:)
  TYPE(String) :: dsetname
  INTEGER(I4B) :: ii, dummy, jj

  dsetname = trim(group)
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'Making dsetname')
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'dsetname = '//TRIM(dsetname))
  !>check
  IF (.NOT. hdf5%isOpen()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'HDF5 file is not opened')
  END IF
  !>check
  IF (.NOT. hdf5%isRead()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'HDF5 file does not have read permission')
  END IF
  !>check
  IF (.NOT. hdf5%isGroup(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname) // ' is not a group; it should be a group which &
      & contains the meshEntity' )
  END IF
  !>check
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//' path does not exists')
  END IF
  !> read Uid
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/uid")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/uid"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/uid", obj%Uid)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'uid = '//TRIM(str(obj%Uid, .true.)))
  END IF
  !> read xidim
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/xidim")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/xidim"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/xidim", obj%xidim)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'xidim = '//TRIM(str(obj%xidim, .true.)))
  END IF
  !>reading elemtype
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/elemType")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/elemType"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/elemType", obj%elemType)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'elemType = '//TRIM(str(obj%elemType, .true.)))
  END IF
  !>reading nsd
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/nsd")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/nsd"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/nsd", obj%nsd)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'nsd = '//TRIM(str(obj%elemType, .true.)))
  END IF
  !> reading tIntNodes
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/tIntNodes")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/tIntNodes"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/tIntNodes", &
      & obj%tIntNodes)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'tIntNodes = '//TRIM(str(obj%tIntNodes, .true.)))
  END IF
  !> reading tElements, allocate obj%elementData
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/tElements")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/tElements"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/tElements", &
      & obj%tElements)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'tElements = '//TRIM(str(obj%tElements, .true.)))
  END IF
  IF (ALLOCATED(obj%elementData)) DEALLOCATE (obj%elementData)
  IF (obj%tElements .NE. 0) &
    & ALLOCATE (obj%elementData(obj%tElements))
  !>reading minX
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/minX")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/minX"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/minX", obj%minX)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'minX = '//TRIM(str(obj%minX, .true.)))
  END IF
  !> reading minY
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/minY")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/minY"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/minY", obj%minY)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'minY = '//TRIM(str(obj%minY, .true.)))
  END IF
  !> reading minZ
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/minZ")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/minZ"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/minZ", obj%minZ)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'minZ = '//TRIM(str(obj%minZ, .true.)))
  END IF
  !> reading maxX
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/maxX")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/maxX"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/maxX", obj%maxX)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'maxX = '//TRIM(str(obj%maxX, .true.)))
  END IF
  !> reading maxY
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/maxY")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/maxY"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/maxY", obj%maxY)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'maxY = '//TRIM(str(obj%maxY, .true.)))
  END IF
  !> reading maxZ
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/maxZ")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/maxZ"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/maxZ", obj%minZ)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'maxZ = '//TRIM(str(obj%maxZ, .true.)))
  END IF
  !> reading x
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/x")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/x"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/x", obj%x)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'x = '//TRIM(str(obj%x, .true.)))
  END IF
  !> reading y
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/y")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/y"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/y", obj%y)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'y = '//TRIM(str(obj%y, .true.)))
  END IF
  !> reading z
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/z")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/z"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/z", obj%z)
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'z = '//TRIM(str(obj%z, .true.)))
  END IF
  !> reading physicalTag
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'reading physicalTag')
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/physicalTag")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/physicalTag"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/physicalTag", &
      & obj%physicalTag)
    IF (e%isLogActive()) THEN
      CALL e%raiseInformation(modName//'::'//myName//" - ")
      CALL Display(obj%physicalTag, "physicalTag = ", &
        & unitno=e%getLogFileUnit())
    END IF
  END IF
  !> Reading elemNumber, maxElemNum, minElemNum, local_elemNumber,
  !> elementData%globalElemNum, elementData%localElemNum
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'reading elemNumber')
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/elemNumber")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/elemNumber"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/elemNumber", &
      & elemNumber)
    IF (e%isLogActive()) THEN
      CALL e%raiseInformation(modName//'::'//myName//" - ")
      CALL Display(elemNumber, "elemNumber = ", &
        & unitno=e%getLogFileUnit())
    END IF
    IF (ALLOCATED(elemNumber) .AND. SIZE(elemNumber) .NE. 0) THEN
      obj%maxElemNum = MAXVAL(elemNumber)
      obj%minElemNum = MINVAL(elemNumber)
    END IF
    CALL Reallocate(obj%local_elemNumber, obj%maxElemNum)
    DO CONCURRENT(ii=1:obj%tElements)
      obj%local_elemNumber(elemNumber(ii)) = ii
      obj%elementData(ii)%globalElemNum = elemNumber(ii)
      obj%elementData(ii)%localElemNum = ii
    END DO
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'maxElemNum = '//trim(str(obj%maxElemNum, .TRUE.)))
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'minElemNum = '//trim(str(obj%minElemNum, .TRUE.)))
  END IF
  !> reading connectivity, maxNptrs, minNptrs, tNodes
  !> elementData%globalNodes, local_nptrs,
  !> nodeData%globalNodenum,  nodeData%localNodenum
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'reading connectivity')
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/connectivity")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/connectivity"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/connectivity", connectivity)
    IF (obj%elemType .EQ. Point1 .OR. obj%elemType .EQ. 0) THEN
      obj%tNodes = 1
      IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
      ALLOCATE (obj%nodeData(obj%tNodes))
      obj%nodeData(1)%globalNodeNum = 1
      obj%nodeData(1)%localNodeNum = 1
      obj%nodeData(1)%nodeType = BOUNDARY_NODE
    ELSE
      obj%maxNptrs = MAXVAL(connectivity)
      obj%minNptrs = MINVAL(connectivity)
      CALL Reallocate(obj%Local_Nptrs, obj%maxNptrs)
      DO CONCURRENT(ii=1:obj%tElements)
        obj%elementData(ii)%globalNodes = connectivity(:, ii)
        obj%Local_Nptrs(connectivity(:, ii)) = connectivity(:, ii)
      END DO
      obj%tNodes = COUNT(obj%Local_Nptrs .NE. 0)
      IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
      ALLOCATE (obj%nodeData(obj%tNodes))
      dummy = 0
      DO ii = 1, obj%maxNptrs
        IF (obj%Local_Nptrs(ii) .NE. 0) THEN
          dummy = dummy + 1
          obj%nodeData(dummy)%globalNodeNum = obj%Local_Nptrs(ii)
          obj%nodeData(dummy)%localNodeNum = dummy
          obj%nodeData(dummy)%nodeType = BOUNDARY_NODE
              !! The above step is unusual, but we know the position of
              !! internal nptrs, so later we will set the
              !! those nodes as INTERNAL_NODE, in this way we can
              !! identify the boundary nodes
          obj%Local_Nptrs(ii) = dummy
        END IF
      END DO
    END IF
    IF (e%isLogActive()) THEN
      CALL e%raiseInformation(modName//'::'//myName//" - "// &
        & 'connectivity(1:NNS, 1:tElements) is given below = ')
      DO ii = 1, SIZE(connectivity, 1)
        CALL DISP(title="Node-"//trim(str(ii, .true.)), &
          & x=connectivity(ii, :), &
          & unit=e%getLogFileUnit(), style="UNDERLINE", &
          & advance="NO")
      END DO
      CALL DISP(title='', x='', unit=e%getLogFileUnit(), &
        & advance="DOUBLE")
    END IF
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
        & 'maxNptrs = '//trim(str(obj%maxNptrs, .TRUE.)))
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
        & 'minNptrs = '//trim(str(obj%minNptrs, .TRUE.)))
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
        & 'tNodes = '//trim(str(obj%tNodes, .TRUE.)))
  END IF
  !> reading InternalNptrs, nodeData%globalNodeNumber,
  !> nodeData%localNodeNumber, nodeData%nodeType
  !> mark INTERNAL_NODE
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'reading InternalNptrs')
  IF (.NOT. hdf5%pathExists(TRIM(dsetname)//"/intNodeNumber")) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(dsetname)//"/intNodeNumber"//' path does not exists')
  ELSE
    CALL hdf5%read(TRIM(dsetname)//"/intNodeNumber", InternalNptrs)
    IF (e%isLogActive()) THEN
      CALL e%raiseInformation(modName//'::'//myName//" - ")
      CALL Display(InternalNptrs, "InternalNptrs = ", &
        & unitno=e%getLogFileUnit())
    END IF
    IF (obj%elemType .EQ. Point1 .OR. obj%elemType .EQ. 0) THEN
      obj%nodeData(1)%globalNodeNum = InternalNptrs(1)
      obj%nodeData(1)%nodeType = INTERNAL_NODE
    ELSE
      DO ii = 1, SIZE(InternalNptrs)
        jj = obj%getLocalNodeNumber(InternalNptrs(ii))
        obj%nodeData(jj)%globalNodeNum = InternalNptrs(ii)
        obj%nodeData(jj)%nodeType = INTERNAL_NODE
      END DO
    END IF
  END IF
  !> reading boundingEntity
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'reading boundingEntity')
  IF (hdf5%pathExists(TRIM(dsetname)//"/boundingEntity")) THEN
    CALL hdf5%read(TRIM(dsetname)//"/boundingEntity", &
      & obj%boundingEntity)
    IF (e%isLogActive()) THEN
      CALL e%raiseInformation(modName//'::'//myName//" - ")
      CALL Display(obj%boundingEntity, "boundingEntity = ", &
        & unitno=e%getLogFileUnit())
    END IF
  END IF
  !> set Reference Element
  CALL e%raiseInformation(modName//'::'//myName//" - " &
    & //"setting reference element")
  obj%refelem => ReferenceElement_Pointer(xidim=obj%xidim, nsd=obj%nsd, &
    & elemType=obj%elemType)
  IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
  IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
  IF (ALLOCATED(InternalNptrs)) DEALLOCATE (InternalNptrs)
END PROCEDURE mesh_Import

!----------------------------------------------------------------------------
!                                                              getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeCoord
  CHARACTER(LEN=*), PARAMETER :: myName = "mesh_getNodeCoord"
  TYPE(String) :: dsetname
  INTEGER(I4B) :: ii, jj
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  !!
  !! main
  !!
  dsetname = TRIM(group)
  !!
  !! check
  !!
  IF (.NOT. hdf5%isOpen()) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  !!
  !! check
  !!
  IF (.NOT. hdf5%isRead()) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have read permission')
  !!
  !! check
  !!
  IF (.NOT. hdf5%pathExists(dsetname%chars())) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & TRIM(dsetname)//' path does not exists')
  !!
  !! build nodeCoord
  !!
  CALL hdf5%read(dsetname%chars(), xij)
  CALL Reallocate(nodeCoord, 3_I4B, obj%getTotalNodes())
  jj = SIZE(xij, 1)
  DO ii = 1, SIZE(nodeCoord, 2)
    nodeCoord(1:jj, ii) = xij(1:jj, obj%getGlobalNodeNumber(ii))
  END DO
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  !!
END PROCEDURE mesh_getNodeCoord

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Export
  CHARACTER(LEN=*), PARAMETER :: myName = "mesh_Export"
  CALL e%raiseError(modName//"::"//myName//" - "// &
      & "This routine has not been implemented yet.")
END PROCEDURE mesh_Export

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_ExportToVTK
  CHARACTER(LEN=*), PARAMETER :: myName = "mesh_ExportToVTK"
  LOGICAL(LGT) :: OpenTag_, CloseTag_, Content_
  INTEGER(Int8) :: vtkType
  INTEGER(Int8), ALLOCATABLE :: types(:)
  INTEGER(I4B) :: nCells, nPoints, ii, jj, nne
  INTEGER(I4B), ALLOCATABLE :: vtkIndx(:), connectivity(:), &
    & offsets(:), localNptrs(:)
  !! main
  IF (.NOT. vtkFile%isInitiated) THEN
    IF (.NOT. PRESENT(filename)) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
      & "VTKFile_ is not initiated, and filename is not present.")
    ELSE
      CALL vtkFile%InitiateVTKFile(filename=filename, &
        & mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
        & DataStructureType=VTK_UnStructuredGrid)
    END IF
  END IF
  !!
  nCells = obj%getTotalElements()
  nPoints = obj%getTotalNodes()
  OpenTag_ = INPUT(default=.TRUE., option=OpenTag)
  CloseTag_ = INPUT(default=.TRUE., option=CloseTag)
  Content_ = INPUT(default=.TRUE., option=Content)
  !! Write piece information if OpenTag is true
  IF (OpenTag_) CALL vtkFile%WritePiece(nPoints=nPoints, nCells=nCells)
  !! Write Points information
  IF (PRESENT(nodeCoord)) THEN
    IF (ANY(SHAPE(nodeCoord) .NE. [3, nPoints])) &
        & CALL e%raiseError(modName//"::"//myName//" - "// &
        & "Shape of nodeCoord should be [3, nPoints]")
    CALL vtkFile%WritePoints(x=nodeCoord)
  END IF
  !! Write Cells
  IF (Content_) THEN
    CALL getVTKelementType(elemType=obj%elemType, &
      & vtk_type=vtkType, Nptrs=vtkIndx)
    nne = SIZE(vtkIndx)
    ALLOCATE (types(nCells), offsets(nCells), &
      & connectivity(nne * nCells))
    types = vtkType; offsets(1) = nne; jj = 0
    DO ii = 2, nCells
      offsets(ii) = offsets(ii - 1) + nne
    END DO
    DO ii = obj%minElemNum, obj%maxElemNum
      IF (obj%isElementPresent(ii)) THEN
        jj = jj + 1
        localNptrs = obj%getLocalNodeNumber( &
          & obj%getConnectivity(globalElement=ii))
      connectivity(offsets(jj) - nne + 1:offsets(jj)) = localNptrs(vtkIndx) - 1
      END IF
    END DO
    CALL vtkFile%WriteCells(connectivity=connectivity, offsets=offsets, &
      & types=types)
  END IF
  IF (CloseTag_) CALL vtkFile%WritePiece()
  !! clean up
  IF (ALLOCATED(types)) DEALLOCATE (types)
  IF (ALLOCATED(vtkIndx)) DEALLOCATE (vtkIndx)
  IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
  IF (ALLOCATED(offsets)) DEALLOCATE (offsets)
  IF (ALLOCATED(localNptrs)) DEALLOCATE (localNptrs)
END PROCEDURE mesh_ExportToVTK

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE elemData_Display
  CALL Display( TRIM(msg), unitno=unitno )
  CALL Display( obj%globalElemNum, msg="# globalElemNum=", unitno=unitno)
  CALL Display( obj%localElemNum, msg="# localElemNum=", unitno=unitno)
  !!
  SELECT CASE( obj%elementType )
  CASE( INTERNAL_ELEMENT )
    CALL Display( "# elementType=INTERNAL_ELEMENT", unitno=unitno)
  CASE( BOUNDARY_ELEMENT )
    CALL Display( "# elementType=BOUNDARY_ELEMENT", unitno=unitno)
  CASE( DOMAIN_BOUNDARY_ELEMENT )
    CALL Display( "# elementType=DOMAIN_BOUNDARY_ELEMENT", unitno=unitno)
  CASE( GHOST_ELEMENT )
    CALL Display( "# elementType=GHOST_ELEMENT", unitno=unitno)
  END SELECT
  !!
  !! globalNodes
  !!
  IF( ALLOCATED( obj%globalNodes ) ) THEN
    CALL Display( obj%globalNodes, msg="# globalNodes=", unitno=unitno)
  END IF
  !!
  !! globalElements
  !!
  IF( ALLOCATED( obj%globalElements ) ) THEN
    CALL Display( obj%globalElements, msg="# globalElements=", unitno=unitno)
  END IF
  !!
  !! boundaryData
  !!
  IF( ALLOCATED( obj%boundaryData ) ) THEN
    CALL Display( obj%boundaryData, msg="# boundaryData=", unitno=unitno)
  END IF
  !!
END PROCEDURE elemData_Display

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE nodeData_Display
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  CALL Display( obj%globalNodeNum, msg="# globalNodeNum=", unitno=unitno)
  CALL Display( obj%localNodeNum, msg="# localNodeNum=", unitno=unitno)
  !!
  SELECT CASE( obj%nodeType )
  CASE( INTERNAL_NODE )
    CALL Display( "# nodeType=INTERNAL_NODE", unitno=unitno)
  CASE( BOUNDARY_NODE )
    CALL Display( "# nodeType=BOUNDARY_NODE", unitno=unitno)
  CASE( DOMAIN_BOUNDARY_NODE )
    CALL Display( "# nodeType=DOMAIN_BOUNDARY_NODE", unitno=unitno)
  CASE( GHOST_NODE )
    CALL Display( "# nodeType=GHOST_NODE", unitno=unitno)
  END SELECT
  !!
  CALL Display( obj%localNodeNum, msg="# localNodeNum=", unitno=unitno)
  !!
  !! globalNodes
  !!
  IF( ALLOCATED( obj%globalNodes ) ) THEN
    CALL Display( obj%globalNodes, msg="# globalNodes=", unitno=unitno)
  END IF
  !!
  !! globalElements
  !!
  IF( ALLOCATED( obj%globalElements ) ) THEN
    CALL Display( obj%globalElements, msg="# globalElements=", unitno=unitno)
  END IF
  !!
END PROCEDURE nodeData_Display

!----------------------------------------------------------------------------
!                                                 InternalFacetData_Display
!----------------------------------------------------------------------------

MODULE PROCEDURE InternalFacetData_Display
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  CALL Display( "# elementType=INTERNAL_ELEMENT", unitno=unitno)
  CALL Display( obj%masterCellNumber, msg="# masterCellNumber=", &
    & unitno=unitno)
  CALL Display( obj%slaveCellNumber, msg="# slaveCellNumber=", &
    & unitno=unitno)
  CALL Display( obj%masterlocalFacetID, msg="# masterlocalFacetID=", &
    & unitno=unitno)
  CALL Display( obj%slavelocalFacetID, msg="# slavelocalFacetID=", &
    & unitno=unitno)
  !!
END PROCEDURE InternalFacetData_Display

!----------------------------------------------------------------------------
!                                                          MeshFacet_Display
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacet_Display
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  CALL Display( "# elementType=BOUNDARY_ELEMENT", unitno=unitno)
  !!
  IF( ALLOCATED( obj%masterCellNumber ) ) THEN
    CALL Display( obj%masterCellNumber, msg="# masterCellNumber=", &
      & unitno=unitno)
  ELSE
    CALL Display( "# masterCellNumber NOT ALLOCATED", unitno=unitno)
  END IF
  !!
  IF( ALLOCATED( obj%slaveCellNumber ) ) THEN
    CALL Display( obj%slaveCellNumber, msg="# slaveCellNumber=", &
      & unitno=unitno)
  ELSE
    CALL Display( "# slaveCellNumber NOT ALLOCATED", unitno=unitno)
  END IF
  !!
  IF( ALLOCATED( obj%masterlocalFacetID ) ) THEN
    CALL Display( obj%masterlocalFacetID, msg="# masterlocalFacetID=", &
      & unitno=unitno)
  ELSE
    CALL Display( "# masterlocalFacetID NOT ALLOCATED", unitno=unitno)
  END IF
  !!
  IF( ALLOCATED( obj%slavelocalFacetID ) ) THEN
    CALL Display( obj%slavelocalFacetID, msg="# slavelocalFacetID=", &
      & unitno=unitno)
  ELSE
    CALL Display( "# slavelocalFacetID NOT ALLOCATED", unitno=unitno)
  END IF
  !!
END PROCEDURE MeshFacet_Display

!----------------------------------------------------------------------------
!                                                   DomainFacetData_Display
!----------------------------------------------------------------------------

MODULE PROCEDURE DomainFacetData_Display
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  CALL Display( "# elementType=DOMAIN_BOUNDARY_ELEMENT", unitno=unitno)
  CALL Display( obj%masterCellNumber, msg="# masterCellNumber=", &
    & unitno=unitno)
  CALL Display( obj%masterLocalFacetID, msg="# masterlocalFacetID=", &
    & unitno=unitno)
  !
END PROCEDURE DomainFacetData_Display

!----------------------------------------------------------------------------
!                                                        DisplayElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayElementData
  !!
  INTEGER( I4B ) :: ii, telements
  !!
  telements = obj%getTotalElements()
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  !!
  DO ii = 1, telements
    CALL obj%elementData( ii )%Display( msg="elementData( "//tostring(ii) &
      & // " )=", unitno=unitno )
    CALL BlankLines( nol=2, unitno=unitno )
  END DO
  !!
END PROCEDURE mesh_DisplayElementData

!----------------------------------------------------------------------------
!                                                            DisplayNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayNodeData
  !!
  INTEGER( I4B ) :: ii, tNodes
  !!
  tNodes = obj%getTotalNodes()
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  !!
  DO ii = 1, tNodes
    CALL obj%nodeData( ii )%Display( msg="nodeData( "//tostring(ii) &
      & // " )=", unitno=unitno )
    CALL BlankLines( nol=2, unitno=unitno )
  END DO
  !!
END PROCEDURE mesh_DisplayNodeData

!----------------------------------------------------------------------------
!                                                        DisplayFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayInternalFacetData
  !!
  INTEGER( I4B ) :: ii, telements
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  !!
  IF( ALLOCATED( obj%internalFacetData ) ) THEN
    !!
    telements = SIZE( obj%internalFacetData )
    !!
    !!
    DO ii = 1, telements
      CALL obj%internalFacetData( ii )%Display( &
        & msg="internalFacetData( "//tostring(ii) &
        & // " )=", unitno=unitno )
      CALL BlankLines( nol=2, unitno=unitno )
    END DO
    !!
  ELSE
    CALL Display( "internalFacetData NOT ALLOCATED", unitno=unitno )
  END IF
  !!
END PROCEDURE mesh_DisplayInternalFacetData

!----------------------------------------------------------------------------
!                                                        DisplayFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayMeshFacetData
  !!
  INTEGER( I4B ) :: ii
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  !!
  IF( ALLOCATED( obj%meshFacetData ) ) THEN
    !!
    DO ii = 1, SIZE( obj%meshFacetData )
      CALL obj%meshFacetData( ii )%Display( &
        & msg="meshFacetData( "//tostring(ii) &
        & // " )=", unitno=unitno )
      CALL BlankLines( nol=2, unitno=unitno )
    END DO
    !!
  ELSE
    CALL Display( "meshFacetData NOT ALLOCATED", unitno=unitno )
  END IF
  !!
END PROCEDURE mesh_DisplayMeshFacetData

!----------------------------------------------------------------------------
!                                                        DisplayFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayDomainFacetData
  !!
  INTEGER( I4B ) :: ii
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  !!
  IF( ALLOCATED( obj%domainFacetData ) ) THEN
    !!
    DO ii = 1, SIZE( obj%domainFacetData )
      CALL obj%domainFacetData( ii )%Display( &
        & msg="domainFacetData( "//tostring(ii) &
        & // " )=", unitno=unitno )
      CALL BlankLines( nol=2, unitno=unitno )
    END DO
    !!
  ELSE
    CALL Display( "domainFacetData NOT ALLOCATED", unitno=unitno )
  END IF
  !!
END PROCEDURE mesh_DisplayDomainFacetData

END SUBMODULE IOMethods
