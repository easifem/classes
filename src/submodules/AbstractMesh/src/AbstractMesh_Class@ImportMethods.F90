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

SUBMODULE(AbstractMesh_Class) ImportMethods
USE ReallocateUtility, ONLY: Reallocate

USE HDF5File_Method, ONLY: HDF5GetEntities, &
                           HDF5ReadScalar, &
                           HDF5ReadVector, &
                           HDF5ReadMatrix

USE ArangeUtility, ONLY: Arange

USE InputUtility, ONLY: Input

USE ReferenceElement_Method, ONLY: GetElementIndex, &
                                   GetTotalNodes, &
                                   ReferenceElementInfo, &
                                   GetVTKelementType_

USE Display_Method, ONLY: Display, &
                          EqualLine, &
                          ToString

USE AssertUtility, ONLY: Assert

USE NodeData_Class, ONLY: INTERNAL_NODE, &
                          BOUNDARY_NODE, &
                          NodeData_Set, &
                          NodeData_SetNodeCoord

USE ElemData_Class, ONLY: ElemData_Set, &
                          ElemData_Deallocate

USE GlobalData, ONLY: stdout, INT8

USE StringUtility, ONLY: PathJoin, PathBase

USE VTKFile_Class, ONLY: VTK_BINARY_APPENDED, VTK_UnStructuredGrid

IMPLICIT NONE

CONTAINS

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
                        '[START] ')
#endif

group0 = Input(option=group, default="")

isArg = [PRESENT(group), PRESENT(dim), PRESENT(entities)]

cases(1) = (.NOT. isArg(2)) .AND. (.NOT. isArg(3))
cases(2) = ALL(isArg(2:3))
cases(3) = isArg(2) .AND. (.NOT. isArg(3))

IF (cases(1)) THEN
  ! group present
  ! dim absent
  ! entities absent
  CALL MeshImportFromGroup(obj, hdf5, group0)

ELSEIF (cases(2)) THEN
  ! group present
  ! dim present
  ! entities present
  CALL MeshImportFromDim(obj, hdf5, group0, dim, entities, SIZE(entities))

ELSEIF (cases(3)) THEN

  CALL HDF5GetEntities(hdf5=hdf5, group=group0, dim=dim, &
                       tEntities=tEntities, myName=myName, modName=modName)

  IF (tEntities .GT. 0_I4B) THEN
    entities0 = Arange(1_I4B, tEntities)
    CALL MeshImportFromDim(obj, hdf5, group0, dim, entities0, tEntities)

  ELSE
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            'tEntities is zero')
  END IF

ELSE

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found')

  RETURN
END IF

CALL obj%InitiateElementToElements()

IF (ALLOCATED(entities0)) DEALLOCATE (entities0)
group0 = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                           LocateNodeCoord
!----------------------------------------------------------------------------

FUNCTION LocateNodeCoord(hdf5, group, isok) RESULT(dsetname)
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  LOGICAL(LGT), INTENT(OUT) :: isok
  CHARACTER(:), ALLOCATABLE :: dsetname

  dsetname = TRIM(group)

  IF (dsetname == "") dsetname = "/"

  isok = hdf5%pathExists(dsetname)

  IF (.NOT. isok) THEN
    dsetname = PathJoin(group, "nodeCoord")
    isok = hdf5%pathExists(dsetname)
  END IF

  IF (.NOT. isok) THEN
    dsetname = PathBase(group)//"/nodeCoord"
    isok = hdf5%pathExists(dsetname)
  END IF

END FUNCTION LocateNodeCoord

!----------------------------------------------------------------------------
!                                                              GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord1
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord1()"
CHARACTER(:), ALLOCATABLE :: dsetname
INTEGER(I4B) :: ii, jj
REAL(DFP), ALLOCATABLE :: xij(:, :)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                '[INTERNAL ERROR] :: HDF5 file does not have read permission')
END IF

dsetname = LocateNodeCoord(hdf5, group, isok)

IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: cannot locate nodeCoord in the path')
  RETURN
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
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeCoord1

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
CALL e%RaiseError(modName//"::"//myName//" - "// &
                  "[WIP]: This routine has not been implemented yet.")
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                ExportToVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
LOGICAL(LGT) :: opentag_, closetag_, content_, isok
INTEGER(INT8), ALLOCATABLE :: types(:)
INTEGER(I4B) :: ncells, npoints, ii, jj, nne, maxnne, elemtype, tsize
INTEGER(I4B), ALLOCATABLE :: vtkindx(:), connectivity(:), &
                             offsets(:), cellcon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main
IF (.NOT. vtk%isInitiated) THEN
  isok = PRESENT(filename)
  CALL AssertError1(isok, myname, &
                    "VTKFile_ is not initiated, and filename is not present.")

  CALL vtk%InitiateVTKFile(filename=filename, &
                           mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
                           DataStructureType=VTK_UnStructuredGrid)

END IF

ncells = obj%GetTotalElements()
npoints = obj%GetTotalNodes()

opentag_ = INPUT(default=.TRUE., option=OpenTag)
closetag_ = INPUT(default=.TRUE., option=CloseTag)
content_ = INPUT(default=.TRUE., option=Content)

! Write piece information if OpenTag is true
IF (opentag_) CALL vtk%WritePiece(nPoints=npoints, nCells=ncells)

!  Write Points information
IF (PRESENT(nodeCoord)) THEN
  isok = (SIZE(nodecoord, 1) .EQ. 3) .AND. (SIZE(nodecoord, 2) .EQ. npoints)

  CALL AssertError1(isok, myname, &
                    "Shape of nodeCoord should be [3, nPoints]")

  CALL vtk%WritePoints(x=nodeCoord)

ELSE
  CALL Reallocate(xij, 3, npoints)
  CALL obj%GetNodeCoord(nodecoord=xij, nrow=ii, ncol=jj)
  CALL vtk%WritePoints(x=xij)

END IF

! Write Cells
IF (Content_) THEN

  maxnne = obj%GetMaxNNE()

#ifdef DEBUG_VER
  isok = maxnne .GT. 0
  CALL AssertError1(isok, myname, &
                    "maxnne should be greater than zero")
#endif

  ALLOCATE (types(ncells), offsets(0:ncells), connectivity(maxnne * ncells), &
            cellcon(maxnne), vtkindx(maxnne))

  offsets(0) = 0

  DO ii = 1, ncells
    isok = obj%isElementActive(globalElement=ii, islocal=.TRUE.)
    IF (.NOT. isok) CYCLE

    elemtype = obj%GetElemType(globalElement=ii, islocal=.TRUE.)

    CALL GetVTKelementType_(elemType=elemtype, vtk_type=types(ii), &
                            nptrs=vtkindx, tsize=nne)

    offsets(ii) = offsets(ii - 1) + nne

    CALL obj%GetConnectivity_(globalElement=ii, islocal=.TRUE., &
                              ans=cellcon, tsize=tsize)

#ifdef DEBUG_VER
    isok = tsize .EQ. nne
    CALL AssertError1(isok, myname, &
                      "tsize should be equal to nne")
#endif

    CALL obj%GetLocalNodeNumber_(globalNode=cellcon(1:tsize), &
                                 ans=cellcon(1:tsize), islocal=.FALSE.)

    DO jj = 1, nne
      connectivity(offsets(ii) - nne + jj) = cellcon(vtkindx(jj)) - 1
    END DO

  END DO

  tsize = offsets(ncells)

  CALL vtk%WriteCells(connectivity=connectivity(1:tsize), &
                      offsets=offsets(1:ncells), &
                      types=types(1:ncells))

END IF

IF (CloseTag_) CALL vtk%WritePiece()
! clean up
IF (ALLOCATED(types)) DEALLOCATE (types)
IF (ALLOCATED(vtkindx)) DEALLOCATE (vtkindx)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(offsets)) DEALLOCATE (offsets)
IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                         MeshImportScalar
!----------------------------------------------------------------------------

SUBROUTINE MeshImportScalar(obj, hdf5, group)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

  CHARACTER(*), PARAMETER :: myName = "MeshImportScalar()"
  CHARACTER(:), ALLOCATABLE :: dsetname

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  dsetname = TRIM(group)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%uid, group=dsetname, &
                fieldname="uid", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%nsd, group=dsetname, &
                fieldname="nsd", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%xidim, group=dsetname, &
              fieldname="xidim", myname=myname, modname=modname, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%tElements, group=dsetname, &
          fieldname="tElements", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minX, group=dsetname, &
               fieldname="minX", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minY, group=dsetname, &
               fieldname="minY", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minZ, group=dsetname, &
               fieldname="minZ", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxX, group=dsetname, &
               fieldname="maxX", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxY, group=dsetname, &
               fieldname="maxY", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxZ, group=dsetname, &
               fieldname="maxZ", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%x, group=dsetname, &
                  fieldname="x", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%y, group=dsetname, &
                  fieldname="y", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%z, group=dsetname, &
                  fieldname="z", myname=myname, modName=modName, check=.TRUE.)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportScalar

!----------------------------------------------------------------------------
!                                                          MeshImportVector
!----------------------------------------------------------------------------

SUBROUTINE MeshImportVector(obj, hdf5, group, connectivity, elemNumber, &
                            internalNptrs)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: connectivity(:, :)
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: elemNumber(:)
  INTEGER(I4B), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: internalNptrs(:)

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "MeshImportVector()"
  CHARACTER(:), ALLOCATABLE :: dsetname
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  dsetname = TRIM(group)

  CALL HDF5ReadVector(hdf5=hdf5, VALUE=obj%boundingEntity, group=dsetname, &
                 fieldname="boundingEntity", myname=myname, modName=modName, &
                      check=.FALSE.)

  ! If boundingEntity is not initiated then we initiate it with size=0
  ! Bounding entity will not be initiated for point type
  IF (.NOT. ALLOCATED(obj%boundingEntity)) THEN
    CALL Reallocate(obj%boundingEntity, 0)
  END IF

  CALL HDF5ReadVector(hdf5=hdf5, VALUE=elemNumber, group=dsetname, &
         fieldname="elemNumber", myname=myname, modName=modName, check=.TRUE.)

  isok = .FALSE.
  IF (ALLOCATED(elemNumber)) THEN
    isok = SIZE(elemNumber) .NE. 0
  END IF

  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: either elemNumber not ALLOCATED '// &
                      'or size of elemNumber is zero')
    RETURN
  END IF

  CALL HDF5ReadMatrix(hdf5=hdf5, VALUE=connectivity, group=dsetname, &
       fieldname="connectivity", myname=myname, modName=modName, check=.TRUE.)

  isok = ALLOCATED(connectivity)
  IF (isok) THEN
    isok = SIZE(connectivity, 1) .NE. 0 .AND. SIZE(connectivity, 2) .NE. 0
  END IF

  IF (.NOT. isok) THEN
    CALL Display(dsetname, "dsetname: ", unitno=stdout)
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      '[INTERNAL ERROR] :: either connectivity not allocated,&
      & or its size is zero')
    RETURN
  END IF

  obj%maxNNE = SIZE(connectivity, 1)

  IF (PRESENT(internalNptrs)) THEN
    CALL HDF5ReadVector(hdf5=hdf5, VALUE=internalNptrs, group=dsetname, &
                  fieldname="intNodeNumber", myname=myname, modName=modName, &
                        check=.TRUE.)
  END IF

  obj%maxElemNum = MAXVAL(elemNumber)
  obj%minElemNum = MINVAL(elemNumber)
  obj%maxNptrs = MAXVAL(connectivity)
  obj%minNptrs = MINVAL(connectivity)

  dsetname = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportVector

!----------------------------------------------------------------------------
!                                                     MeshImportCheckError
!----------------------------------------------------------------------------

SUBROUTINE MeshImportCheckError(hdf5, group)
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

  CHARACTER(*), PARAMETER :: myName = "MeshImportCheckError()"
  CHARACTER(:), ALLOCATABLE :: dsetname
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  dsetname = TRIM(group)

  isok = hdf5%isOpen()
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INTERNAL ERROR]:: HDF5 file is not opened')
    RETURN
  END IF

  isok = hdf5%isRead()
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR]:: HDF5 file does not have read permission')
    RETURN
  END IF

  isok = hdf5%isGroup(dsetname)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INTERNAL ERROR]:: '//dsetname// &
        ' is not a group; it should be a group which contains the meshEntity')
    RETURN
  END IF

  isok = hdf5%pathExists(dsetname)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                     '[INTERNAL ERROR]:: '//dsetname//' path does not exists')
    RETURN
  END IF

  dsetname = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportCheckError

!----------------------------------------------------------------------------
!                                                   MeshImportElementData
!----------------------------------------------------------------------------

SUBROUTINE MeshImportElementData(obj, hdf5, group, connectivity, elemNumber)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  INTEGER(I4B), INTENT(IN) :: connectivity(:, :)
  INTEGER(I4B), INTENT(IN) :: elemNumber(:)

  ! Internal variables
  INTEGER(I4B) :: ii, elemType, meshID
  CHARACTER(:), ALLOCATABLE :: dsetname
  CHARACTER(*), PARAMETER :: myName = "MeshImportElementData()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  dsetname = TRIM(group)

  ALLOCATE (obj%elementData(obj%tElements))

  CALL Reallocate(obj%local_elemNumber, obj%maxElemNum)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=elemType, group=dsetname, &
           fieldname="elemType", myname=myname, modname=modname, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=meshID, group=dsetname, &
                fieldname="uid", myname=myname, modname=modname, check=.TRUE.)

  !$OMP PARALLEL DO PRIVATE(ii)
  DO ii = 1, obj%tElements
    obj%local_elemNumber(elemNumber(ii)) = ii

    ALLOCATE (obj%elementData(ii)%ptr)
    CALL ElemData_Deallocate(obj%elementData(ii)%ptr)

    CALL ElemData_Set(obj=obj%elementData(ii)%ptr, &
                      globalElemNum=elemNumber(ii), &
                      localElemNum=ii, &
                      globalNodes=connectivity(:, ii), &
                      name=elemType, &
                      isActive=.TRUE., &
                      meshID=meshID)
  END DO
  !$OMP END PARALLEL DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportElementData

!----------------------------------------------------------------------------
!                                                   MeshImportElementData
!----------------------------------------------------------------------------

SUBROUTINE MeshImportElementDataFromDim(obj, elemType, meshID, &
                                  connectivity, elemNumber, istart, iend, nne)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: meshID
  INTEGER(I4B), INTENT(IN) :: connectivity(:, :)
  INTEGER(I4B), INTENT(IN) :: elemNumber(:)
  INTEGER(I4B), INTENT(IN) :: istart
  INTEGER(I4B), INTENT(IN) :: iend
  INTEGER(I4B), INTENT(IN) :: nne

  ! Internal variables
  INTEGER(I4B) :: ii
  CHARACTER(*), PARAMETER :: myName = "MeshImportElementDataFromDim()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  !$OMP PARALLEL DO PRIVATE(ii)
  DO ii = istart, iend
    obj%local_elemNumber(elemNumber(ii)) = ii

    ALLOCATE (obj%elementData(ii)%ptr)
    CALL ElemData_Deallocate(obj%elementData(ii)%ptr)

    CALL ElemData_Set(obj=obj%elementData(ii)%ptr, &
                      globalElemNum=elemNumber(ii), &
                      localElemNum=ii, &
                      globalNodes=connectivity(1:nne, ii), &
                      name=elemType, &
                      isActive=.TRUE., &
                      meshID=meshID)

  END DO
  !$OMP END PARALLEL DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportElementDataFromDim

!----------------------------------------------------------------------------
!                                                   MeshImportNodeData
!----------------------------------------------------------------------------

SUBROUTINE MeshImportNodeData(obj, connectivity, internalNptrs)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: connectivity(:, :)
  INTEGER(I4B), INTENT(IN) :: internalNptrs(:)

  ! Internal veriables
  INTEGER(I4B) :: ii, jj, aint, dummy
  LOGICAL(LGT), ALLOCATABLE :: mask(:)
  CHARACTER(*), PARAMETER :: myName = "MeshImportNodeData()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  aint = SIZE(connectivity, 1)
  CALL Reallocate(obj%local_nptrs, obj%maxNptrs)

  DO CONCURRENT(ii=1:obj%tElements)
    DO jj = 1, aint
      obj%local_nptrs(connectivity(jj, ii)) = connectivity(jj, ii)
    END DO
  END DO

  obj%tNodes = COUNT(obj%local_nptrs .NE. 0)
  ALLOCATE (obj%nodeData(obj%tNodes))
  CALL Reallocate(mask, obj%maxNptrs)
  mask = .FALSE.
  mask(internalNptrs) = .TRUE.

  dummy = 0
  DO ii = 1, obj%maxNptrs
    IF (obj%local_nptrs(ii) .EQ. 0) CYCLE

    dummy = dummy + 1

    IF (mask(ii)) THEN
      aint = INTERNAL_NODE
    ELSE
      aint = BOUNDARY_NODE
    END IF

    ALLOCATE (obj%nodeData(dummy)%ptr)

    CALL NodeData_Set(obj=obj%nodeData(dummy)%ptr, &
                      globalNodeNum=obj%local_nptrs(ii), &
                      localNodeNum=dummy, &
                      nodeType=aint)

    obj%local_nptrs(ii) = dummy
  END DO

  IF (ALLOCATED(mask)) DEALLOCATE (mask)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportNodeData

!----------------------------------------------------------------------------
!                                                        MeshImportNodeData
!----------------------------------------------------------------------------

SUBROUTINE MeshImportNodeDataFromDim(obj, connectivity)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: connectivity(:, :)

  ! Internal veriables
  INTEGER(I4B) :: ii, jj, aint, dummy
  CHARACTER(*), PARAMETER :: myName = "MeshImportNodeDataFromDim()"
  LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  aint = SIZE(connectivity, 1)
  CALL Reallocate(obj%local_nptrs, obj%maxNptrs)

  DO ii = 1, obj%tElements
    DO jj = 1, aint
      dummy = connectivity(jj, ii)
      IF (dummy .NE. 0) THEN
        obj%local_nptrs(dummy) = dummy
      END IF
    END DO
  END DO

  obj%tNodes = COUNT(obj%local_nptrs .NE. 0)
  ALLOCATE (obj%nodeData(obj%tNodes))

  dummy = 0
  DO ii = 1, obj%maxNptrs
    IF (obj%local_nptrs(ii) .EQ. 0) CYCLE

    dummy = dummy + 1

#ifdef DEBUG_VER
    problem = ASSOCIATED(obj%nodeData(dummy)%ptr)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: obj%nodeData('//tostring(dummy)// &
                        ')%ptr is already associated')
      RETURN
    END IF
#endif

    ALLOCATE (obj%nodeData(dummy)%ptr)

    CALL NodeData_Set(obj=obj%nodeData(dummy)%ptr, &
                      globalNodeNum=obj%local_nptrs(ii), &
                      localNodeNum=dummy, &
                      nodeType=INTERNAL_NODE)

    obj%local_nptrs(ii) = dummy
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportNodeDataFromDim

!----------------------------------------------------------------------------
!                                                   MeshImportFromGroup
!----------------------------------------------------------------------------

SUBROUTINE MeshImportFromGroup(obj, hdf5, group)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "MeshImportFromGroup()"
  INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:), &
                               internalNptrs(:)
  TYPE(CPUTime_) :: TypeCPUTime
  CHARACTER(:), ALLOCATABLE :: dsetname
  LOGICAL(LGT) :: isok
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  REAL(DFP) :: x(3)
  INTEGER(I4B) :: ii, jj, kk, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL obj%DEALLOCATE()

  CALL MeshImportCheckError(hdf5, group)

  IF (obj%showTime) THEN
    CALL Display("Showing Time States of Importing Mesh", unitno=stdout)
    CALL EqualLine(unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportScalar(obj, hdf5, group)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName// &
                 " : time in importing scalar data [MeshImportScalar] : "// &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportVector(obj, hdf5, group, connectivity, elemNumber, &
                        internalNptrs)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName// &
                 " : time in importing vector data [MeshImportVector] : "// &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportElementData(obj, hdf5, group, connectivity, elemNumber)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName// &
            " : time in importing element data [MeshImportElementData] : "// &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportNodeData(obj, connectivity, internalNptrs)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName// &
                 " : time in importing node data [MeshImportNodeData] : "// &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  dsetname = LocateNodeCoord(hdf5, group, isok)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: cannot locate nodeCoord in the path')
    RETURN
  END IF

  ! INFO: HDf5ReadMatrix is defined in HDF5File_Method
  CALL HDF5ReadMatrix(hdf5=hdf5, check=.TRUE., group=dsetname, &
                      VALUE=xij, fieldname="", myName=myName, &
                      modName=modName)
  jj = SIZE(xij, 1)
  tsize = obj%GetTotalNodes()
  x = 0.0_DFP
  DO ii = 1, tsize
    x(1:jj) = xij(1:jj, ii)
    kk = obj%GetLocalNodeNumber(globalNode=ii, islocal=.FALSE.)
    CALL NodeData_SetNodeCoord(obj%nodeData(kk)%ptr, x)
  END DO

  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
  IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
  IF (ALLOCATED(internalNptrs)) DEALLOCATE (internalNptrs)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportFromGroup

!----------------------------------------------------------------------------
!                                                       MeshImportFromDim
!----------------------------------------------------------------------------

SUBROUTINE MeshImportFromDim(obj, hdf5, group, dim, entities, tEntities)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B), INTENT(IN) :: entities(:)
  INTEGER(I4B), INTENT(IN) :: tEntities

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "MeshImportFromDim()"
  INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:), &
                          temp_int_2d(:, :), temp_int_1d(:), boundingEntity(:)
  INTEGER(I4B) :: ii, jj, kk, tElements(tEntities), tsize, &
                  nsd(tEntities), uid(tEntities), elemType(tEntities), &
                  nne(tEntities), aint, bint, maxBoundingEntities, &
                  xidim(tEntities)
  ! TYPE(CPUTime_) :: TypeCPUTime
  CHARACTER(:), ALLOCATABLE :: dsetname, prefix
  REAL(DFP), DIMENSION(3, tEntities) :: xyz, min_xyz, max_xyz
  TYPE(CPUTime_) :: TypeCPUTime
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  REAL(DFP) :: x(3)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')

#endif

  CALL obj%DEALLOCATE()

  IF (obj%showTime) THEN
    CALL Display("Showing Time States of Importing Mesh", unitno=stdout)
    CALL EqualLine(unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  SELECT CASE (dim)
  CASE (0_I4B)
    prefix = group//"/pointEntities_"
    maxBoundingEntities = 0_I4B
  CASE (1_I4B)
    prefix = group//"/curveEntities_"
  CASE (2_I4B)
    prefix = group//"/surfaceEntities_"
  CASE (3_I4B)
    prefix = group//"/volumeEntities_"
  END SELECT

  DO ii = 1, tEntities
    dsetname = prefix//ToString(entities(ii))
    CALL MeshImportCheckError(hdf5, dsetname)
    CALL MeshImportScalar(obj, hdf5, dsetname)
    CALL HDF5ReadScalar(hdf5=hdf5, VALUE=elemType(ii), group=dsetname, &
           fieldname="elemType", myname=myname, modname=modname, check=.TRUE.)

    nne(ii) = GetTotalNodes(elemType(ii))

    uid(ii) = obj%uid
    nsd(ii) = obj%nsd
    xidim(ii) = obj%xidim
    tElements(ii) = obj%tElements
    min_xyz(1, ii) = obj%minX
    min_xyz(2, ii) = obj%minY
    min_xyz(3, ii) = obj%minZ
    max_xyz(1, ii) = obj%maxX
    max_xyz(2, ii) = obj%maxY
    max_xyz(3, ii) = obj%maxZ
    xyz(1, ii) = obj%x
    xyz(2, ii) = obj%y
    xyz(3, ii) = obj%z

    aint = GetElementIndex(elemType(ii))
    obj%tElements_topology_wise(aint) = obj%tElements_topology_wise(aint) &
                                        + tElements(ii)

  END DO

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName// &
                 " : time in importing scalar data [MeshImportScalar] : "// &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

#ifdef DEBUG_VER
  CALL Assert(nn=nsd, &
             msg="[INTERNAL ERROR] :: nsd of all entities is not the same.", &
              file=__FILE__, &
              line=__LINE__, &
              routine=myName)

  CALL Assert(nn=xidim, &
           msg="[INTERNAL ERROR] :: xidim of all entities is not the same.", &
              file=__FILE__, &
              line=__LINE__, &
              routine=myName)
#endif

  DO ii = 1, SIZE(obj%tElements_topology_wise)
    aint = obj%tElements_topology_wise(ii)
    IF (aint .GT. 0) THEN
      obj%tElemTopologies = obj%tElemTopologies + 1
      obj%elemTopologies(obj%tElemTopologies) = &
        ReferenceElementInfo%elemTopologyName(ii)
    END IF
  END DO

  obj%maxNNE = MAXVAL(nne)
  obj%tElements = SUM(tElements)

  isok = obj%maxNNE .NE. 0 .AND. obj%tElements .NE. 0
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: maxNNE or tElements are zero')
    RETURN
  END IF

  CALL Reallocate(connectivity, obj%maxNNE, obj%tElements)
  CALL Reallocate(elemNumber, obj%tElements)

  IF (dim .GT. 0) THEN
    CALL HDF5GetEntities(hdf5=hdf5, group=group, dim=dim - 1, &
                tEntities=maxBoundingEntities, myName=myName, modName=modName)
  ELSE
    maxBoundingEntities = 0
  END IF

  CALL Reallocate(boundingEntity, maxBoundingEntities)

  aint = 0
  bint = 0
  DO ii = 1, tEntities
    dsetname = prefix//ToString(entities(ii))
    CALL MeshImportVector(obj, hdf5, dsetname, temp_int_2d, temp_int_1d)
    aint = bint + 1
    bint = bint + tElements(ii)
    connectivity(1:nne(ii), aint:bint) = &
      temp_int_2d(1:nne(ii), 1:tElements(ii))
    elemNumber(aint:bint) = temp_int_1d(1:tElements(ii))

    DO jj = 1, SIZE(obj%boundingEntity)
      kk = ABS(obj%boundingEntity(jj))
      boundingEntity(kk) = boundingEntity(kk) + obj%boundingEntity(jj)
    END DO
  END DO

  aint = 0
  DO ii = 1, SIZE(boundingEntity)
    IF (boundingEntity(ii) .NE. 0) aint = aint + 1
  END DO

  CALL Reallocate(obj%boundingEntity, aint)

  aint = 0
  DO ii = 1, SIZE(boundingEntity)
    IF (boundingEntity(ii) .NE. 0) THEN
      aint = aint + 1
      obj%boundingEntity(aint) = boundingEntity(ii)
    END IF
  END DO

  obj%maxElemNum = MAXVAL(elemNumber)
  obj%minElemNum = MINVAL(elemNumber)
  obj%maxNptrs = MAXVAL(connectivity)
  obj%minNptrs = MINVAL(connectivity)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName// &
                 " : time in importing vector data [MeshImportVector] : "// &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  ALLOCATE (obj%elementData(obj%tElements))

  CALL Reallocate(obj%local_elemNumber, obj%maxElemNum)

  bint = 0
  DO ii = 1, tEntities
    aint = bint + 1
    bint = bint + tElements(ii)
    CALL MeshImportElementDataFromDim(obj=obj, elemType=elemType(ii), &
                                      meshID=entities(ii), &
                                      connectivity=connectivity, &
                                      elemNumber=elemNumber, &
                                      istart=aint, &
                                      iend=bint, &
                                      nne=nne(ii))
  END DO

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName// &
            " : time in importing element data [MeshImportElementData] : "// &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportNodeDataFromDim(obj, connectivity)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName// &
                 " : time in importing node data [MeshImportNodeData] : "// &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  dsetname = LocateNodeCoord(hdf5, group, isok)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: cannot locate nodeCoord in the path')
    RETURN
  END IF

  ! INFO: HDf5ReadMatrix is defined in HDF5File_Method
  CALL HDF5ReadMatrix(hdf5=hdf5, check=.TRUE., group=dsetname, &
                      VALUE=xij, fieldname="", myName=myName, &
                      modName=modName)
  jj = SIZE(xij, 1)
  tsize = obj%GetTotalNodes()
  x = 0.0_DFP
  DO ii = 1, tsize
    x(1:jj) = xij(1:jj, ii)
    kk = obj%GetLocalNodeNumber(globalNode=ii, islocal=.FALSE.)
    CALL NodeData_SetNodeCoord(obj%nodeData(kk)%ptr, x)
  END DO

  IF (ALLOCATED(xij)) DEALLOCATE (xij)

  IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
  IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
  IF (ALLOCATED(temp_int_2d)) DEALLOCATE (temp_int_2d)
  IF (ALLOCATED(temp_int_1d)) DEALLOCATE (temp_int_1d)
  IF (ALLOCATED(boundingEntity)) DEALLOCATE (boundingEntity)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE MeshImportFromDim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ImportMethods
