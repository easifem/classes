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
USE HDF5File_Method, ONLY: HDF5ReadScalar, HDF5ReadVector, HDF5ReadMatrix
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

CALL Display(obj%isFacetDataInitiated, "isFacetDataInitiated: ",  &
  & unitno=unitno)

CALL Display(obj%uid, "uid: ", unitno=unitno)

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
CHARACTER(:), ALLOCATABLE :: dsetname
INTEGER(I4B) :: ii, dummy
LOGICAL(LGT) :: isok
INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:),  &
  & internalNptrs(:)
LOGICAL(LGT), ALLOCATABLE :: mask(:)

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

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%uid, group=dsetname,  &
  & fieldname="uid", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%nsd, group=dsetname,  &
  & fieldname="nsd", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%tIntNodes, group=dsetname,  &
  & fieldname="tIntNodes", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%tElements, group=dsetname,  &
  & fieldname="tElements", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minX, group=dsetname,  &
  & fieldname="minX", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minY, group=dsetname,  &
  & fieldname="minY", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minZ, group=dsetname,  &
  & fieldname="minZ", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxX, group=dsetname,  &
  & fieldname="maxX", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxY, group=dsetname,  &
  & fieldname="maxY", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxZ, group=dsetname,  &
  & fieldname="maxZ", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%x, group=dsetname,  &
  & fieldname="x", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%y, group=dsetname,  &
  & fieldname="y", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%z, group=dsetname,  &
  & fieldname="z", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadVector(hdf5=hdf5, VALUE=obj%physicalTag, group=dsetname,  &
  & fieldname="physicalTag", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadVector(hdf5=hdf5, VALUE=elemNumber, group=dsetname,  &
  & fieldname="elemNumber", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadMatrix(hdf5=hdf5, VALUE=connectivity, group=dsetname,  &
  & fieldname="connectivity", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadVector(hdf5=hdf5, VALUE=internalNptrs, group=dsetname,  &
  & fieldname="intNodeNumber", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadVector(hdf5=hdf5, VALUE=obj%boundingEntity, group=dsetname,  &
  & fieldname="boundingEntity", myname=myname, modname=modname, check=.TRUE.)

isok = .FALSE.
IF (ALLOCATED(elemNumber)) THEN
  isok = SIZE(elemNumber) .NE. 0
END IF

IF (isok) THEN
  obj%maxElemNum = MAXVAL(elemNumber)
  obj%minElemNum = MINVAL(elemNumber)
  obj%maxNptrs = MAXVAL(connectivity)
  obj%minNptrs = MINVAL(connectivity)
END IF

CALL Reallocate(obj%local_elemNumber, obj%maxElemNum)
CALL Reallocate(obj%local_nptrs, obj%maxNptrs)
CALL Reallocate(mask, obj%maxNptrs)
ALLOCATE (obj%elementData(obj%tElements))

mask = .FALSE.
mask(internalNptrs) = .TRUE.

DO CONCURRENT(ii=1:obj%tElements)
  obj%local_elemNumber(elemNumber(ii)) = ii
  obj%elementData(ii)%globalElemNum = elemNumber(ii)
  obj%elementData(ii)%localElemNum = ii
  obj%elementData(ii)%globalNodes = connectivity(:, ii)
  obj%local_nptrs(connectivity(:, ii)) = connectivity(:, ii)
END DO

obj%tNodes = COUNT(obj%local_nptrs .NE. 0)
dummy = 0
DO ii = 1, obj%maxNptrs
  IF (obj%local_nptrs(ii) .NE. 0) THEN
    dummy = dummy + 1
    obj%nodeData(dummy)%globalNodeNum = obj%local_Nptrs(ii)
    obj%nodeData(dummy)%localNodeNum = dummy

    IF (mask(ii)) THEN
      obj%nodeData(dummy)%nodeType = INTERNAL_NODE
    ELSE
      obj%nodeData(dummy)%nodeType = BOUNDARY_NODE
    END IF

    obj%local_nptrs(ii) = dummy
  END IF
END DO

IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(internalNptrs)) DEALLOCATE (internalNptrs)
IF (ALLOCATED(mask)) DEALLOCATE (mask)

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
