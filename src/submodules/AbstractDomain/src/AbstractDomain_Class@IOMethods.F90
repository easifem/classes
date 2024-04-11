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

SUBMODULE(AbstractDomain_Class) IOMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method
USE StringUtility
USE ReallocateUtility
USE tomlf, ONLY: toml_serialize, toml_get => get_value
USE TomlUtility
USE HDF5File_Method
USE FEMesh_Class, ONLY: FEMesh_, FEMesh_Pointer
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL Display(obj%isInitiated, "AbstractDomain_::obj Initiated: ", unitno=unitno)
IF (.NOT. obj%isInitiated) RETURN

CALL Display("engine: "//obj%engine, unitno=unitno)
CALL Display("majorVersion: "//tostring(obj%majorVersion), unitno=unitno)
CALL Display("minorVersion: "//tostring(obj%minorVersion), unitno=unitno)
CALL Display("version: "//tostring(obj%version), unitno=unitno)
CALL Display("nsd: "//tostring(obj%nsd), unitno=unitno)
CALL Display("maxNptrs: "//tostring(obj%maxNptrs), unitno=unitno)
CALL Display("minNptrs: "//tostring(obj%minNptrs), unitno=unitno)
CALL Display("tNodes: "//tostring(obj%tNodes), unitno=unitno)
CALL Display(obj%isNodeNumberSparse, "isNodeNumberSparse: ", unitno=unitno)
CALL Display("maxElemNum: "//tostring(obj%maxElemNum), unitno=unitno)
CALL Display("minElemNum: "//tostring(obj%minElemNum), unitno=unitno)
CALL Display(obj%isElemNumberSparse, "isElemNumberSparse: ", unitno=unitno)
CALL Display("tEntitiesForNodes: "//tostring(obj%tEntitiesForNodes), &
  & unitno=unitno)
CALL Display("tEntitiesForElements: "//tostring(obj%tEntitiesForElements), &
  & unitno=unitno)
CALL Display("tEntitiesForElements: "//tostring(obj%tEntitiesForElements), &
  & unitno=unitno)
CALL Display("tElements: "//tostring(obj%tElements), &
  & unitno=unitno)
CALL Display("tEntities: "//tostring(obj%tEntities), &
  & unitno=unitno)

abool = ALLOCATED(obj%nodeCoord)
CALL Display(abool, "nodeCoord Allocated: ", unitno=unitno)

abool = ASSOCIATED(obj%meshVolume)
CALL Display(abool, "meshVolume ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL BlankLines(nol=1, unitno=unitno)
  CALL obj%meshVolume%DisplayMeshInfo("Volume Mesh Info:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END IF

abool = ASSOCIATED(obj%meshSurface)
CALL Display(abool, "meshSurface ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL BlankLines(nol=1, unitno=unitno)
  CALL obj%meshSurface%DisplayMeshInfo("Surface Mesh Info:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END IF

abool = ASSOCIATED(obj%meshCurve)
CALL Display(abool, "meshCurve ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL BlankLines(nol=1, unitno=unitno)
  CALL obj%meshCurve%DisplayMeshInfo("Curve Mesh Info:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END IF

abool = ASSOCIATED(obj%meshPoint)
CALL Display(abool, "meshPoint ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL BlankLines(nol=1, unitno=unitno)
  CALL obj%meshPoint%DisplayMeshInfo("Point Mesh Info:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END IF

CALL Display(obj%meshMap%isInitiated, "meshMap Initiated: ", unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                          DisplaDomainInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayDomainInfo
LOGICAL(LGT) :: abool

CALL Display(obj%isInitiated, "AbstractDomain_::obj Initiated: ", &
             unitno=unitno)
IF (.NOT. obj%isInitiated) RETURN

CALL EqualLine(unitno=unitno)
CALL Display("engine: "//obj%engine, unitno=unitno)
CALL Display("version: "//tostring(obj%version), unitno=unitno)
CALL Display("nsd: "//tostring(obj%nsd), unitno=unitno)
CALL Display("minNptrs: "//tostring(obj%minNptrs), unitno=unitno)
CALL Display("maxNptrs: "//tostring(obj%maxNptrs), unitno=unitno)
CALL Display("minElemNum: "//tostring(obj%minElemNum), unitno=unitno)
CALL Display("maxElemNum: "//tostring(obj%maxElemNum), unitno=unitno)

CALL Display("tNodes: "//tostring(obj%tNodes), unitno=unitno)

CALL Display("tEntitiesForNodes: "//tostring(obj%tEntitiesForNodes), &
  & unitno=unitno)

CALL Display("tEntitiesForElements: "//tostring(obj%tEntitiesForElements), &
  & unitno=unitno)

CALL Display("tElements: "//tostring(obj%tElements), unitno=unitno)

CALL Display("Total mesh of volume: "//tostring(obj%tEntities(3)),  &
  & unitno=unitno)

CALL Display("Total mesh of surface: "//tostring(obj%tEntities(2)), &
  & unitno=unitno)

CALL Display("Total mesh of curve: "//tostring(obj%tEntities(1)), &
  & unitno=unitno)

CALL Display("Total mesh of point: "//tostring(obj%tEntities(0)), &
  & unitno=unitno)

SELECT CASE (obj%nsd)
CASE (3)
  abool = ASSOCIATED(obj%meshVolume)
  CALL Display(abool, "meshVolume ASSOCIATED: ", unitno=unitno)
  IF (abool) THEN
    CALL obj%meshVolume%DisplayMeshInfo("Volume Mesh Info:", unitno=unitno)
  END IF
CASE (2)
  abool = ASSOCIATED(obj%meshSurface)
  CALL Display(abool, "meshSurface ASSOCIATED: ", unitno=unitno)
  IF (abool) THEN
    CALL obj%meshSurface%DisplayMeshInfo("Surface Mesh Info:", unitno=unitno)
  END IF
CASE (1)
  abool = ASSOCIATED(obj%meshCurve)
  CALL Display(abool, "meshCurve ASSOCIATED: ", unitno=unitno)
  IF (abool) THEN
    CALL obj%meshCurve%DisplayMeshInfo("Curve Mesh Info:", unitno=unitno)
  END IF
CASE (0)
  abool = ASSOCIATED(obj%meshPoint)
  CALL Display(abool, "meshPoint ASSOCIATED: ", unitno=unitno)
  IF (abool) THEN
    CALL obj%meshPoint%DisplayMeshInfo("Point Mesh Info:", unitno=unitno)
  END IF
END SELECT

END PROCEDURE obj_DisplayDomainInfo

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "AbstractDomain_Import()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling AbstractDomainImportCheckErr()')
#endif

CALL AbstractDomainImportCheckErr(obj=obj, hdf5=hdf5, myName=myName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling AbstractDomainImportMetaData')
#endif

CALL AbstractDomainImportMetaData(obj=obj, hdf5=hdf5, group=group, &
                                  myName=myName)

IF (obj%nsd .EQ. 3_I4B) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'Importing meshVolume')
#endif

  obj%meshVolume => FEMesh_Pointer()
  CALL obj%meshVolume%Initiate(hdf5=hdf5, group=group, dim=3_I4B)
  obj%tElements(3) = obj%meshVolume%GetTotalElements()
END IF

IF (obj%nsd .GT. 1_I4B) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'Importing meshSurface')
#endif

  obj%meshSurface => FEMesh_Pointer()
  CALL obj%meshSurface%Initiate(hdf5=hdf5, group=group, dim=2_I4B)
  obj%tElements(2) = obj%meshSurface%GetTotalElements()

END IF

IF (obj%nsd .GE. 1_I4B) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'Importing meshCurve')
#endif

  obj%meshCurve => FEMesh_Pointer()
  CALL obj%meshCurve%Initiate(hdf5=hdf5, group=group, dim=1_I4B)
  obj%tElements(1) = obj%meshCurve%GetTotalElements()

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Importing meshPoint')
#endif

obj%meshPoint => FEMesh_Pointer()
CALL obj%meshPoint%Initiate(hdf5=hdf5, group=group, dim=0_I4B)
obj%tElements(0) = obj%meshPoint%GetTotalElements()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                               AbstractDomainImportCheckErr
!----------------------------------------------------------------------------

SUBROUTINE AbstractDomainImportCheckErr(obj, hdf5, myName)
  CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: myName

  ! internal variable
  LOGICAL(LGT) :: problem

  problem = obj%isInitiated

  IF (problem) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
      & "[INTERNAL ERROR] :: AbstractDomain_Class::obj is already initiated.")
    RETURN
  END IF

  problem = .NOT. hdf5%isOpen()
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR] :: HDF5 file is not opened')
    RETURN
  END IF

  problem = .NOT. hdf5%isRead()
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file does not have read permission')
    RETURN
  END IF
END SUBROUTINE AbstractDomainImportCheckErr

!----------------------------------------------------------------------------
!                                               AbstractDomainImportMetaData
!----------------------------------------------------------------------------

SUBROUTINE AbstractDomainImportMetaData(obj, hdf5, group, myName)
  CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: myName

  obj%isInitiated = .TRUE.

  ! read engine
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%engine, fieldname="engine", myName=myName, modName=modName)

  ! read majorVersion
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%majorVersion, fieldname="majorVersion", myName=myName,  &
    & modName=modName)

  ! read minorVersion
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%minorVersion, fieldname="minorVersion", myName=myName,  &
    & modName=modName)

  ! read version
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%version, fieldname="version", myName=myName,  &
    & modName=modName)

  ! read NSD
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%NSD, fieldname="NSD", myName=myName,  &
    & modName=modName)

  ! maxNptrs
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%maxNptrs, fieldname="maxNptrs", myName=myName,  &
    & modName=modName)

  ! minNptrs
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%minNptrs, fieldname="minNptrs", myName=myName,  &
    & modName=modName)

  ! tNodes
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%tNodes, fieldname="tNodes", myName=myName,  &
    & modName=modName)

  ! nodeCoord
  CALL HDF5ReadMatrix(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%nodeCoord, fieldname="nodeCoord", myName=myName,  &
    & modName=modName)

  ! is node number sparse
  IF ((obj%maxNptrs - obj%minNptrs) .EQ. (obj%tNodes - 1)) THEN
    obj%isNodeNumberSparse = .FALSE.
  ELSE
    obj%isNodeNumberSparse = .TRUE.
  END IF

  ! maxElemNum
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%maxElemNum, fieldname="maxElemNum", myName=myName,  &
    & modName=modName)

  ! minElemNum
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%minElemNum, fieldname="minElemNum", myName=myName,  &
    & modName=modName)

  ! tEntitiesForNodes
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%tEntitiesForNodes, fieldname="tEntitiesForNodes",  &
    & myName=myName, modName=modName)

  ! tEntitiesForElements
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%tEntitiesForElements, fieldname="tEntitiesForElements",  &
    & myName=myName, modName=modName)

  ! numVolumeEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%tEntities(3), fieldname="numVolumeEntities",  &
    & myName=myName, modName=modName)

  ! numSurfaceEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%tEntities(2), fieldname="numSurfaceEntities",  &
    & myName=myName, modName=modName)

  ! numCurveEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%tEntities(1), fieldname="numCurveEntities",  &
    & myName=myName, modName=modName)

  ! numPointEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%tEntities(0), fieldname="numPointEntities",  &
    & myName=myName, modName=modName)

END SUBROUTINE AbstractDomainImportMetaData

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "AbstractDomain_ImportFromToml()"
TYPE(HDF5File_) :: meshfile
CHARACTER(:), ALLOCATABLE :: meshfilename, ext, group
CHARACTER(*), PARAMETER :: default_meshfilename = "mesh.h5"
CHARACTER(*), PARAMETER :: default_group = ""
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL toml_get(table, "filename", meshfilename, default_meshfilename,  &
  & origin=origin, stat=stat)

ext = getExtension(meshfilename)
problem = .NOT. ext .EQ. "h5"

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: given filename is not HDF5File. '//  &
    & 'Extension should be "h5"')
END IF

CALL toml_get(table, "group", group, default_group,  &
  & origin=origin, stat=stat)

CALL meshfile%Initiate(meshfilename, mode="READ")
CALL meshfile%OPEN()
CALL obj%IMPORT(hdf5=meshfile, group=group)
CALL meshfile%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "AbstractDomain_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE.,  &
  & stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: following error occured while reading '//  &
    & 'the toml file :: cannot find '//tomlName//" table in config.")
END IF

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
CALL Display(toml_serialize(node), "AbstractDomain toml config: "//CHAR_LF,  &
            & unitno=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
