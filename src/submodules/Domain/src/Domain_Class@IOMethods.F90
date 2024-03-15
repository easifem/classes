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

SUBMODULE(Domain_Class) IOMethods
USE Display_Method
USE StringUtility
USE ReallocateUtility
USE tomlf, ONLY: toml_serialize, toml_get => get_value
USE TomlUtility
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Display
CALL Display(obj%isInitiated, "Domain_::obj Initiated: ", unitno=unitno)
IF (.NOT. obj%isInitiated) RETURN

CALL Display("engine: "//obj%engine, unitno=unitno)
CALL Display("majorVersion: "//tostring(obj%majorVersion), unitno=unitNo)
CALL Display("minorVersion: "//tostring(obj%minorVersion), unitno=unitNo)
CALL Display("version: "//tostring(obj%version), unitno=unitNo)
CALL Display("nsd: "//tostring(obj%nsd), unitno=unitNo)
CALL Display("maxNptrs: "//tostring(obj%maxNptrs), unitno=unitNo)
CALL Display("minNptrs: "//tostring(obj%minNptrs), unitno=unitNo)
CALL Display("tNodes: "//tostring(obj%tNodes), unitno=unitNo)
CALL Display(obj%isNodeNumberSparse, "isNodeNumberSparse: ", unitno=unitNo)
CALL Display("maxElemNum: "//tostring(obj%maxElemNum), unitno=unitNo)
CALL Display("minElemNum: "//tostring(obj%minElemNum), unitno=unitNo)
CALL Display(obj%isElemNumberSparse, "isElemNumberSparse: ", unitno=unitNo)
CALL Display("tEntitiesForNodes: "//tostring(obj%tEntitiesForNodes), &
  & unitno=unitNo)
CALL Display("tEntitiesForElements: "//tostring(obj%tEntitiesForElements), &
  & unitno=unitNo)
CALL Display("tEntitiesForElements: "//tostring(obj%tEntitiesForElements), &
  & unitno=unitNo)
CALL Display("tElements: "//tostring(obj%tElements), &
  & unitno=unitno)
CALL Display("tEntities: "//tostring(obj%tEntities), &
  & unitno=unitno)
CALL Display(ALLOCATED(obj%nodeCoord), "nodeCoord Allocated: ", &
  &  unitno=unitno)
CALL Display(ALLOCATED(obj%local_nptrs), "local_nptrs Allocated: ", &
  & unitno=unitno)
CALL Display(ALLOCATED(obj%global_nptrs), "global_nptrs Allocated: ", &
  & unitno=unitno)

CALL Display(ALLOCATED(obj%meshVolume), "meshVolume Allocated: ", &
  & unitno=unitno)

CALL Display(ALLOCATED(obj%meshSurface), "meshSurface Allocated: ", &
  & unitno=unitno)

CALL Display(ALLOCATED(obj%meshCurve), "meshCurve Allocated: ", &
  & unitno=unitno)

CALL Display(ALLOCATED(obj%meshPoint), "meshPoint Allocated: ", &
  & unitno=unitno)

CALL Display(ALLOCATED(obj%meshFacetData), "meshFacetData Allocated: ", &
  & unitno=unitno)
CALL Display(obj%meshMap%isInitiated, "meshMap Initiated: ", unitno=unitno)

END PROCEDURE Domain_Display

!----------------------------------------------------------------------------
!                                                       DisplayMeshFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_DisplayMeshFacetData
INTEGER(I4B) :: telements, ii
LOGICAL(LGT) :: abool

CALL Display(msg, unitNo=unitNo)

abool = ALLOCATED(obj%meshFacetData)
CALL Display(abool, "meshFacetData Allocated: ", unitNo=unitNo)

IF (abool) THEN
  telements = SIZE(obj%meshFacetData)
  DO ii = 1, telements
    CALL obj%meshFacetData(ii)%Display( &
      & msg="meshFacetData( "//tostring(ii) &
      & //" ): ", unitno=unitno)
    CALL BlankLines(nol=2, unitno=unitno)
  END DO
END IF
END PROCEDURE Domain_DisplayMeshFacetData

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacetData_Display
LOGICAL(LGT) :: abool

CALL Display(msg, unitno=unitno)

CALL Display("elementType: BOUNDARY_ELEMENT", unitno=unitno)

CALL Display(obj%masterMesh, "masterMesh: ", unitno=unitno)

CALL Display(obj%slaveMesh, "slaveMesh: ", unitno=unitno)

abool = ALLOCATED(obj%masterCellNumber)
CALL Display(abool, "masterCellNumber Allocated: ", unitNo=unitNo)

IF (abool) THEN
  CALL Display(obj%masterCellNumber, msg="masterCellNumber: ", &
    & unitno=unitno)
END IF

abool = ALLOCATED(obj%masterlocalFacetID)
CALL Display(abool, "masterlocalFacetID Allocated: ", unitNo=unitNo)

IF (abool) THEN
  CALL Display(obj%masterlocalFacetID, msg="masterlocalFacetID: ", &
    & unitno=unitno)
END IF

abool = ALLOCATED(obj%slaveCellNumber)
CALL Display(abool, "slaveCellNumber Allocated: ", unitNo=unitNo)

IF (abool) THEN
  CALL Display(obj%slaveCellNumber, msg="slaveCellNumber: ", &
    & unitno=unitno)
END IF

abool = ALLOCATED(obj%slavelocalFacetID)
IF (abool) THEN
  CALL Display(obj%slavelocalFacetID, msg="slavelocalFacetID: ", &
    & unitno=unitno)
END IF

END PROCEDURE MeshFacetData_Display

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Import
CHARACTER(*), PARAMETER :: myName = "Domain_Import()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling DomainImportCheckErr()')
#endif

CALL DomainImportCheckErr(obj=obj, hdf5=hdf5, myName=myName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling DomainImportMetaData')
#endif

CALL DomainImportMetaData(obj=obj, hdf5=hdf5, group=group, myName=myName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling DomainImportMesh() for volume')
#endif

CALL DomainImportMesh(obj=obj, hdf5=hdf5, group=group, myName=myName,  &
  & nsd=3_I4B, ent_name="/volumeEntities_", meshes=obj%meshVolume)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling DomainImportMesh() for surface')
#endif

CALL DomainImportMesh(obj=obj, hdf5=hdf5, group=group, myName=myName,  &
  & nsd=2_I4B, ent_name="/surfaceEntities_", meshes=obj%meshSurface)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling DomainImportMesh() for curve')
#endif

CALL DomainImportMesh(obj=obj, hdf5=hdf5, group=group, myName=myName,  &
  & nsd=1_I4B, ent_name="/curveEntities_", meshes=obj%meshCurve)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling DomainImportMesh() for point')
#endif

CALL DomainImportMesh(obj=obj, hdf5=hdf5, group=group, myName=myName,  &
  & nsd=0_I4B, ent_name="/pointEntities_", meshes=obj%meshPoint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE Domain_Import

!----------------------------------------------------------------------------
!                                                       DomainImportCheckErr
!----------------------------------------------------------------------------

SUBROUTINE DomainImportCheckErr(obj, hdf5, myName)
  CLASS(Domain_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: myName

  ! internal variable
  LOGICAL(LGT) :: problem

  problem = obj%isInitiated

  IF (problem) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
      & "[INTERNAL ERROR] :: Domain_Class::obj is already initiated.")
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
END SUBROUTINE DomainImportCheckErr

!----------------------------------------------------------------------------
!                                                       DomainImportMetaData
!----------------------------------------------------------------------------

SUBROUTINE DomainImportMetaData(obj, hdf5, group, myName)
  CLASS(Domain_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: myName

  ! internal variables
  INTEGER(I4B) :: ii, jj

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

  ! local_nptrs
  CALL HDF5ReadVector(hdf5=hdf5, check=.TRUE., group=group,  &
    & VALUE=obj%local_nptrs, fieldname="local_nptrs", myName=myName,  &
    & modName=modName)

  ! global_nptrs
  CALL Reallocate(obj%global_nptrs, obj%tNodes)
  DO ii = 1, SIZE(obj%local_nptrs)
    jj = obj%local_nptrs(ii)
    IF (jj .NE. 0) THEN
      obj%global_nptrs(jj) = ii
    ELSE
      obj%global_nptrs(jj) = 0
    END IF
  END DO

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

  ALLOCATE (obj%meshVolume(obj%tEntities(3)))
  ALLOCATE (obj%meshSurface(obj%tEntities(2)))
  ALLOCATE (obj%meshCurve(obj%tEntities(1)))
  ALLOCATE (obj%meshPoint(obj%tEntities(0)))

END SUBROUTINE DomainImportMetaData

!----------------------------------------------------------------------------
!                                                        DomainImportMesh
!----------------------------------------------------------------------------

SUBROUTINE DomainImportMesh(obj, hdf5, group, myName, nsd, ent_name,  &
  & meshes)
  CLASS(Domain_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: myName
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: ent_name
  TYPE(MeshPointer_), INTENT(INOUT) :: meshes(:)

  ! internal variables
  INTEGER(I4B) :: ii
  TYPE(MeshPointer_) :: meshObj
  LOGICAL(LGT) :: problem
  TYPE(String) :: dsetname

  meshObj%ptr => NULL()
  obj%tElements(nsd) = 0

  DO ii = 1, obj%tEntities(nsd)

    dsetname = TRIM(group)//ent_name//tostring(ii)
    meshObj%ptr => Mesh_Pointer(hdf5=hdf5, group=dsetname%chars())

#ifdef DEBUG_VER
    problem = .NOT. ASSOCIATED(meshObj%ptr)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//" - "// &
        & '[INTERNAL ERROR] :: mesh for '//dsetname//" not ASSOCIATED.")
      RETURN
    END IF
#endif

    meshes(ii)%ptr => meshObj%ptr
    obj%tElements(nsd) = obj%tElements(nsd) + meshObj%ptr%GetTotalElements()

  END DO

  NULLIFY (meshObj%ptr)

END SUBROUTINE DomainImportMesh

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "Domain_ImportFromToml()"
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

END PROCEDURE Domain_ImportFromToml1

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "Domain_ImportFromToml2()"
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
  CALL Display(toml_serialize(node), "Domain toml config: "//CHAR_LF,  &
    & unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

END PROCEDURE Domain_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
