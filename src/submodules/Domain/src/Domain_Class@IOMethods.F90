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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Display
CHARACTER(*), PARAMETER :: myName = "Domain_Display"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'WIP: This method is curretly not available')
IF (.NOT. obj%isInitiated) THEN
  CALL display("Domain_::obj is Not Initiated", unitno=unitno)
  RETURN
END IF

CALL display("Domain_::obj is Initiated", unitno=unitno)
CALL display("engine = "//obj%engine, unitno=unitno)
CALL display("majorVersion = "//tostring(obj%majorVersion), unitno=unitNo)
CALL display("minorVersion = "//tostring(obj%minorVersion), unitno=unitNo)
CALL display("version = "//tostring(obj%version), unitno=unitNo)
CALL display("nsd = "//tostring(obj%nsd), unitno=unitNo)
CALL display("maxNptrs = "//tostring(obj%maxNptrs), unitno=unitNo)
CALL display("minNptrs = "//tostring(obj%minNptrs), unitno=unitNo)
CALL display("tNodes = "//tostring(obj%tNodes), unitno=unitNo)
IF (obj%isNodeNumberSparse) THEN
  CALL display("isNodeNumberSparse = TRUE", unitno=unitNo)
ELSE
  CALL display("isNodeNumberSparse = FALSE", unitno=unitNo)
END IF
CALL display("maxElemNum = "//tostring(obj%maxElemNum), unitno=unitNo)
CALL display("minElemNum = "//tostring(obj%minElemNum), unitno=unitNo)
IF (obj%isElemNumberSparse) THEN
  CALL display("isElemNumberSparse = TRUE", unitno=unitNo)
ELSE
  CALL display("isElemNumberSparse = FALSE", unitno=unitNo)
END IF
CALL display("tEntitiesForNodes = "//tostring(obj%tEntitiesForNodes), &
& unitno=unitNo)
CALL display("tEntitiesForElements = "//tostring(obj%tEntitiesForElements), &
& unitno=unitNo)
CALL display("tEntitiesForElements = "//tostring(obj%tEntitiesForElements), &
& unitno=unitNo)
CALL display("tElements = "//tostring(obj%tElements), &
& unitno=unitno)
CALL display("tEntities = "//tostring(obj%tEntities), &
& unitno=unitno)
IF (ALLOCATED(obj%nodeCoord)) THEN
  CALL display("nodeCoord is allocated", unitno=unitno)
ELSE
  CALL display("nodeCoord is NOT allocated", unitno=unitno)
END IF
IF (ALLOCATED(obj%local_nptrs)) THEN
  CALL display("local_nptrs is allocated", unitno=unitno)
ELSE
  CALL display("local_nptrs is NOT allocated", unitno=unitno)
END IF
IF (ALLOCATED(obj%global_nptrs)) THEN
  CALL display("global_nptrs is allocated", unitno=unitno)
ELSE
  CALL display("global_nptrs is NOT allocated", unitno=unitno)
END IF
IF (ALLOCATED(obj%meshList)) THEN
  CALL display("meshList is allocated", unitno=unitno)
ELSE
  CALL display("meshList is NOT allocated", unitno=unitno)
END IF
IF (ALLOCATED(obj%meshFacetData)) THEN
  CALL display("meshFacetData is allocated", unitno=unitno)
ELSE
  CALL display("meshFacetData is NOT allocated", unitno=unitno)
END IF
IF (obj%meshMap%isInitiated) THEN
  CALL display("meshMap is Initiated", unitno=unitno)
ELSE
  CALL display("meshMap is NOT Initiated", unitno=unitno)
END IF
END PROCEDURE Domain_Display

!----------------------------------------------------------------------------
!                                                       DisplayMeshFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_DisplayMeshFacetData
INTEGER(I4B) :: telements, ii
!
! main
!
CALL Display(msg, unitNo=unitNo)
!
IF (ALLOCATED(obj%meshFacetData)) THEN
  !
  telements = SIZE(obj%meshFacetData)
  !
  !
  DO ii = 1, telements
    CALL obj%meshFacetData(ii)%Display( &
      & msg="meshFacetData( "//tostring(ii) &
      & //" )=", unitno=unitno)
    CALL BlankLines(nol=2, unitno=unitno)
  END DO
ELSE
  CALL Display("# meshFacetData NOT ALLOCATED", UnitNo=UnitNo)
END IF
END PROCEDURE Domain_DisplayMeshFacetData

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacetData_Display
!
CALL Display(TRIM(msg), unitno=unitno)
!
CALL Display("# elementType=BOUNDARY_ELEMENT", unitno=unitno)
!
CALL Display(obj%masterMesh, &
  & "# masterMesh = ", unitno=unitno)
!
CALL Display(obj%slaveMesh, &
  & "# slaveMesh = ", unitno=unitno)
!
IF (ALLOCATED(obj%masterCellNumber)) THEN
  CALL Display(obj%masterCellNumber, msg="# masterCellNumber=", &
    & unitno=unitno)
ELSE
  CALL Display("# masterCellNumber NOT ALLOCATED", unitno=unitno)
END IF
!
IF (ALLOCATED(obj%masterlocalFacetID)) THEN
  CALL Display(obj%masterlocalFacetID, msg="# masterlocalFacetID=", &
    & unitno=unitno)
ELSE
  CALL Display("# masterlocalFacetID NOT ALLOCATED", unitno=unitno)
END IF
!
IF (ALLOCATED(obj%slaveCellNumber)) THEN
  CALL Display(obj%slaveCellNumber, msg="# slaveCellNumber=", &
    & unitno=unitno)
ELSE
  CALL Display("# slaveCellNumber NOT ALLOCATED", unitno=unitno)
END IF
!
IF (ALLOCATED(obj%slavelocalFacetID)) THEN
  CALL Display(obj%slavelocalFacetID, msg="# slavelocalFacetID=", &
    & unitno=unitno)
ELSE
  CALL Display("# slavelocalFacetID NOT ALLOCATED", unitno=unitno)
END IF
!
END PROCEDURE MeshFacetData_Display

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Import
INTEGER(I4B) :: ii, jj, kk
TYPE(String) :: dsetname
INTEGER(I4B), ALLOCATABLE :: intvec(:)
TYPE(MeshPointer_) :: meshObj
CHARACTER(*), PARAMETER :: myName = "Domain_Import"
!
! read full domain data
!
! Information
!
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Importing domain")
!
! check
!
IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "DomainData is already initiated.")
END IF
!
! check
!
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
END IF
!
! check
!
IF (.NOT. hdf5%isRead()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
END IF
!
!
!
obj%isInitiated = .TRUE.
!
! read engine
!
dsetname = TRIM(group)//"/engine"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//'path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%engine)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "engine = "//TRIM(obj%engine))
END IF
!
! read majorVersion
!
dsetname = TRIM(group)//"/majorVersion"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%majorVersion)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "majorVersion = "//TRIM(str(obj%majorVersion, .TRUE.)))
END IF
!
! read minorVersion
!
dsetname = TRIM(group)//"/minorVersion"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%minorVersion)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "minorVersion = "//TRIM(str(obj%minorVersion, .TRUE.)))
END IF
!
! read version
!
dsetname = TRIM(group)//"/version"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%version)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "version = "//TRIM(str(obj%version)))
END IF
!
! read NSD
!
dsetname = TRIM(group)//"/NSD"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%NSD)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "NSD = "//TRIM(str(obj%NSD, .TRUE.)))
END IF
!
! maxNptrs
!
dsetname = TRIM(group)//"/maxNptrs"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%maxNptrs)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "maxNptrs = "//TRIM(str(obj%maxNptrs, .TRUE.)))
END IF
!
! minNptrs
!
dsetname = TRIM(group)//"/minNptrs"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%minNptrs)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "minNptrs = "//TRIM(str(obj%minNptrs, .TRUE.)))
END IF
!
! tNodes
!
dsetname = TRIM(group)//"/tNodes"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%tNodes)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "tNodes = "//TRIM(str(obj%tNodes, .TRUE.)))
END IF
!
! nodeCoord
!
dsetname = TRIM(group)//"/nodeCoord"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%nodeCoord)
  IF (e%isLogActive()) &
    & CALL Display(obj%nodeCoord, 'nodeCoord = ', &
    & unitNo=e%getLogFileUnit())
END IF
!
! local_nptrs
!
dsetname = TRIM(group)//"/local_nptrs"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%local_nptrs)
  IF (e%isLogActive()) &
    & CALL Display(obj%local_nptrs, 'local_nptrs = ', &
    & unitNo=e%getLogFileUnit())
END IF
!
! global_nptrs
!
CALL Reallocate(obj%global_nptrs, obj%tNodes)
DO ii = 1, SIZE(obj%local_nptrs)
  jj = obj%local_nptrs(ii)
  IF (jj .NE. 0) THEN
    obj%global_nptrs(jj) = ii
  ELSE
    obj%global_nptrs(jj) = 0
  END IF
END DO
!
! is node number sparse
!
IF ((obj%maxNptrs - obj%minNptrs) .EQ. (obj%tNodes - 1)) THEN
  obj%isNodeNumberSparse = .FALSE.
ELSE
  obj%isNodeNumberSparse = .TRUE.
END IF
!
! maxElemNum
!
dsetname = TRIM(group)//"/maxElemNum"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%maxElemNum)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "maxElemNum = "//TRIM(str(obj%maxElemNum, .TRUE.)))
END IF
!
! minElemNum
!
dsetname = TRIM(group)//"/minElemNum"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%minElemNum)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "minElemNum = "//TRIM(str(obj%minElemNum, .TRUE.)))
END IF
!
! tEntitiesForNodes
!
dsetname = TRIM(group)//"/tEntitiesForNodes"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%tEntitiesForNodes)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "tEntitiesForNodes = "//TRIM(str(obj%tEntitiesForNodes, .TRUE.)))
END IF
!
! tEntitiesForElements
!
dsetname = TRIM(group)//"/tEntitiesForElements"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%tEntitiesForElements)
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "tEntitiesForElements = "  &
    & //TRIM(str(obj%tEntitiesForElements, .TRUE.)))
END IF
!
! numVolumeEntities
!
dsetname = TRIM(group)//"/numVolumeEntities"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%tEntities(3))
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "numVolumeEntites = "//TRIM(str(obj%tEntities(3), .TRUE.)))
END IF
!
! numSurfaceEntities
!
dsetname = TRIM(group)//"/numSurfaceEntities"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%tEntities(2))
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "numSurfaceEntites = "//TRIM(str(obj%tEntities(2), .TRUE.)))
END IF
!
! numCurveEntities
!
dsetname = TRIM(group)//"/numCurveEntities"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%tEntities(1))
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "numCurveEntites = "//TRIM(str(obj%tEntities(1), .TRUE.)))
END IF
!
! numPointEntities
!
dsetname = TRIM(group)//"/numPointEntities"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & dsetname%chars()//' path does not exists')
ELSE
  CALL hdf5%READ(dsetname%chars(), obj%tEntities(0))
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "numPointEntites = "//TRIM(str(obj%tEntities(0), .TRUE.)))
END IF
!
! set the sizes of meshes of point, curve, surface, volume entities
!
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "ALLOCATING obj%meshList")
ALLOCATE (obj%meshList(0:3))
DO ii = 0, 3
  CALL obj%meshList(ii)%initiate()
END DO
!
! Handling point entities
!
meshObj%ptr => NULL(); obj%tElements(0:) = 0
DO jj = 0, 3
  DO ii = 1, obj%tEntities(jj)
    CALL e%raiseInformation(modName//"::"//myName//" - "// &
      & "Adding mesh entity xidim="//TRIM(str(jj, .TRUE.))// &
      & " entry number="//TRIM(str(ii, .TRUE.)))
    SELECT CASE (jj)
    CASE (0)
      dsetname = TRIM(group)//"/pointEntities_"//TRIM(str(ii, .TRUE.))
    CASE (1)
      dsetname = TRIM(group)//"/curveEntities_"//TRIM(str(ii, .TRUE.))
    CASE (2)
      dsetname = TRIM(group)//"/surfaceEntities_"//TRIM(str(ii, .TRUE.))
    CASE (3)
      dsetname = TRIM(group)//"/volumeEntities_"//TRIM(str(ii, .TRUE.))
    END SELECT
    meshObj%ptr => Mesh_Pointer(hdf5=hdf5, group=dsetname%chars())
    IF (.NOT. ASSOCIATED(meshObj%ptr)) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'some of the mesh entities are not associated')
    CALL obj%meshList(jj)%pushback(meshObj)
    obj%tElements(jj) = obj%tElements(jj) + meshObj%ptr%getTotalElements()
  END DO
END DO
!   !
!   ! Setting the data of domain boundary element for cell elements
!   ! and facet elements
!   !
! CALL e%raiseInformation(modName//"::"//myName//" - "// &
!   & "Calling SetFacetElementType()")
! CALL obj%SetFacetElementType()
!   !
! CALL e%raiseInformation(modName//"::"//myName//" - "// &
!   & "Calling SetDomainFacetElement()")
! CALL obj%SetDomainFacetElement()
!
NULLIFY (meshObj%ptr)
!
! Information
!
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Importing domain [OK!]")
!
END PROCEDURE Domain_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
