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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE( Domain_Class ) DomainMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Import
  INTEGER( I4B ) :: tEntities, ii, jj, kk
  TYPE( String ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE :: intvec( : )
  TYPE( MeshPointer_ ) :: meshObj
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_Import"

  !> read full domain data
  !> check
  CALL eDomain%raiseInformation(modName//"::"//myName//" - "// &
    & "Importing domain")

  IF( obj%isInitiated ) THEN
    CALL eDomain%raiseError(modName//"::"//myName//" - "// &
      & "DomainData is already initiated.")
  ELSE
    obj%isInitiated = .TRUE.
  END IF

  CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
    & "isInitiated = .TRUE." )

  !> check
  IF( .NOT. meshFile%isOpen() ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & 'HDF5 file is not opened')
  END IF

  !> check
  IF( .NOT. meshFile%isRead() ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & 'HDF5 file does not have read permission')
  END IF

  !> read engine
  IF( .NOT. meshFile%pathExists("/engine") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/engine path does not exists' )
  ELSE
    CALL meshFile%read( "/engine", obj%engine )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
    & "engine = "//trim(obj%engine) )
  END IF

  !> read majorversion
  IF( .NOT. meshFile%pathExists("/majorVersion") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/majorVersion path does not exists' )
  ELSE
    CALL meshFile%read( "/majorVersion", obj%majorVersion )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "majorVersion = "//trim( str(obj%majorVersion, .true.) ) )
  END IF

  !> read minor version
  IF( .NOT. meshFile%pathExists("/minorVersion") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/minorVersion path does not exists' )
  ELSE
    CALL meshFile%read( "/minorVersion", obj%minorVersion )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "minorVersion = "//trim( str(obj%minorVersion, .true.) ) )
  END IF

  !> read version
  IF( .NOT. meshFile%pathExists("/version") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/version path does not exists' )
  ELSE
    CALL meshFile%read( "/version", obj%version )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "version = "//trim( str(obj%version) ) )
  END IF

  !> read NSD
  IF( .NOT. meshFile%pathExists("/NSD") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/NSD path does not exists' )
  ELSE
    CALL meshFile%read( "/NSD", obj%NSD )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "NSD = "//trim( str(obj%NSD, .true.) ) )
  END IF

  !> maxNptrs
  IF( .NOT. meshFile%pathExists("/maxNptrs") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/maxNptrs path does not exists' )
  ELSE
    CALL meshFile%read( "/maxNptrs", obj%maxNptrs )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "maxNptrs = "//trim( str(obj%maxNptrs, .true.) ) )
  END IF

  !> minNptrs
  IF( .NOT. meshFile%pathExists("/minNptrs") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/minNptrs path does not exists' )
  ELSE
    CALL meshFile%read( "/minNptrs", obj%minNptrs )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "minNptrs = "//trim( str(obj%minNptrs, .true.) ) )
  END IF

  !> tNodes
  IF( .NOT. meshFile%pathExists("/tNodes") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/tNodes path does not exists' )
  ELSE
    CALL meshFile%read( "/tNodes", obj%tNodes )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "tNodes = "//trim( str(obj%tNodes, .true.) ) )
  END IF

  !> is node number sparse
  IF( ( obj%maxNptrs - obj%minNptrs ) .EQ. ( obj%tNodes - 1 ) ) THEN
    obj%isNodeNumberSparse = .FALSE.
  ELSE
    obj%isNodeNumberSparse = .TRUE.
  END IF

  !> maxElemNum
  IF( .NOT. meshFile%pathExists("/maxElemNum") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/maxElemNum path does not exists' )
  ELSE
    CALL meshFile%read( "/maxElemNum", obj%maxElemNum )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "maxElemNum = "//trim( str(obj%maxElemNum, .true.) ) )
  END IF

  !> minElemNum
  IF( .NOT. meshFile%pathExists("/minElemNum") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/minElemNum path does not exists' )
  ELSE
    CALL meshFile%read( "/minElemNum", obj%minElemNum )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "minElemNum = "//trim( str(obj%minElemNum, .true.) ) )
  END IF

  !> tEntitiesForNodes
  IF( .NOT. meshFile%pathExists("/tEntitiesForNodes") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/tEntitiesForNodes path does not exists' )
  ELSE
    CALL meshFile%read( "/tEntitiesForNodes", obj%tEntitiesForNodes )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "tEntitiesForNodes = "//trim( str(obj%tEntitiesForNodes, .true.) ) )
  END IF

  !> tEntitiesForElements
  IF( .NOT. meshFile%pathExists("/tEntitiesForElements") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/tEntitiesForElements path does not exists' )
  ELSE
    CALL meshFile%read( "/tEntitiesForElements", obj%tEntitiesForElements )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "tEntitiesForElements = "//trim( str(obj%tEntitiesForElements, .true.) ) )
  END IF

  !> numVolumeEntities
  IF( .NOT. meshFile%pathExists("/numVolumeEntities") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/numVolumeEntities path does not exists' )
  ELSE
    CALL meshFile%read( "/numVolumeEntities", obj%tEntities( 3 ) )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "numVolumeEntites = "//trim( str(obj%tEntities( 3 ), .true.) ) )
  END IF

  !> numSurfaceEntities
  IF( .NOT. meshFile%pathExists("/numSurfaceEntities") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/numSurfaceEntities path does not exists' )
  ELSE
    CALL meshFile%read( "/numSurfaceEntities", obj%tEntities( 2 ) )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "numSurfaceEntites = "//trim( str(obj%tEntities( 2 ), .true.) ) )
  END IF

  !> numCurveEntities
  IF( .NOT. meshFile%pathExists("/numCurveEntities") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/numCurveEntities path does not exists' )
  ELSE
    CALL meshFile%read( "/numCurveEntities", obj%tEntities( 1 ) )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "numCurveEntites = "//trim( str(obj%tEntities( 1 ), .true.) ) )
  END IF

  !> numPointEntities
  IF( .NOT. meshFile%pathExists("/numPointEntities") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/numPointEntities path does not exists' )
  ELSE
    CALL meshFile%read( "/numPointEntities", obj%tEntities( 0 ) )
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "numPointEntites = "//trim( str(obj%tEntities( 0 ), .true.) ) )
  END IF

  !> PhysicalNames/NSD
  CALL eDomain%raiseInformation(modName//"::"//myName//" - "// &
    & "reading NSDVec")
  IF( .NOT. meshFile%pathExists("/PhysicalNames/NSD") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/PhysicalNames/NSD path does not exists' )
  ELSE
    CALL meshFile%read( "/PhysicalNames/NSD", obj%NSDVec )
    IF( eDomain%isLogActive() ) CALL Display( obj%NSDVec, "NSD(:)=", unitNo=eDomain%getLogFileUnit() )
  END IF

  !> PhysicalNames/tag
  CALL eDomain%raiseInformation(modName//"::"//myName//" - "// &
    & "reading tag")
  IF( .NOT. meshFile%pathExists("/PhysicalNames/tag") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/PhysicalNames/tag path does not exists' )
  ELSE
    CALL meshFile%read( "/PhysicalNames/tag", obj%tag )
    IF( eDomain%isLogActive() ) CALL Display( obj%tag, &
      & "tag(:)=", unitNo=eDomain%getLogFileUnit() )
  END IF

  !> PhysicalNames/numElements
  CALL eDomain%raiseInformation(modName//"::"//myName//" - "// &
    & "reading numElements")
  IF( .NOT. meshFile%pathExists("/PhysicalNames/numElements") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/PhysicalNames/numElements path does not exists' )
  ELSE
    CALL meshFile%read( "/PhysicalNames/numElements", obj%numElements )
    IF( eDomain%isLogActive() ) CALL Display( obj%numElements, &
      & "numElements(:)=", unitNo=eDomain%getLogFileUnit() )
  END IF

  !> PhysicalNames/numNodes
  CALL eDomain%raiseInformation(modName//"::"//myName//" - "// &
    & "reading numNodes")
  IF( .NOT. meshFile%pathExists("/PhysicalNames/numNodes") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/PhysicalNames/numNodes path does not exists' )
  ELSE
    CALL meshFile%read( "/PhysicalNames/numNodes", obj%numNodes )
    IF( eDomain%isLogActive() ) CALL Display( obj%numNodes, &
      & "numNodes(:)=", unitNo=eDomain%getLogFileUnit() )
  END IF

  !> PhysicalNames/totalPhysicalEntities
  CALL eDomain%raiseInformation(modName//"::"//myName//" - "// &
    & "reading totalPhysicalEntities")
  IF( .NOT. meshFile%pathExists("/PhysicalNames/totalPhysicalEntities") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/PhysicalNames/totalPhysicalEntities path does not exists' )
  ELSE
    CALL meshFile%read( "/PhysicalNames/totalPhysicalEntities", tEntities )
  END IF

  CALL eDomain%raiseInformation(modName//"::"//myName//" - "// &
    & "Allocatting entities")
  ALLOCATE( obj%entities( tEntities ) )

  DO ii = 1, tEntities
    !> PhysicalNames/totalPhysicalEntities
    dsetname = "/PhysicalNames/entities_" // TRIM(str(ii,.true.))
    CALL eDomain%raiseInformation(modName//"::"//myName//" - "// &
      & "Reading "//TRIM(dsetname) )
    IF( .NOT. meshFile%pathExists(TRIM(dsetname%chars())) ) THEN
      CALL eDomain%raiseError(modName//'::'//myName// &
        &  TRIM(dsetname%chars()) // 'path does not exists' )
    ELSE
      CALL meshFile%read( TRIM(dsetname%chars()), intvec )
      obj%entities( ii ) = IntVector( intvec )
      IF( eDomain%isLogActive() ) CALL Display( intvec, dsetname//'=', &
        & unitNo = eDomain%getLogFileUnit() )
      IF( ALLOCATED( intvec ) ) DEALLOCATE( intvec )
    END IF
  END DO

  !> PhysicalNames/physicalName
  CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
    & "Reading physical names" )
  IF( .NOT. meshFile%pathExists("/PhysicalNames/physicalName") ) THEN
    CALL eDomain%raiseError(modName//'::'//myName// &
      & '/PhysicalNames/physicalName path does not exists' )
  ELSE
    CALL meshFile%read( "/PhysicalNames/physicalName", obj%physicalName )
    IF( eDomain%isLogActive() ) THEN
      DO ii = 1, SIZE(obj%physicalName)
        CALL Display( obj%physicalName(ii), &
          & dsetname//'=', unitNo = eDomain%getLogFileUnit() )
      END DO
    END IF
  END IF

  dsetname = ''

  CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
    & "Storing total number of elements" )
  DO ii = 0, 3
    obj%tElements( ii ) = SUM( obj%numElements, obj%NSDVec .EQ. ii )
  END DO
  CALL Display( obj%tElements, 'tElements =', &
    & unitNo = eDomain%getLogFileUnit() )

  !> set the sizes of meshes of point, curve, surface, volume entities
  CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
    & "Allocating obj%meshList" )
  ALLOCATE( obj%meshList( 0:3 ) )
  DO ii = 0, 3
    CALL obj%meshList(ii)%initiate( )
  END DO

  ! > Handling point entities
  meshObj%ptr => NULL()
  DO jj = 0, 3
    DO ii = 1, obj%tEntities( jj )
      CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
        & "Adding mesh entity xidim="//TRIM(str(jj,.true.)) // &
        & " entry number=" // TRIM(str(ii,.true.)) )
      meshObj%ptr => Mesh_Pointer(meshFile, xidim=jj, id=ii )
      IF( .NOT. ASSOCIATED( meshObj%ptr ) ) &
        & CALL eDomain%raiseError(modName//'::'//myName// &
        & 'some of the mesh entities are not associated' )
      CALL obj%meshList( jj )%pushback( meshObj )
    END DO
  END DO

  NULLIFY( meshObj%ptr )
END PROCEDURE Domain_Import

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Initiate
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_Initiate"
  CLASS( ExceptionHandler_ ), POINTER :: surr
  LOGICAL( LGT ) :: exist, opened

  !> setting up exception messages output settings
  surr => NULL()
  CALL eDomain%getSurrogate(surr)
  IF( .NOT. ASSOCIATED( surr ) ) THEN
    CALL eDomain%setQuietMode( .TRUE. )
    CALL eDomain%setStopOnError( .TRUE. )
    INQUIRE(file=eLogFile, exist=exist, opened=opened )
    IF( exist ) THEN
      IF( .NOT. opened ) OPEN( Unit=eUnitNo, FILE=eLogFile, &
        & POSITION='APPEND',STATUS='OLD', &
        & ACTION="WRITE" )
      CALL eDomain%setLogFileUnit( eUnitNo )
      CALL eDomain%setLogActive( .TRUE. )
    ELSE
      OPEN( Unit=eUnitNo, FILE=eLogFile, &
        & ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='REPLACE' )
      CALL eDomain%setLogFileUnit( eUnitNo )
      CALL eDomain%setLogActive( .TRUE. )
    END IF
  END IF
  surr => NULL()

  ! > Exception related to Mesh_ data type wil be printed in the
  ! domain only
  CALL eMesh%addSurrogate(eDomain)
  CALL eDomain%raiseInformation( modName//'::'//myName//'-'// &
    & 'Initiating domain' )
  CALL obj%import( meshFile )
  CALL eDomain%raiseInformation( modName//'::'//myName//'-'// &
    & 'Domain has been initiated' )

  !> now we are going to fix the nodal coordinates
  CALL eDomain%raiseInformation( modName//'::'//myName//'-'// &
    & 'Fixing nodal coordinates' )
  CALL FixNodeCoord( obj )
  CALL eDomain%raiseInformation( modName//'::'//myName//'-'// &
    & 'Nodal coordinates fixed' )
END PROCEDURE Domain_Initiate

!----------------------------------------------------------------------------
!                                                            FixNodeCoord
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Fix the nodal coordinates
!
!### Introduction
!
! When we import the domain, then nodeCoord in each mesh contains only the
! internal nodal coordinates.

SUBROUTINE FixNodeCoord(obj)
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
  ! Internal variables
  INTEGER( I4B ) :: ii, tMesh, imesh, tNodes, xidim, count
  LOGICAL( LGT ) :: isEmpty
  TYPE( MeshPointerIterator_ ) :: iterator
  CHARACTER( LEN = * ), PARAMETER :: myName = "FixNodeCoord"
  INTEGER( I4B ), ALLOCATABLE :: nptrs( :, : ), internalNptrs( : )
  REAL( DFP ), ALLOCATABLE :: nodeCoord( :, : )

  CALL Reallocate( obj%local_nptrs, obj%maxNptrs )
  CALL Reallocate( obj%nodeCoord, 3, obj%tNodes )
  count = 0
  !> getting node numbers from mesh of point entities
  DO xidim = 0, 3
    isEmpty = obj%meshList( xidim )%isEmpty()
    IF( .NOT. isEmpty ) THEN
      !> Get total number of meshes presented
      tMesh = obj%meshList( xidim )%size()
      !> Get the first mesh
      iterator = obj%meshList( xidim )%Begin()
      DO imesh = 1, tMesh
        IF( .NOT. ASSOCIATED( iterator%value%ptr ) ) &
          & CALL eDomain%raiseError(modName//'::'//myName//'-'// &
          & 'some of the meshes of point entities is not associated' )
        internalNptrs = iterator%value%ptr%getInternalNptrs()
        nodeCoord = iterator%value%ptr%getNodeCoord()
        DO ii = 1, SIZE( internalNptrs )
          count = count + 1
          obj%local_nptrs( internalNptrs( ii ) ) = count
          obj%nodeCoord(1:3, count) = nodeCoord( 1:3, ii )
        END DO
        CALL iterator%inc()
      END DO
    END IF
  END DO

  DO xidim = 1, 3
    isEmpty = obj%meshList( xidim )%isEmpty()
    IF( .NOT. isEmpty ) THEN
      !> Get total number of meshes presented
      tMesh = obj%meshList( xidim )%size()
      !> Get the first mesh
      iterator = obj%meshList( xidim )%Begin()
      DO imesh = 1, tMesh
        nptrs = iterator%value%ptr%getNptrs()
        nodeCoord = obj%nodeCoord( 1:3, nptrs( :, 1 ) )
        CALL iterator%value%ptr%setNodeCoord( nodeCoord )
        CALL iterator%inc()
      END DO
    END IF
  END DO

  IF( ALLOCATED( nodeCoord ) ) DEALLOCATE( nodeCoord )
  IF( ALLOCATED( nptrs ) ) DEALLOCATE( nptrs )
  IF( ALLOCATED( internalNptrs ) ) DEALLOCATE( internalNptrs )
  iterator%value%ptr => NULL()

END SUBROUTINE FixNodeCoord

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_DeallocateData
  INTEGER( I4B ) :: ii, jj
  obj%isInitiated = .FALSE.
  obj%engine = ''
  obj%majorVersion = 0
  obj%minorVersion = 0
  obj%version = 0.0_DFP
  obj%nsd = 0
  obj%maxNptrs = 0
  obj%minNptrs = 0
  obj%tNodes = 0
  obj%isNodeNumberSparse = .FALSE.
  obj%maxElemNum = 0
  obj%minElemNum = 0
  obj%isElemNumberSparse = .FALSE.
  obj%tEntitiesForNodes = 0
  obj%tEntitiesForElements = 0
  IF( ALLOCATED( obj%NSDVec ) ) DEALLOCATE( obj%NSDVec )
  IF( ALLOCATED( obj%tag ) ) DEALLOCATE( obj%tag )
  IF( ALLOCATED( obj%numElements ) ) DEALLOCATE( obj%numElements )
  IF( ALLOCATED( obj%numNodes ) ) DEALLOCATE( obj%numNodes )
  IF( ALLOCATED( obj%entities ) ) DEALLOCATE( obj%entities )
  IF( ALLOCATED( obj%physicalName ) ) DEALLOCATE( obj%physicalName )
  obj%tElements( 0:3 ) = 0
  obj%tEntities( 0:3 ) = 0
  IF( ALLOCATED( obj%meshList ) ) DEALLOCATE( obj%meshList )
  IF( ALLOCATED( obj%nodeCoord ) ) DEALLOCATE( obj%nodeCoord )
  IF( ALLOCATED( obj%local_nptrs ) ) DEALLOCATE( obj%local_nptrs )
END PROCEDURE Domain_DeallocateData

!----------------------------------------------------------------------------
!                                                              Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Final
  CALL Obj%DeallocateData()
END PROCEDURE Domain_Final

!----------------------------------------------------------------------------
!                                                             Domain
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Constructor1
  CALL ans%initiate( meshFile )
END PROCEDURE Domain_Constructor1

!----------------------------------------------------------------------------
!                                                            Domain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Constructor_1
  ALLOCATE( ans )
  CALL ans%initiate( meshFile )
END PROCEDURE Domain_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE DomainMethods