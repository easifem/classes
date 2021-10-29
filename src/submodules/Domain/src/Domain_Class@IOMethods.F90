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
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Importing domain")
  IF( obj%isInitiated ) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "DomainData is already initiated.")
  ELSE
    obj%isInitiated = .TRUE.
  END IF
  CALL e%raiseInformation( modName//"::"//myName//" - "// &
    & "isInitiated = .TRUE." )
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'HDF5 file is not opened')
  END IF
  !> check
  IF( .NOT. hdf5%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have read permission')
  END IF
  !> read engine
  dsetname=trim(group)//"/engine"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // 'path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%engine )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
    & "engine = "//trim(obj%engine) )
  END IF
  !> read majorversion
  dsetname=trim(group)//"/majorVersion"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%majorVersion )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "majorVersion = "//trim( str(obj%majorVersion, .true.) ) )
  END IF
  !> read minor version
  dsetname=trim(group)//"/minorVersion"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%minorVersion )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "minorVersion = "//trim( str(obj%minorVersion, .true.) ) )
  END IF
  !> read version
  dsetname=trim(group)//"/version"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%version )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "version = "//trim( str(obj%version) ) )
  END IF
  !> read NSD
  dsetname=trim(group)//"/NSD"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%NSD )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "NSD = "//trim( str(obj%NSD, .true.) ) )
  END IF
  !> maxNptrs
  dsetname=trim(group)//"/maxNptrs"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%maxNptrs )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "maxNptrs = "//trim( str(obj%maxNptrs, .true.) ) )
  END IF
  !> minNptrs
  dsetname=trim(group)//"/minNptrs"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%minNptrs )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "minNptrs = "//trim( str(obj%minNptrs, .true.) ) )
  END IF
  !> tNodes
  dsetname=trim(group)//"/tNodes"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%tNodes )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "tNodes = "//trim( str(obj%tNodes, .true.) ) )
  END IF
  !> nodeCoord
  dsetname=trim(group)//"/nodeCoord"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%nodeCoord )
    IF( e%isLogActive() ) &
      & CALL Display( obj%nodeCoord, 'nodeCoord = ', &
      & unitNo = e%getLogFileUnit() )
  END IF
  !> local_nptrs
  dsetname=trim(group)//"/local_nptrs"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%local_nptrs )
    IF( e%isLogActive() ) &
      & CALL Display( obj%local_nptrs, 'local_nptrs = ', &
      & unitNo = e%getLogFileUnit() )
  END IF
  !> is node number sparse
  IF( ( obj%maxNptrs - obj%minNptrs ) .EQ. ( obj%tNodes - 1 ) ) THEN
    obj%isNodeNumberSparse = .FALSE.
  ELSE
    obj%isNodeNumberSparse = .TRUE.
  END IF
  !> maxElemNum
  dsetname=trim(group)//"/maxElemNum"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%maxElemNum )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "maxElemNum = "//trim( str(obj%maxElemNum, .true.) ) )
  END IF
  !> minElemNum
  dsetname=trim(group)//"/minElemNum"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%minElemNum )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "minElemNum = "//trim( str(obj%minElemNum, .true.) ) )
  END IF
  !> tEntitiesForNodes
  dsetname=trim(group)//"/tEntitiesForNodes"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%tEntitiesForNodes )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "tEntitiesForNodes = "//trim( str(obj%tEntitiesForNodes, .true.) ) )
  END IF
  !> tEntitiesForElements
  dsetname=trim(group)//"/tEntitiesForElements"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%tEntitiesForElements )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "tEntitiesForElements = "  &
      & //trim( str(obj%tEntitiesForElements, .true.) ) )
  END IF
  !> numVolumeEntities
  dsetname=trim(group)//"/numVolumeEntities"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%tEntities( 3 ) )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "numVolumeEntites = "//trim( str(obj%tEntities( 3 ), .true.) ) )
  END IF
  !> numSurfaceEntities
  dsetname=trim(group)//"/numSurfaceEntities"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%tEntities( 2 ) )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "numSurfaceEntites = "//trim( str(obj%tEntities( 2 ), .true.) ) )
  END IF
  !> numCurveEntities
  dsetname=trim(group)//"/numCurveEntities"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%tEntities( 1 ) )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "numCurveEntites = "//trim( str(obj%tEntities( 1 ), .true.) ) )
  END IF
  !> numPointEntities
  dsetname=trim(group)//"/numPointEntities"
  IF( .NOT. hdf5%pathExists( dsetname%chars() ) ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & dsetname%chars() // ' path does not exists' )
  ELSE
    CALL hdf5%read( dsetname%chars(), obj%tEntities( 0 ) )
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "numPointEntites = "//trim( str(obj%tEntities( 0 ), .true.) ) )
  END IF
  !> set the sizes of meshes of point, curve, surface, volume entities
  CALL e%raiseInformation( modName//"::"//myName//" - "// &
    & "ALLOCATING obj%meshList" )
  ALLOCATE( obj%meshList( 0:3 ) )
  DO ii = 0, 3
    CALL obj%meshList(ii)%initiate( )
  END DO
  ! > Handling point entities
  meshObj%ptr => NULL(); obj%tElements( 0: ) = 0
  DO jj = 0, 3
    DO ii = 1, obj%tEntities( jj )
      CALL e%raiseInformation( modName//"::"//myName//" - "// &
        & "Adding mesh entity xidim="//TRIM(str(jj,.true.)) // &
        & " entry number=" // TRIM(str(ii,.true.)) )
      SELECT CASE( jj )
      CASE( 0 )
      dsetname = trim(group)//"/pointEntities_"//TRIM(str(ii, .true.) )
      CASE( 1 )
      dsetname = trim(group)//"/curveEntities_"//TRIM(str(ii, .true.) )
      CASE( 2 )
      dsetname = trim(group)//"/surfaceEntities_"//TRIM(str(ii, .true.) )
      CASE( 3 )
      dsetname = trim(group)//"/volumeEntities_"//TRIM(str(ii, .true.) )
      END SELECT
      meshObj%ptr => Mesh_Pointer(hdf5=hdf5, group=dsetname%chars())
      IF( .NOT. ASSOCIATED( meshObj%ptr ) ) &
        & CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'some of the mesh entities are not associated' )
      CALL obj%meshList( jj )%pushback( meshObj )
      obj%tElements( jj ) = obj%tElements( jj )+meshObj%ptr%getTotalElements()
    END DO
  END DO
  IF( e%isLogActive() ) &
      & CALL Display( obj%tElements, 'tElements =', &
      & unitNo = e%getLogFileUnit() )
  NULLIFY( meshObj%ptr )
END PROCEDURE Domain_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods