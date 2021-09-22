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

SUBMODULE( MSH_Class ) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="msh_Export"
  CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'This routine is under condtruction')
END PROCEDURE msh_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="msh_Export"
  INTEGER( I4B ) :: ii, tsize,  tNodes, count
  REAL( DFP ), ALLOCATABLE :: nodeCoord( :, : )
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  TYPE( String ) :: dsetname
  !> main
  dsetname = trim(group)
  !>check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'HDF5 file is not opened')
  END IF
  !>check
  IF( .NOT. hdf5%isWrite() ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'HDF5 file does not have write permission')
  END IF

  tNodes = obj%nodes%getNumNodes()
  ALLOCATE( nodeCoord( 3, tNodes ) )
  ALLOCATE( local_nptrs( obj%Nodes%getMaxNodeTag() ) )
  count = 0
  local_nptrs = 0

  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/NSD", vals=obj%nsd)
  CALL ExportMeshFormat(obj,hdf5,dsetname)
  IF( obj%physicalNames%isInitiated ) &
    & CALL ExportMeshPhysicalNames(obj,hdf5,dsetname)
  CALL ExportMeshNodeInfo(obj,hdf5,dsetname)
  CALL ExportMeshElementInfo(obj,hdf5,dsetname)
  IF( ALLOCATED(obj%pointEntities) ) THEN
    tsize = SIZE(obj%pointEntities)
  ELSE
    tsize = 0
  END IF
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/numPointEntities", vals=tsize)
  DO ii = 1, tsize
    CALL ExportMeshEntity(obj%pointEntities(ii), hdf5, &
      & dsetname=TRIM(dsetname%chars())//"/pointEntities_" // &
      & TRIM(str(ii, .true.)), nsd=obj%nsd )
    CALL getNodeCoord( obj=obj%pointEntities(ii), nodeCoord=nodeCoord, &
      & local_nptrs=local_nptrs, count=count )
  END DO
  IF( ALLOCATED(obj%curveEntities) ) THEN
    tsize = SIZE(obj%curveEntities)
  ELSE
    tsize = 0
  END IF
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/numCurveEntities", vals=tsize)
  DO ii = 1, tsize
    CALL ExportMeshEntity(obj%curveEntities(ii), hdf5, &
      & dsetname=TRIM(dsetname%chars())//"/curveEntities_"// &
      & TRIM(str(ii, .true.)), nsd=obj%nsd)
    CALL getNodeCoord( obj=obj%curveEntities(ii), nodeCoord=nodeCoord, &
      & local_nptrs=local_nptrs, count=count )
  END DO
  IF( ALLOCATED(obj%surfaceEntities) ) THEN
    tsize = SIZE(obj%surfaceEntities)
  ELSE
    tsize = 0
  END IF
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/numSurfaceEntities", vals=tsize)
  DO ii = 1, tsize
    CALL ExportMeshEntity(obj%surfaceEntities(ii), hdf5, &
      & dsetname=TRIM(dsetname%chars())//"/surfaceEntities_"// &
      & TRIM(str(ii, .true.)), nsd=obj%nsd)
    CALL getNodeCoord( obj=obj%surfaceEntities(ii), nodeCoord=nodeCoord, &
      & local_nptrs=local_nptrs, count=count )
  END DO
  IF( ALLOCATED(obj%volumeEntities) ) THEN
    tsize = SIZE(obj%volumeEntities)
  ELSE
    tsize = 0
  END IF
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/numVolumeEntities", vals=tsize)
  DO ii = 1, tsize
    CALL ExportMeshEntity(obj%volumeEntities(ii), hdf5, &
      & dsetname=TRIM(dsetname%chars())//"/volumeEntities_"// &
      & TRIM(str(ii, .true.)), nsd=obj%nsd)
    CALL getNodeCoord( obj=obj%volumeEntities(ii), nodeCoord=nodeCoord, &
      & local_nptrs=local_nptrs, count=count )
  END DO
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/nodeCoord", &
    &vals=nodeCoord )
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/local_nptrs", &
    &vals=local_nptrs )
  IF( ALLOCATED( nodeCoord ) ) DEALLOCATE( nodeCoord )
END PROCEDURE msh_Export

!----------------------------------------------------------------------------
!                                                          ExportMeshFormat
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshFormat(obj, hdf5, dsetname)
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  TYPE( String ), INTENT( IN ) :: dsetname
  ! Writing mesh format
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/version", &
    & vals=obj%format%getVersion() )
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/majorVersion", &
    & vals=obj%format%getMajorVersion() )
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/minorVersion", &
    & vals=obj%format%getMinorVersion() )
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/engine", &
    & vals=string("GMSH 4.1 0 8") )
END SUBROUTINE ExportMeshFormat

!----------------------------------------------------------------------------
!                                                    ExportMeshPhysicalNames
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshPhysicalNames( obj, hdf5, dsetname )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  TYPE( String ), INTENT( IN ) :: dsetname

  ! Internal variables
  INTEGER( I4B ) :: ii, tsize

  ! Physical Names
  tsize = obj%physicalNames%getTotalPhysicalEntities()
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/PhysicalNames/totalPhysicalEntities", &
    & vals=tsize)

  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/PhysicalNames/NSD", &
    & vals=obj%physicalNames%getNSD() )

  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/PhysicalNames/tag", &
    & vals=obj%physicalNames%getPhysicalTags() )

  CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/PhysicalNames/numElements", &
    & vals=obj%physicalNames%getNumElements() )

  CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/PhysicalNames/numNodes", &
    & vals=obj%physicalNames%getNumNodes() )

  CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/PhysicalNames/physicalName", &
    & vals=obj%physicalNames%getPhysicalNames() )

  DO ii = 1, tsize
    CALL hdf5%write(dsetname=TRIM(dsetname%chars())// &
    & "/PhysicalNames/entities_" &
    & // trim(str(ii, .true.)), &
    & vals=obj%physicalNames%getEntities(indx=ii) )
  END DO
END SUBROUTINE ExportMeshPhysicalNames

!----------------------------------------------------------------------------
!                                                     ExportMeshNodeInfo
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshNodeInfo( obj, hdf5, dsetname )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  TYPE( String ), INTENT( IN ) :: dsetname
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/tNodes", &
    & vals=obj%Nodes%getNumNodes())
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/tEntitiesForNodes", &
    & vals=obj%Nodes%getnumEntityBlocks())
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/minNptrs", &
    & vals=obj%Nodes%getMinNodeTag())
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/maxNptrs", &
    & vals=obj%Nodes%getMaxNodeTag())
END SUBROUTINE ExportMeshNodeInfo

!----------------------------------------------------------------------------
!                                                     ExportMeshElementInfo
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshElementInfo( obj, hdf5, dsetname )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  TYPE( String ), INTENT( IN ) :: dsetname
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/tElements", &
    & vals=obj%Elements%getNumElements())
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/tEntitiesForElements", &
    & vals=obj%Elements%getnumEntityBlocks())
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/minElemNum", &
    & vals=obj%Elements%getMinElementTag())
  CALL hdf5%write(dsetname=TRIM(dsetname%chars())//"/maxElemNum", &
    & vals=obj%Elements%getMaxElementTag())
END SUBROUTINE ExportMeshElementInfo

!----------------------------------------------------------------------------
!                                                          ExportMeshEntity
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshEntity( obj, hdf5, dsetname, nsd )
  TYPE( mshEntity_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) ::  hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), INTENT( IN ) :: nsd
  INTEGER( I4B ), ALLOCATABLE :: intvec( : )
  REAL( DFP ), ALLOCATABLE :: realvec( : ), realMat(:,:)

  CALL hdf5%write( TRIM(dsetname) // "/uid", obj%getUid() )
  CALL hdf5%write( TRIM(dsetname) // "/xidim", obj%getXidim() )
  CALL hdf5%write( TRIM(dsetname) // "/elemType", obj%getElemType() )
  CALL hdf5%write( TRIM(dsetname) // "/nsd", nsd )
  CALL hdf5%write( TRIM(dsetname) // "/minX", obj%getMinX() )
  CALL hdf5%write( TRIM(dsetname) // "/minY", obj%getMinY() )
  CALL hdf5%write( TRIM(dsetname) // "/minZ", obj%getMinZ() )
  CALL hdf5%write( TRIM(dsetname) // "/maxX", obj%getMaxX() )
  CALL hdf5%write( TRIM(dsetname) // "/maxY", obj%getMaxY() )
  CALL hdf5%write( TRIM(dsetname) // "/maxZ", obj%getMaxZ() )
  CALL hdf5%write( TRIM(dsetname) // "/x", obj%getX() )
  CALL hdf5%write( TRIM(dsetname) // "/y", obj%getY() )
  CALL hdf5%write( TRIM(dsetname) // "/z", obj%getZ() )

  CALL hdf5%write( TRIM(dsetname) // "/tElements", obj%getTotalElements() )
  CALL hdf5%write( TRIM(dsetname) // "/tIntNodes", obj%getTotalIntNodes() )

  CALL hdf5%write( TRIM(dsetname) // "/physicalTag", obj%getPhysicalTag() )
  CALL hdf5%write( TRIM(dsetname) // "/intNodeNumber", obj%getIntNodeNumber() )
  CALL hdf5%write( TRIM(dsetname) // "/elemNumber", obj%getElemNumber() )
  CALL hdf5%write( TRIM(dsetname) // "/connectivity", obj%getConnectivity() )

  intvec = obj%getBoundingEntity()
  IF( SIZE( intvec ) .NE. 0 ) THEN
    CALL hdf5%write( TRIM(dsetname) // "/boundingEntity", &
    & intvec )
  END IF
END SUBROUTINE ExportMeshEntity

!----------------------------------------------------------------------------
!                                                           ExportNodeCoord
!----------------------------------------------------------------------------

SUBROUTINE getNodeCoord( obj, nodeCoord, local_nptrs, count )
  TYPE( mshEntity_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: nodeCoord( :, : )
  INTEGER( I4B ), INTENT( INOUT ) :: local_nptrs( : )
  INTEGER( I4B ), INTENT( INOUT ) :: count
  ! internal data
  REAL( DFP ), ALLOCATABLE :: myNodeCoord(:,:)
  INTEGER( I4B ), ALLOCATABLE :: myNptrs( : )
  INTEGER( I4B ) :: ii
  myNodeCoord = obj%getNodeCoord()
  myNptrs = obj%getIntNodeNumber()
  DO ii = 1, SIZE( myNptrs )
    count = count + 1
    local_nptrs( myNptrs( ii ) ) = count
    nodeCoord( :, count ) = myNodeCoord(:,ii)
  END DO
  IF( ALLOCATED( myNodeCoord ) ) DEALLOCATE( myNodeCoord )
  IF( ALLOCATED( myNptrs ) ) DEALLOCATE( myNptrs )
END SUBROUTINE getNodeCoord

END SUBMODULE IOMethods