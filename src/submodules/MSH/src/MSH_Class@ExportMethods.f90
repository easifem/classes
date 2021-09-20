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

SUBMODULE( MSH_Class ) ExportMethods
USE BaseMethod
USE HDF5File_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ExportMesh
  CHARACTER( LEN = * ), PARAMETER :: myName="msh_ExportMesh"
  TYPE( HDF5File_ ) :: hdf5
  INTEGER( I4B ) :: ii, tsize,  tNodes, count
  REAL( DFP ), ALLOCATABLE :: nodeCoord( :, : )
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )

  tNodes = obj%nodes%getNumNodes()
  ALLOCATE( nodeCoord( 3, tNodes ) )
  ALLOCATE( local_nptrs( obj%Nodes%getMaxNodeTag() ) )
  count = 0
  local_nptrs = 0
  CALL hdf5%initiate(filename=file, mode="NEW" )
  CALL hdf5%open()
  CALL hdf5%write(dsetname="/NSD", vals=obj%nsd)
  CALL ExportMeshFormat(obj,hdf5)
  CALL ExportMeshPhysicalNames(obj,hdf5)
  CALL ExportMeshNodeInfo(obj,hdf5)
  CALL ExportMeshElementInfo(obj,hdf5)
  IF( ALLOCATED(obj%pointEntities) ) THEN
    tsize = SIZE(obj%pointEntities)
  ELSE
    tsize = 0
  END IF
  CALL hdf5%write(dsetname="/numPointEntities", vals=tsize)
  DO ii = 1, tsize
    CALL ExportMeshEntity(obj%pointEntities(ii), hdf5, dsetname="/pointEntities_"//TRIM(str(ii, .true.)), nsd=obj%nsd )
    CALL getNodeCoord( obj=obj%pointEntities(ii), nodeCoord=nodeCoord, &
      & local_nptrs=local_nptrs, count=count )
  END DO
  IF( ALLOCATED(obj%curveEntities) ) THEN
    tsize = SIZE(obj%curveEntities)
  ELSE
    tsize = 0
  END IF
  CALL hdf5%write(dsetname="/numCurveEntities", vals=tsize)
  DO ii = 1, tsize
    CALL ExportMeshEntity(obj%curveEntities(ii), hdf5, dsetname="/curveEntities_"//TRIM(str(ii, .true.)), nsd=obj%nsd)
    CALL getNodeCoord( obj=obj%curveEntities(ii), nodeCoord=nodeCoord, &
      & local_nptrs=local_nptrs, count=count )
  END DO
  IF( ALLOCATED(obj%surfaceEntities) ) THEN
    tsize = SIZE(obj%surfaceEntities)
  ELSE
    tsize = 0
  END IF
  CALL hdf5%write(dsetname="/numSurfaceEntities", vals=tsize)
  DO ii = 1, tsize
    CALL ExportMeshEntity(obj%surfaceEntities(ii), hdf5, dsetname="/surfaceEntities_"//TRIM(str(ii, .true.)), nsd=obj%nsd)
    CALL getNodeCoord( obj=obj%surfaceEntities(ii), nodeCoord=nodeCoord, &
      & local_nptrs=local_nptrs, count=count )
  END DO
  IF( ALLOCATED(obj%volumeEntities) ) THEN
    tsize = SIZE(obj%volumeEntities)
  ELSE
    tsize = 0
  END IF
  CALL hdf5%write(dsetname="/numVolumeEntities", vals=tsize)
  DO ii = 1, tsize
    CALL ExportMeshEntity(obj%volumeEntities(ii), hdf5, dsetname="/volumeEntities_"//TRIM(str(ii, .true.)), nsd=obj%nsd)
    CALL getNodeCoord( obj=obj%volumeEntities(ii), nodeCoord=nodeCoord, &
      & local_nptrs=local_nptrs, count=count )
  END DO
  CALL hdf5%write(dsetname="/nodeCoord", vals=nodeCoord )
  CALL hdf5%write(dsetname="/local_nptrs", vals=local_nptrs )
  CALL hdf5%close()
  CALL hdf5%DeallocateData()
  IF( ALLOCATED( nodeCoord ) ) DEALLOCATE( nodeCoord )
END PROCEDURE msh_ExportMesh

!----------------------------------------------------------------------------
!                                                          ExportMeshFormat
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshFormat(obj, hdf5)
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5

  ! Writing mesh format
  CALL hdf5%write(dsetname="/version", &
    & vals=obj%format%getVersion() )
  CALL hdf5%write(dsetname="/majorVersion", &
    & vals=obj%format%getMajorVersion() )
  CALL hdf5%write(dsetname="/minorVersion", &
    & vals=obj%format%getMinorVersion() )
  CALL hdf5%write(dsetname="/engine", &
    & vals=string("GMSH 4.1 0 8") )
END SUBROUTINE ExportMeshFormat

!----------------------------------------------------------------------------
!                                                    ExportMeshPhysicalNames
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshPhysicalNames( obj, hdf5 )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5

  ! Internal variables
  INTEGER( I4B ) :: ii, tsize

  ! Physical Names
  tsize = obj%physicalNames%getTotalPhysicalEntities()
  CALL hdf5%write(dsetname="/PhysicalNames/totalPhysicalEntities", &
    & vals=tsize)

  CALL hdf5%write(dsetname="/PhysicalNames/NSD", &
    & vals=obj%physicalNames%getNSD() )

  CALL hdf5%write(dsetname="/PhysicalNames/tag", &
    & vals=obj%physicalNames%getPhysicalTags() )

  CALL hdf5%write(dsetname="/PhysicalNames/numElements", &
    & vals=obj%physicalNames%getNumElements() )

  CALL hdf5%write(dsetname="/PhysicalNames/numNodes", &
    & vals=obj%physicalNames%getNumNodes() )

  CALL hdf5%write(dsetname="/PhysicalNames/physicalName", &
    & vals=obj%physicalNames%getPhysicalNames() )

  DO ii = 1, tsize
    CALL hdf5%write(dsetname="/PhysicalNames/entities_" &
      & // trim(str(ii, .true.)), &
      & vals=obj%physicalNames%getEntities(indx=ii) )
  END DO
END SUBROUTINE ExportMeshPhysicalNames

!----------------------------------------------------------------------------
!                                                     ExportMeshNodeInfo
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshNodeInfo( obj, hdf5 )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CALL hdf5%write(dsetname="/tNodes", vals=obj%Nodes%getNumNodes())
  CALL hdf5%write(dsetname="/tEntitiesForNodes", vals=obj%Nodes%getnumEntityBlocks())
  CALL hdf5%write(dsetname="/minNptrs", vals=obj%Nodes%getMinNodeTag())
  CALL hdf5%write(dsetname="/maxNptrs", vals=obj%Nodes%getMaxNodeTag())
END SUBROUTINE ExportMeshNodeInfo

!----------------------------------------------------------------------------
!                                                     ExportMeshElementInfo
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshElementInfo( obj, hdf5 )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CALL hdf5%write(dsetname="/tElements",vals=obj%Elements%getNumElements())
  CALL hdf5%write(dsetname="/tEntitiesForElements", &
    & vals=obj%Elements%getnumEntityBlocks())
  CALL hdf5%write(dsetname="/minElemNum", &
    & vals=obj%Elements%getMinElementTag())
  CALL hdf5%write(dsetname="/maxElemNum", &
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

END SUBMODULE ExportMethods