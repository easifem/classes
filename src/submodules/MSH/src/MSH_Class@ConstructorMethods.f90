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

SUBMODULE( MSH_Class ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Final
  CALL obj%DeallocateData()
END PROCEDURE msh_Final

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_deallocatedata
  CALL obj%Format%DeallocateData()
  CALL obj%PhysicalNames%DeallocateData()
  CALL obj%Nodes%DeallocateData()
  CALL obj%Elements%DeallocateData()
  IF( ALLOCATED( obj%PointEntities ) ) DEALLOCATE( obj%PointEntities )
  IF( ALLOCATED( obj%CurveEntities ) ) DEALLOCATE( obj%CurveEntities )
  IF( ALLOCATED( obj%SurfaceEntities ) ) DEALLOCATE( obj%SurfaceEntities )
  IF( ALLOCATED( obj%VolumeEntities ) ) DEALLOCATE( obj%VolumeEntities )
  obj%nsd = 0
  IF( ASSOCIATED( obj % buffer ) ) DEALLOCATE( obj%buffer )
  NULLIFY( obj%buffer )
  CALL DeallocateTxtFile( obj, Delete )
END PROCEDURE msh_deallocatedata

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_initiate
  ! Define internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName = "msh_Initiate"
  INTEGER( I4B ) :: error, unitNo, tp, tc, ts, tv
  !> main
  !> info
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'OPENING: MSH4 file')
  !> initiating mshFile
  CALL InitiateTxtFile( obj=obj, file=file, status="OLD", action="READ" )
  CALL obj%open()
  unitNo = obj%getunitNo()
  !> info
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'OPENING: MSH4 file [OK!]')
  !> reading mesh format
  CALL e%raiseDebug(modName//'::'//myName//' - '// &
    & 'READING: meshFormat')
  CALL obj%Format%Read( mshFile=obj, error=error)
  IF( error .NE. 0 ) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Failed in Reading mesh format')
  ELSE
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & 'READING: meshFormat [OK!]')
  END IF
  !> reading physical group information
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: physicalNames')
  CALL obj%PhysicalNames%Read( mshFile=obj, error=error )
  IF( obj%PhysicalNames%isInitiated ) THEN
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & 'READING: physicalNames [OK!]')
  ELSE
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & 'READING: physicalNames [NOT FOUND!]')
  END IF
  !> Entities
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'LOCATING: $Entities')
  CALL TypemshEntity%GotoTag( mshFile=obj, error=error )
  IF( error .NE. 0 ) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'LOCATING: $Entities [NOT FOUND!]')
  ELSE
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & 'LOCATING: $Entities [OK!]')
  END IF
  !> Entities
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: $Entities')
  READ( unitNo, * ) tp, tc, ts, tv
  IF( tp .NE. 0 ) obj%nsd=0
  IF( tc .NE. 0 ) obj%nsd=1
  IF( ts .NE. 0 ) obj%nsd=2
  IF( tv .NE. 0 ) obj%nsd=3
  CALL obj%ReadPointEntities( te=tp )
  CALL obj%ReadCurveEntities( te=tc )
  CALL obj%ReadSurfaceEntities( te=ts )
  CALL obj%ReadVolumeEntities( te=tv )
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: $Entities [OK!]')
  !> Nodes
  CALL obj%ReadNodes( )
  !> Elements
  CALL obj%ReadElements( )
  !> nodes in physical regions
  CALL setNumNodesInPhysicalNames( obj )
END PROCEDURE msh_initiate

!----------------------------------------------------------------------------
!                                                               setNumNodes
!----------------------------------------------------------------------------

SUBROUTINE setNumNodesInPhysicalNames( obj )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  ! Internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName="setNumNodesInPhysicalNames"
  INTEGER( I4B ) :: tpt, i, j, k, tElements, dim
  INTEGER( I4B ), ALLOCATABLE :: Indx( : ), entIndx( : ), Nptrs( : ), &
    & dummyNptrs( : )
  !> main
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: Nodes in Physical entities' )
  ALLOCATE( Nptrs( obj%Nodes%getMaxNodeTag() ) )
  ! Points
  dim = 0
  tpt = obj%PhysicalNames%getTotalPhysicalEntities( [dim] )
  IF( tpt .NE. 0 ) THEN
    Indx = obj%PhysicalNames%getIndex( dim=dim )
    ! loop over all physical points
    DO i = 1, tpt
      ! get Entities associated
      entIndx = obj%PhysicalNames%getEntities( Indx( i ) )
      CALL obj%PhysicalNames%IncNumNodes( Indx=Indx( i ), &
        & incr=SIZE( entIndx ) )
    END DO
  END IF
  ! Curve
  dim=1
  tpt = obj%PhysicalNames%getTotalPhysicalEntities( [dim] )
  IF( tpt .NE. 0 ) THEN
    Indx = obj%PhysicalNames%getIndex( dim=dim )
    ! loop over physical points
    DO i = 1, tpt
      ! get Entities associated
      entIndx = obj%PhysicalNames%getEntities( Indx( i ) )
      Nptrs = 0_I4B
      DO j = 1, SIZE( entIndx )
        tElements = obj%CurveEntities( entIndx( j ) )%getTotalElements( )
        DO k = 1, tElements
          dummyNptrs = obj%CurveEntities( entIndx( j ) )%getConnectivity( k )
          Nptrs( dummyNptrs ) = dummyNptrs
        END DO
      END DO
      ! count the nonzero nptrs
      CALL obj%PhysicalNames%setNumNodes( indx=Indx( i ), numNode = COUNT( Nptrs .NE. 0 ) )
    END DO
  END IF
  ! Surface
  dim=2
  tpt = obj%PhysicalNames%getTotalPhysicalEntities( dim=[dim] )
  IF( tpt .NE. 0 ) THEN
    Indx = obj%PhysicalNames%getIndex( dim=dim )
    ! loop over physical points
    DO i = 1, tpt
      ! get Entities associated
      entIndx = obj%PhysicalNames%getEntities( indx=Indx( i ) )
      Nptrs = 0_I4B
      DO j = 1, SIZE( entIndx )
        tElements = obj%SurfaceEntities( entIndx( j ) )%getTotalElements( )
        DO k = 1, tElements
          dummyNptrs = obj%SurfaceEntities( entIndx( j ) )%getConnectivity( k )
          Nptrs( dummyNptrs ) = dummyNptrs
        END DO
      END DO
      ! count the nonzero nptrs
      CALL obj%PhysicalNames%setNumNodes( indx=Indx( i ), numNode=COUNT( Nptrs .NE. 0 ) )
    END DO
  END IF
  ! Volume
  dim=3
  tpt = obj%PhysicalNames%getTotalPhysicalEntities( dim=[dim] )
  IF( tpt .NE. 0 ) THEN
    Indx = obj%PhysicalNames%getIndex( dim=dim )
    ! loop over physical points
    DO i = 1, tpt
      ! get Entities associated
      entIndx = obj%PhysicalNames%getEntities( Indx( i ) )
      Nptrs = 0_I4B
      DO j = 1, SIZE( entIndx )
        tElements = obj%VolumeEntities( entIndx( j ) )%getTotalElements()
        DO k = 1, tElements
          dummyNptrs = obj%VolumeEntities( entIndx( j ) )%getConnectivity( k )
          Nptrs( dummyNptrs ) = dummyNptrs
        END DO
      END DO
      ! count the nonzero nptrs
      CALL obj%PhysicalNames%setNumNodes( indx=Indx( i ), &
        & numNode = COUNT( Nptrs .NE. 0 ) )
    END DO
  END IF
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: Nodes in Physical entities [OK!]' )
  ! add deallocate stmt
  IF( ALLOCATED( Indx ) ) DEALLOCATE( Indx )
  IF( ALLOCATED( entIndx ) ) DEALLOCATE( entIndx )
  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  IF( ALLOCATED( DummyNptrs ) ) DEALLOCATE( DummyNptrs )
END SUBROUTINE setNumNodesInPhysicalNames

! !----------------------------------------------------------------------------
! !                                                                        msh4
! !----------------------------------------------------------------------------

! MODULE PROCEDURE msh_constuctor1
!   CALL ans%Initiate( Path, FileName, Extension, NSD )
! END PROCEDURE msh_constuctor1


END SUBMODULE Methods