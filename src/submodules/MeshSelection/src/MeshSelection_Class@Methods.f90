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

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This module defines a data type for mesh selection

SUBMODULE (MeshSelection_Class) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_addSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE meshSelect_addSurrogate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Initiate
  IF( .NOT. obj%isInitiated ) THEN
    obj%isInitiated = .TRUE.
    IF( PRESENT( isSelectionByMeshID ) ) THEN
      obj%isSelectionByMeshID = isSelectionByMeshID
    ELSE
      obj%isSelectionByMeshID = .FALSE.
    END IF
    IF( PRESENT( isSelectionByElemID ) ) THEN
      obj%isSelectionByElemID = isSelectionByElemID
    ELSE
      obj%isSelectionByElemID = .FALSE.
    END IF
    IF( PRESENT( isSelectionByBox ) ) THEN
      obj%isSelectionByBox = isSelectionByBox
    ELSE
      obj%isSelectionByBox = .FALSE.
    END IF
  ELSE
    IF( PRESENT( isSelectionByMeshID ) ) THEN
      obj%isSelectionByMeshID = isSelectionByMeshID
    END IF
    IF( PRESENT( isSelectionByElemID ) ) THEN
      obj%isSelectionByElemID = isSelectionByElemID
    END IF
    IF( PRESENT( isSelectionByBox ) ) THEN
      obj%isSelectionByBox = isSelectionByBox
    END IF
  END IF
END PROCEDURE meshSelect_Initiate

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_DeallocateData
  obj%isInitiated = .FALSE.
  obj%isSelectionByElemID = .FALSE.
  obj%isSelectionByMeshID = .FALSE.
  obj%isSelectionByBox = .FALSE.
  CALL DeallocateData( obj%PointMeshID )
  CALL DeallocateData( obj%CurveMeshID )
  CALL DeallocateData( obj%SurfaceMeshID )
  CALL DeallocateData( obj%VolumeMeshID )
  CALL DeallocateData( obj%PointElemNum )
  CALL DeallocateData( obj%CurveElemNum )
  CALL DeallocateData( obj%SurfaceElemNum )
  CALL DeallocateData( obj%VolumeElemNum )
END PROCEDURE meshSelect_DeallocateData

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Final
  CALL obj%DeallocateData()
END PROCEDURE meshSelect_Final

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Add
  CHARACTER( LEN = * ), PARAMETER :: myName="meshSelect_Add"
  IF( PRESENT( xidim ) .AND. PRESENT( meshID ) ) THEN
    obj%isSelectionByMeshID = .TRUE.
    SELECT CASE( xidim )
    CASE( 0 )
      CALL APPEND( obj%PointMeshID, meshID )
    CASE( 1 )
      CALL APPEND( obj%CurveMeshID, meshID )
    CASE( 2 )
      CALL APPEND( obj%SurfaceMeshID, meshID )
    CASE( 3 )
      CALL APPEND( obj%VolumeMeshID, meshID )
    END SELECT
    RETURN
  END IF
  IF( PRESENT( xidim ) .AND. PRESENT( elemNum ) ) THEN
    obj%isSelectionByElemID = .TRUE.
    SELECT CASE( xidim )
    CASE( 0 )
      CALL APPEND( obj%PointElemNum, elemNum )
    CASE( 1 )
      CALL APPEND( obj%CurveElemNum, elemNum )
    CASE( 2 )
      CALL APPEND( obj%SurfaceElemNum, elemNum )
    CASE( 3 )
      CALL APPEND( obj%VolumeElemNum, elemNum )
    END SELECT
    RETURN
  END IF
  CALL e%raiseError( modName//'::'//myName//'-'// &
    & 'Currently mesh selection is possible through (xidim, meshID), &
    & and (xidim, elemNum). We are working on it' )
END PROCEDURE meshSelect_Add

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Set
  IF( isAllocated( obj%PointMeshID ) ) THEN
    CALL RemoveDuplicates( obj%PointMeshID )
  END IF
  IF( isAllocated( obj%CurveMeshID ) ) THEN
    CALL RemoveDuplicates( obj%CurveMeshID )
  END IF
  IF( isAllocated( obj%SurfaceMeshID ) ) THEN
    CALL RemoveDuplicates( obj%SurfaceMeshID )
  END IF
  IF( isAllocated( obj%VolumeMeshID ) ) THEN
    CALL RemoveDuplicates( obj%VolumeMeshID )
  END IF
  IF( isAllocated( obj%PointElemNum ) ) THEN
    CALL RemoveDuplicates( obj%PointElemNum )
  END IF
  IF( isAllocated( obj%CurveElemNum ) ) THEN
    CALL RemoveDuplicates( obj%CurveElemNum )
  END IF
  IF( isAllocated( obj%SurfaceElemNum ) ) THEN
    CALL RemoveDuplicates( obj%SurfaceElemNum )
  END IF
  IF( isAllocated( obj%VolumeElemNum ) ) THEN
    CALL RemoveDuplicates( obj%VolumeElemNum )
  END IF
END PROCEDURE meshSelect_Set

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="meshSelect_Import"
  TYPE( String ) :: dsetname, strval
  INTEGER( I4B ) :: ierr
  INTEGER( I4B ), ALLOCATABLE :: intvec( : )
  !> check
  IF( obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The object is already initiated, deallocate first!')
  END IF
  obj%isInitiated = .TRUE.
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "IMPORTING MESH SELECTION")
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'HDF5 file is not opened')
  END IF
  !> check
  IF( .NOT. hdf5%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'HDF5 file does not have read permission')
  END IF
  ! READ isSelectionByMeshID
  dsetname=TRIM(group)//"/isSelectionByMeshID"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=obj%isSelectionByMeshID)
  ELSE
    obj%isSelectionByMeshID=.FALSE.
  END IF
  ! READ isSelectionByElemID
  dsetname=TRIM(group)//"/isSelectionByElemID"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=obj%isSelectionByElemID)
  ELSE
    obj%isSelectionByElemID=.FALSE.
  END IF
  ! READ isSelectionByBox
  dsetname=TRIM(group)//"/isSelectionByBox"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=obj%isSelectionByBox)
  ELSE
    obj%isSelectionByBox=.FALSE.
  END IF
  ! READ PointMeshID
  dsetname=TRIM(group)//"/PointMeshID"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%PointMeshID=intvec
    obj%isSelectionByMeshID=.TRUE.
  END IF
  ! READ CurveMeshID
  dsetname=TRIM(group)//"/CurveMeshID"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%CurveMeshID=intvec
    obj%isSelectionByMeshID=.TRUE.
  END IF
  ! READ SurfaceMeshID
  dsetname=TRIM(group)//"/SurfaceMeshID"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%SurfaceMeshID=intvec
    obj%isSelectionByMeshID=.TRUE.
  END IF
  ! READ VolumeMeshID
  dsetname=TRIM(group)//"/VolumeMeshID"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%VolumeMeshID=intvec
    obj%isSelectionByMeshID=.TRUE.
  END IF
  ! READ PointElemNum
  dsetname=TRIM(group)//"/PointElemNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%PointElemNum=intvec
    obj%isSelectionByElemID=.TRUE.
  END IF
  ! READ CurveElemNum
  dsetname=TRIM(group)//"/CurveElemNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%CurveElemNum=intvec
    obj%isSelectionByElemID=.TRUE.
  END IF
  ! READ SurfaceElemNum
  dsetname=TRIM(group)//"/SurfaceElemNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%SurfaceElemNum=intvec
    obj%isSelectionByElemID=.TRUE.
  END IF
  ! READ VolumeElemNum
  dsetname=TRIM(group)//"/VolumeElemNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%VolumeElemNum=intvec
    obj%isSelectionByElemID=.TRUE.
  END IF
END PROCEDURE meshSelect_Import

!----------------------------------------------------------------------------
!                                                                   Export
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="meshSelect_Export"
  TYPE( String ) :: dsetname, strval
  INTEGER( I4B ) :: ierr
  INTEGER( I4B ), ALLOCATABLE :: intvec( : )
  !> check
  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The object is not initiated, allocate first!')
  END IF
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "EXPORTING MESH SELECTION")
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'HDF5 file is not opened')
  END IF
  !> check
  IF( .NOT. hdf5%isWrite() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'HDF5 file does not have write permission')
  END IF
  ! READ isSelectionByMeshID
  dsetname=TRIM(group)//"/isSelectionByMeshID"
  CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
    & vals=obj%isSelectionByMeshID)
  ! READ isSelectionByElemID
  dsetname=TRIM(group)//"/isSelectionByElemID"
  CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
    & vals=obj%isSelectionByElemID)
  ! READ isSelectionByBox
  dsetname=TRIM(group)//"/isSelectionByBox"
  CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
    & vals=obj%isSelectionByBox)
  ! READ PointMeshID
  IF( isAllocated(obj%PointMeshID) ) THEN
    dsetname=TRIM(group)//"/PointMeshID"
    intvec=obj%PointMeshID
    CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
  END IF
  ! READ CurveMeshID
  IF( isAllocated(obj%CurveMeshID) ) THEN
    dsetname=TRIM(group)//"/CurveMeshID"
    intvec=obj%CurveMeshID
    CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
  END IF
  ! READ SurfaceMeshID
  IF( isAllocated(obj%SurfaceMeshID) ) THEN
    dsetname=TRIM(group)//"/SurfaceMeshID"
    intvec=obj%SurfaceMeshID
    CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
  END IF
  ! READ VolumeMeshID
  IF( isAllocated(obj%VolumeMeshID) ) THEN
    dsetname=TRIM(group)//"/VolumeMeshID"
    intvec=obj%VolumeMeshID
    CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
  END IF
  ! READ PointElemNum
  IF( isAllocated(obj%PointElemNum) ) THEN
    dsetname=TRIM(group)//"/PointElemNum"
    intvec=obj%PointElemNum
    CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
  END IF
  ! READ CurveElemNum
  IF( isAllocated(obj%CurveElemNum) ) THEN
    dsetname=TRIM(group)//"/CurveElemNum"
    intvec=obj%CurveElemNum
    CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
  END IF
  ! READ SurfaceElemNum
  IF( isAllocated(obj%SurfaceElemNum) ) THEN
    dsetname=TRIM(group)//"/SurfaceElemNum"
    intvec=obj%SurfaceElemNum
    CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
  END IF
  ! READ VolumeElemNum
  IF( isAllocated(obj%VolumeElemNum) ) THEN
    dsetname=TRIM(group)//"/VolumeElemNum"
    intvec=obj%VolumeElemNum
    CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
  END IF
END PROCEDURE meshSelect_Export

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Display
  IF( LEN_TRIM( msg ) .GT. 0 ) THEN
    CALL Display( "# "//TRIM(msg), unitNo=unitNo )
  END IF
  IF( .NOT. obj%isInitiated ) THEN
    CALL Display( "# The object is not initiated, nothing to show!", &
      & unitNo=unitNo)
    RETURN
  ELSE
    CALL Display( "# isInitiated : TRUE" , &
      & unitNo=unitNo)
  END IF
  CALL Display( obj%isSelectionByMeshID, "# isSelectionByMeshID : ", &
    & unitNo=unitNo )
  CALL Display( obj%isSelectionByElemID, "# isSelectionByElemID : ", &
    & unitNo=unitNo )
  CALL Display( obj%isSelectionByBox, "# isSelectionByBox : ", &
    & unitNo=unitNo )
  IF( isAllocated( obj%PointMeshID ) ) THEN
    CALL Display( obj%PointMeshID, "PointMeshID : ", &
    & unitNo=unitNo )
  END IF
  IF( isAllocated( obj%CurveMeshID ) ) THEN
    CALL Display( obj%CurveMeshID, "CurveMeshID : ", &
    & unitNo=unitNo )
  END IF
  IF( isAllocated( obj%SurfaceMeshID ) ) THEN
    CALL Display( obj%SurfaceMeshID, "SurfaceMeshID : ", &
    & unitNo=unitNo )
  END IF
  IF( isAllocated( obj%VolumeMeshID ) ) THEN
    CALL Display( obj%VolumeMeshID, "VolumeMeshID : ", &
    & unitNo=unitNo )
  END IF
  IF( isAllocated( obj%PointElemNum ) ) THEN
    CALL Display( obj%PointElemNum, "PointElemNum : ", &
    & unitNo=unitNo )
  END IF
  IF( isAllocated( obj%CurveElemNum ) ) THEN
    CALL Display( obj%CurveElemNum, "CurveElemNum : ", &
    & unitNo=unitNo )
  END IF
  IF( isAllocated( obj%SurfaceElemNum ) ) THEN
    CALL Display( obj%SurfaceElemNum, "SurfaceElemNum : ", &
    & unitNo=unitNo )
  END IF
  IF( isAllocated( obj%VolumeElemNum ) ) THEN
    CALL Display( obj%VolumeElemNum, "VolumeElemNum : ", &
    & unitNo=unitNo )
  END IF
END PROCEDURE meshSelect_Display

!----------------------------------------------------------------------------
!                                                      meshSelect_getMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getMeshID
  SELECT CASE( xidim )
  CASE(0)
    IF(isAllocated(obj%PointMeshID))ans=obj%PointMeshID
  CASE(1)
    IF(isAllocated(obj%CurveMeshID))ans=obj%CurveMeshID
  CASE(2)
    IF(isAllocated(obj%SurfaceMeshID))ans=obj%SurfaceMeshID
  CASE(3)
    IF(isAllocated(obj%VolumeMeshID))ans=obj%VolumeMeshID
  END SELECT
END PROCEDURE meshSelect_getMeshID

!----------------------------------------------------------------------------
!                                                       meshSelect_getElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getElemNum
  SELECT CASE( xidim )
  CASE(0)
    IF(isAllocated(obj%PointElemNum))ans=obj%PointElemNum
  CASE(1)
    IF(isAllocated(obj%CurveElemNum))ans=obj%CurveElemNum
  CASE(2)
    IF(isAllocated(obj%SurfaceElemNum))ans=obj%SurfaceElemNum
  CASE(3)
    IF(isAllocated(obj%VolumeElemNum))ans=obj%VolumeElemNum
  END SELECT
END PROCEDURE meshSelect_getElemNum


END SUBMODULE Methods