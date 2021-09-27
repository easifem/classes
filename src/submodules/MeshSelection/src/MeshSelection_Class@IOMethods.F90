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

SUBMODULE (MeshSelection_Class) IOMethods
IMPLICIT NONE
CONTAINS

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
  ! READ isSelectionByElemNum
  dsetname=TRIM(group)//"/isSelectionByElemNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=obj%isSelectionByElemNum)
  ELSE
    obj%isSelectionByElemNum=.FALSE.
  END IF
  ! READ isSelectionByNodeNum
  dsetname=TRIM(group)//"/isSelectionByNodeNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=obj%isSelectionByNodeNum)
  ELSE
    obj%isSelectionByNodeNum=.FALSE.
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
    obj%isSelectionByElemNum=.TRUE.
  END IF
  ! READ CurveElemNum
  dsetname=TRIM(group)//"/CurveElemNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%CurveElemNum=intvec
    obj%isSelectionByElemNum=.TRUE.
  END IF
  ! READ SurfaceElemNum
  dsetname=TRIM(group)//"/SurfaceElemNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%SurfaceElemNum=intvec
    obj%isSelectionByElemNum=.TRUE.
  END IF
  ! READ VolumeElemNum
  dsetname=TRIM(group)//"/VolumeElemNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%VolumeElemNum=intvec
    obj%isSelectionByElemNum=.TRUE.
  END IF
  ! READ NodeNum
  dsetname=TRIM(group)//"/NodeNum"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()), &
      & vals=intvec)
    obj%NodeNum=intvec
    obj%isSelectionByNodeNum =.TRUE.
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
  ! READ isSelectionByElemNum
  dsetname=TRIM(group)//"/isSelectionByElemNum"
  CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
    & vals=obj%isSelectionByElemNum)
  ! READ isSelectionByNodeNum
  dsetname=TRIM(group)//"/isSelectionByNodeNum"
  CALL hdf5%write(dsetname=TRIM(dsetname%chars()), &
    & vals=obj%isSelectionByNodeNum)
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
  ! READ NodeNum
  IF( isAllocated(obj%NodeNum) ) THEN
    dsetname=TRIM(group)//"/NodeNum"
    intvec=obj%NodeNum
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
  CALL Display( obj%isSelectionByElemNum, "# isSelectionByElemNum : ", &
    & unitNo=unitNo )
  CALL Display( obj%isSelectionByNodeNum, "# isSelectionByNodeNum : ", &
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
  IF( isAllocated( obj%NodeNum ) ) THEN
    CALL Display( obj%NodeNum, "NodeNum : ", &
    & unitNo=unitNo )
  END IF
END PROCEDURE meshSelect_Display

END SUBMODULE IOMethods