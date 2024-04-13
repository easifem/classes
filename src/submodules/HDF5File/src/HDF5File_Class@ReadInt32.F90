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

SUBMODULE(HDF5File_Class) ReadInt32
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_n0
  INTEGER( I4B ), PARAMETER :: rank=0
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id

  ! Read the dataset
  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,ierr)
  mem=H5T_NATIVE_INTEGER
  IF(ierr >= 0) &
      CALL h5dread_f(dset_id,mem,vals,dims,ierr)
  CALL postRead(obj,path,dset_id,dspace_id,ierr)
END PROCEDURE hdf5_read_n0

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_n1
  INTEGER( I4B ), PARAMETER :: rank=1
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( rank ) :: dims
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id
  INTEGER( I4B ) :: error

  ! Allocate space if needed, make sure it is the right size
  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    CALL Reallocate( vals, INT(dims, I4B) )
    mem=H5T_NATIVE_INTEGER
    CALL h5dread_f(dset_id,mem,vals,dims,error)
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_n1

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_n2
  INTEGER( I4B ), PARAMETER :: rank=2
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( rank ) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id,dset_id
  INTEGER( I4B ) :: error

  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    CALL Reallocate( vals, INT(dims, I4B) )
    mem=H5T_NATIVE_INTEGER
    CALL h5dread_f(dset_id,mem,vals,dims,error)
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_n2

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_n3
  INTEGER( I4B ), PARAMETER :: rank=3
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( rank ) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id,dset_id
  INTEGER( I4B ) :: error

  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    CALL Reallocate( vals, INT(dims, I4B) )
    mem=H5T_NATIVE_INTEGER
    CALL h5dread_f(dset_id,mem,vals,dims,error)
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_n3

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_n4
  INTEGER( I4B ), PARAMETER :: rank=4
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( rank ) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id,dset_id
  INTEGER( I4B ) :: error

  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    CALL Reallocate( vals, INT(dims, I4B) )
    mem=H5T_NATIVE_INTEGER
    CALL h5dread_f(dset_id,mem,vals,dims,error)
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_n4

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_n5
  INTEGER( I4B ), PARAMETER :: rank=5
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( rank ) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id,dset_id
  INTEGER( I4B ) :: error

  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    CALL Reallocate( vals, INT(dims, I4B) )
    mem=H5T_NATIVE_INTEGER
    CALL h5dread_f(dset_id,mem,vals,dims,error)
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_n5

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_n6
  INTEGER( I4B ), PARAMETER :: rank=6
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( rank ) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id,dset_id
  INTEGER( I4B ) :: error

  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    CALL Reallocate( vals, INT(dims, I4B) )
    mem=H5T_NATIVE_INTEGER
    CALL h5dread_f(dset_id,mem,vals,dims,error)
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_n6

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_n7
  INTEGER( I4B ), PARAMETER :: rank=7
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( rank ) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id,dset_id
  INTEGER( I4B ) :: error

  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    CALL Reallocate( vals, INT(dims, I4B) )
    mem=H5T_NATIVE_INTEGER
    CALL h5dread_f(dset_id,mem,vals,dims,error)
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_n7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ReadInt32