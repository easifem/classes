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

SUBMODULE( HDF5File_Class ) WriteString
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_st0
  CHARACTER( LEN=LEN_TRIM(vals) ) :: valss
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: ldims,gdims,offset,cnt
  INTEGER( I4B ), PARAMETER :: rank=0
  INTEGER( HID_T ) :: mem,dspace_id,dset_id,gspace_id,plist_id

  path=dsetname
  valss=vals%chars()
  ! stash offset
  offset(1)=0
  IF(PRESENT(offset_in)) offset=offset_in

  ! Determine the dimensions for the dataspace
  ldims=1

  ! Store the dimensions from global if present
  IF(PRESENT(gdims_in)) THEN
    gdims=gdims_in
  ELSE
    gdims=ldims
  ENDIF
  cnt=gdims
  IF(PRESENT(cnt_in)) cnt=cnt_in

  CALL h5tcopy_f( H5T_NATIVE_CHARACTER, mem, ierr )
  CALL h5tset_strpad_f( mem, 0, ierr )
  CALL h5tset_size_f( mem, INT( LEN_TRIM(vals), Real64), ierr )
  CALL preWrite( obj, rank, gdims, ldims, path, mem, dset_id, dspace_id, &
    & gspace_id, plist_id, ierr, cnt, offset )
  IF(ierr == 0) &
    & CALL h5dwrite_f(dset_id,mem,valss,gdims,ierr,dspace_id,gspace_id,plist_id)
  CALL postWrite(obj,ierr,dset_id,dspace_id,gspace_id,plist_id)
  CALL h5tclose_f(mem,ierr)
END PROCEDURE hdf5_write_st0

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_st1
  CHARACTER( LEN=length_max ) :: valss( SIZE( vals ) )
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( I4B ) :: j
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: ldims, gdims, offset, cnt
  INTEGER( I4B ), PARAMETER :: rank=1
  INTEGER( HID_T ) :: mem, dspace_id, dset_id, gspace_id, plist_id

  path=dsetname
  ! Fill character array
  DO j=1,SIZE(vals,DIM=1)
    valss(j)=vals(j)%chars()
  ENDDO

  ! stash offset
  offset(1)=0
  IF(PRESENT(offset_in)) offset=offset_in

  ! Determine the dimensions for the dataspace
  ldims=SHAPE(vals)

  ! Store the dimensions from global if present
  IF(PRESENT(gdims_in)) THEN
    gdims=gdims_in
  ELSE
    gdims=ldims
  ENDIF
  cnt=gdims
  IF(PRESENT(cnt_in)) cnt=cnt_in

  CALL h5tcopy_f(H5T_NATIVE_CHARACTER,mem,ierr)
  CALL h5tset_strpad_f(mem,0,ierr)
  CALL h5tset_size_f(mem,INT(length_max,Real64),ierr)
  CALL preWrite(obj,rank,gdims,ldims,path,mem,dset_id,dspace_id, &
    & gspace_id,plist_id,ierr,cnt,offset)
  IF(ierr == 0) &
    CALL h5dwrite_f(dset_id,mem,valss,gdims,ierr,dspace_id,gspace_id,plist_id)
  CALL postWrite(obj,ierr,dset_id,dspace_id,gspace_id,plist_id)
  CALL h5tclose_f(mem,ierr)
END PROCEDURE hdf5_write_st1

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_st1_helper
  INTEGER( I4B ) :: length_max, i, local_gdims(1)
  length_max=0
  DO i=1,SIZE(vals)
    length_max=MAX(LEN_TRIM(vals(i)),length_max)
  ENDDO
  local_gdims(1:)=SHAPE(vals)
  IF(PRESENT(gdims_in)) local_gdims(1)=gdims_in(1)
  CALL hdf5_write_st1(obj,dsetname,vals,length_max,local_gdims)
END PROCEDURE hdf5_write_st1_helper

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_st2_helper
  INTEGER( I4B ) :: length_max, i, j, local_gdims(1)

  length_max=0
  DO j=1,SIZE(vals,1)
    DO i=1,SIZE(vals,2)
      length_max=MAX(LEN_TRIM(vals(j,i)),length_max)
    END DO
  END DO

  IF(PRESENT(gdims_in)) THEN
    CALL hdf5_write_st2(obj, dsetname,vals,length_max,gdims_in)
  ELSE
    CALL hdf5_write_st2(obj, dsetname,vals,length_max)
  ENDIF
END PROCEDURE hdf5_write_st2_helper

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_st2
  CHARACTER( LEN=length_max ) :: valss(SIZE(vals,1),SIZE(vals,2))
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( I4B ) :: j,k
  INTEGER( HSIZE_T ), DIMENSION( 2 ) :: gdims,ldims,offset,cnt
  INTEGER( I4B ), PARAMETER :: rank=2
  INTEGER( HID_T ) :: mem,dspace_id,dset_id,gspace_id,plist_id

  path=dsetname
  DO k = 1, SIZE(vals,2)
    DO j = 1, SIZE(vals,1)
      valss(j,k)=vals(j,k)%chars()
    END DO
  END DO

  ! stash offset
  offset(1)=LBOUND(vals,1)-1
  offset(2)=LBOUND(vals,2)-1
  IF(PRESENT(offset_in)) offset=offset_in

  ! Determine the dimensions for the dataspace
  ldims=SHAPE(vals)

  ! Store the dimensions from global if present
  IF(PRESENT(gdims_in)) THEN
    gdims=gdims_in
  ELSE
    gdims=ldims
  ENDIF
  cnt=gdims
  IF(PRESENT(cnt_in)) cnt=cnt_in

  CALL h5tcopy_f(H5T_NATIVE_CHARACTER,mem,ierr)
  CALL h5tset_strpad_f(mem,0,ierr)
  CALL h5tset_size_f(mem,INT(length_max,Real64),ierr)
  CALL preWrite(obj,rank,gdims,ldims,path,mem,dset_id,dspace_id, &
    & gspace_id,plist_id,ierr,cnt,offset)
  IF(ierr == 0) &
    CALL h5dwrite_f(dset_id,mem,valss,gdims,ierr,dspace_id,gspace_id,plist_id)
  CALL postWrite(obj,ierr,dset_id,dspace_id,gspace_id,plist_id)
  CALL h5tclose_f(mem,ierr)
END PROCEDURE hdf5_write_st2

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_st3_helper
  INTEGER( I4B ) :: length_max, i, j, local_gdims(1), k

  length_max=0
  DO k=1,SIZE(vals, 3)
    DO j=1,SIZE(vals, 2)
      DO i=1,SIZE(vals, 1)
        length_max=MAX(LEN_TRIM(vals(i,j,k)),length_max)
      END DO
    END DO
  END DO

  IF(PRESENT(gdims_in)) THEN
    CALL hdf5_write_st3(obj, dsetname,vals,length_max,gdims_in)
  ELSE
    CALL hdf5_write_st3(obj, dsetname,vals,length_max)
  ENDIF
END PROCEDURE hdf5_write_st3_helper

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_st3
  CHARACTER( LEN=length_max ) :: valss( SIZE(vals,1),SIZE(vals,2), &
    & SIZE(vals,3) )
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( I4B ) :: i,j,k
  INTEGER( HSIZE_T ), DIMENSION( 3 ) :: gdims,ldims,offset,cnt
  INTEGER( I4B ), PARAMETER :: rank=3
  INTEGER( HID_T ) :: mem,dspace_id,dset_id,gspace_id,plist_id

  path=dsetname
  DO i = 1, SIZE(vals, 3)
    DO k = 1, SIZE(vals, 2)
      DO j = 1, SIZE(vals, 1)
        valss(j,k,i)=vals(j,k,i)%chars()
      END DO
    END DO
  END DO

  ! stash offset
  offset( 1 ) = LBOUND( vals, 1 ) - 1
  offset( 2 ) = LBOUND( vals, 2 ) - 1
  offset( 3 ) = LBOUND( vals, 3 ) - 1
  IF( PRESENT( offset_in ) ) offset = offset_in

  ! Determine the dimensions for the dataspace
  ldims=SHAPE(vals)

  ! Store the dimensions from global if present
  IF( PRESENT( gdims_in ) ) THEN
    gdims = gdims_in
  ELSE
    gdims = ldims
  ENDIF
  cnt = gdims
  IF( PRESENT( cnt_in ) ) cnt = cnt_in
  CALL h5tcopy_f(H5T_NATIVE_CHARACTER,mem,ierr)
  CALL h5tset_strpad_f(mem,0,ierr)
  CALL h5tset_size_f(mem,INT(length_max,Real64),ierr)
  CALL preWrite(obj,rank,gdims,ldims,path,mem,dset_id,dspace_id, &
    & gspace_id,plist_id,ierr,cnt,offset)
  IF(ierr == 0) &
    CALL h5dwrite_f(dset_id,mem,valss,gdims,ierr,dspace_id,gspace_id,plist_id)
  CALL postWrite(obj,ierr,dset_id,dspace_id,gspace_id,plist_id)
  CALL h5tclose_f(mem,ierr)
END PROCEDURE hdf5_write_st3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_c1
  CHARACTER( LEN=LEN(dsetname) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: ldims,offset,gdims,cnt
  INTEGER( I4B ), PARAMETER :: rank=1
  INTEGER( HID_T ) :: mem,dspace_id,dset_id,gspace_id,plist_id
  INTEGER( I4B ) :: error
  ! stash offset
  offset(1)=1
  IF(PRESENT(offset_in)) offset=offset_in
  path=dsetname
  ! Determine the dimensions for the dataspace
  ldims(1)=LEN(vals)
  ! Store the dimensions from global if present
  IF(PRESENT(gdims_in)) THEN
    gdims(1)=gdims_in
  ELSE
    gdims(1)=ldims(1)
  ENDIF
  cnt=gdims
  IF(PRESENT(cnt_in)) cnt=cnt_in
  mem=H5T_NATIVE_CHARACTER
  CALL preWrite(obj,rank,gdims,ldims,path,mem,dset_id,dspace_id, &
      gspace_id,plist_id,error,cnt,offset)
  IF(error == 0) &
      CALL h5dwrite_f(dset_id,mem,vals,gdims,error,dspace_id,gspace_id,plist_id)
  CALL postWrite(obj,error,dset_id,dspace_id,gspace_id,plist_id)
END PROCEDURE hdf5_write_c1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE WriteString