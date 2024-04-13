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

SUBMODULE(HDF5File_Class) ReadString
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st0_helper
  INTEGER( I4B ), PARAMETER :: rank=0
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( I4B ) :: ndims
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id
  INTEGER( SIZE_T ) :: max_size
  INTEGER( I4B ) :: error

  mem = -1
  dspace_id = -1
  dset_id = -1
  path = dsetname

  CALL h5dopen_f( obj%file_id, path, dset_id, error )
  CALL h5dget_type_f( dset_id, mem, error )
  CALL h5tget_size_f( mem, max_size, error )
  CALL h5dget_space_f( dset_id, dspace_id, error )
  CALL h5sget_simple_extent_ndims_f( dspace_id, ndims, error )

  IF((ndims .EQ. rank+1) .AND. (max_size .EQ. 1)) THEN
    CALL hdf5_read_ca0(obj,dsetname,vals)
  ELSE
    CALL hdf5_read_st0(obj,dsetname,INT(max_size,I4B),vals)
  ENDIF
  CALL h5sclose_f(dspace_id,error)
  CALL h5dclose_f(dset_id,error)
END PROCEDURE hdf5_read_st0_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st0
  CHARACTER( LEN = length_max ), ALLOCATABLE :: valsc( : )
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  INTEGER( I4B ), PARAMETER :: rank=0
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id, dset_id
  INTEGER( I4B ) :: error

  ! Allocate surrogate data
  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE(valsc(dims(1)))
    ! Read the dataset
    CALL h5dget_type_f(dset_id,mem,error)
    CALL h5dread_f(dset_id,mem,valsc,dims,error)

    IF(length_max==1) THEN
      CALL convert_char_array_to_str(valsc,vals)
    ELSE
      vals=valsc(1)(1:length_max)
    ENDIF
    vals = vals%replace(C_NULL_CHAR,'')
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
  IF( ALLOCATED( valsc ) ) DEALLOCATE( valsc )
END PROCEDURE hdf5_read_st0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_ca0
  INTEGER( I4B ), PARAMETER :: rank=1
  CHARACTER( LEN = 1 ), ALLOCATABLE :: valsc(:)
  INTEGER( I4B ) :: i, error
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id,dset_id

  path = dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE(valsc(dims(1)))
    CALL h5dget_type_f(dset_id,mem,error)
    CALL h5dread_f(dset_id,mem,valsc,dims,error)
    ! Allocate space if needed, make sure it is the right size
    vals=''
    ! Convert to StringType
    DO i=1,SIZE(valsc)
      vals=vals//valsc(i)
    ENDDO
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
  IF(ALLOCATED(valsc)) DEALLOCATE(valsc)
END PROCEDURE hdf5_read_ca0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE convert_char_array_to_str( char_array, s )
  CHARACTER( LEN=1 ), INTENT( IN ) :: char_array( : )
  TYPE( String ), INTENT( INOUT ) :: s
  CHARACTER( LEN=SIZE(char_array) ) :: c
  INTEGER( I4B ) :: i
  DO i=1,LEN(c)
    c(i:i)=char_array(i)
  ENDDO
  s=c
END SUBROUTINE convert_char_array_to_str

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st1_helper
  CHARACTER( LEN = LEN( dsetname ) + 1 ) :: path
  INTEGER( I4B ), PARAMETER :: rank=1
  INTEGER( I4B ) :: ndims, error
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id
  INTEGER( SIZE_T ) :: max_size

  mem = -1
  dspace_id = -1
  dset_id = -1
  path = dsetname
  CALL h5dopen_f(obj%file_id, path, dset_id, error)
  CALL h5dget_type_f(dset_id,mem,error)
  CALL h5tget_size_f(mem,max_size,error)
  CALL h5dget_space_f(dset_id,dspace_id,error)
  CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
  IF((ndims .EQ. rank+1) .AND. (max_size .EQ. 1)) THEN
    CALL hdf5_read_ca1(obj,dsetname,vals)
  ELSE
    CALL hdf5_read_st1(obj,dsetname,INT(max_size,I4B),vals)
  ENDIF
  CALL h5sclose_f(dspace_id,error)
  CALL h5dclose_f(dset_id,error)
END PROCEDURE hdf5_read_st1_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st1
  CHARACTER( LEN=length_max ), ALLOCATABLE :: valsc(:)
  INTEGER( I4B ) :: i, error
  CHARACTER( LEN = LEN(dsetname) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  INTEGER( I4B ), PARAMETER :: rank=1
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id

  path=dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE( valsc( dims(1) ) )
    CALL h5dget_type_f(dset_id,mem,error)
    CALL h5dread_f(dset_id,mem,valsc,dims,error)
    IF(ALLOCATED(vals)) THEN
      IF(SIZE(vals) .NE. dims(1)) THEN
        DEALLOCATE(vals)
        ALLOCATE(vals(dims(1)))
      ENDIF
    ELSE
      ALLOCATE(vals(dims(1)))
    ENDIF
    DO i=1,SIZE(vals)
      vals(i)=valsc(i)(1:length_max)
    ENDDO

    !Find replace C_NULL_CHARs from HDF5
    DO i=1,SIZE(vals)
      vals(i) = vals(i)%replace(C_NULL_CHAR,'')
    ENDDO
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
  IF(ALLOCATED(valsc)) DEALLOCATE(valsc)
END PROCEDURE hdf5_read_st1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_ca1
  CHARACTER( LEN=1) , ALLOCATABLE :: valsc(:,:)
  INTEGER( I4B ) :: i,j,error
  CHARACTER( LEN = LEN(dsetname) + 1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 2 ) :: dims
  INTEGER( I4B ), PARAMETER :: rank=2
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id

  path=dsetname
  ! Allocate character array to size
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE(valsc(dims(1),dims(2)))
    CALL h5dget_type_f(dset_id,mem,error)
    CALL h5dread_f(dset_id,mem,valsc,dims,error)
    ! Allocate space if needed, make sure it is the right size
    IF(ALLOCATED(vals)) THEN
      IF(SIZE(vals) /= dims(2)) THEN
        DEALLOCATE(vals)
        ALLOCATE(vals(dims(2)))
      ENDIF
    ELSE
      ALLOCATE(vals(dims(2)))
    ENDIF
    ! Convert to StringType
    DO i=1,SIZE(vals)
      vals(i)=''
      DO j=1,SIZE(valsc(:,i))
        vals(i)=vals(i)//valsc(j,i)
      ENDDO
    ENDDO
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
  IF(ALLOCATED(valsc)) DEALLOCATE(valsc)
END PROCEDURE hdf5_read_ca1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st2_helper
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( I4B ), PARAMETER :: rank=2
  INTEGER( I4B ) :: ndims, error
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id
  INTEGER( SIZE_T ) :: max_size

  mem = -1
  dspace_id = -1
  dset_id = -1
  path=dsetname
  CALL h5dopen_f(obj%file_id, path, dset_id, error)
  CALL h5dget_type_f(dset_id,mem,error)
  CALL h5tget_size_f(mem,max_size,error)
  CALL h5dget_space_f(dset_id,dspace_id,error)
  CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
  IF((ndims==rank+1) .AND. (max_size==1)) THEN
    CALL hdf5_read_ca2(obj,dsetname,vals)
  ELSE
    CALL hdf5_read_st2(obj,dsetname,INT(max_size,I4B),vals)
  ENDIF
  CALL h5sclose_f(dspace_id,error)
  CALL h5dclose_f(dset_id,error)
END PROCEDURE hdf5_read_st2_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st2
  CHARACTER( LEN=length_max ), ALLOCATABLE :: valsc(:,:)
  INTEGER( I4B ) :: i,j,error
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 2 ) :: dims
  INTEGER( I4B ), PARAMETER :: rank=2
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id

  path=dsetname
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE(valsc(dims(1),dims(2)))
    CALL h5dget_type_f(dset_id,mem,error)
    CALL h5dread_f(dset_id,mem,valsc,dims,error)
    IF(ALLOCATED(vals)) THEN
      IF(ALL(SHAPE(vals) /= (/dims(1),dims(2)/))) THEN
        DEALLOCATE(vals)
        ALLOCATE(vals(dims(1),dims(2)))
      ENDIF
    ELSE
      ALLOCATE(vals(dims(1),dims(2)))
    ENDIF
    DO i=1,SIZE(vals,1)
      DO j=1,SIZE(vals,2)
        vals(i,j)=valsc(i,j)(1:length_max)
      ENDDO
    ENDDO
    DO j=1,SIZE(vals,DIM=2)
      DO i=1,SIZE(vals,DIM=1)
        vals(i,j) = vals(i,j)%replace(C_NULL_CHAR,'')
      ENDDO
    ENDDO
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
  IF(ALLOCATED(valsc)) DEALLOCATE(valsc)
END PROCEDURE hdf5_read_st2

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_ca2
  CHARACTER( LEN=1 ), ALLOCATABLE :: valsc(:,:,:)
  INTEGER( I4B ) :: i,j,k,error
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 3 ) :: dims
  INTEGER( I4B ), PARAMETER :: rank=3
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id

  path=dsetname
  ! Allocate character array to size
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE(valsc(dims(1),dims(2),dims(3)))
    CALL h5dget_type_f(dset_id,mem,error)
    CALL h5dread_f(dset_id,mem,valsc,dims,error)
    ! Allocate space if needed, make sure it is the right size
    IF(ALLOCATED(vals)) THEN
      IF(ALL(SHAPE(vals) /= (/dims(2),dims(3)/))) THEN
        DEALLOCATE(vals)
        ALLOCATE(vals(dims(2),dims(3)))
      ENDIF
    ELSE
      ALLOCATE(vals(dims(2),dims(3)))
    ENDIF
    ! Convert to StringType
    DO i=1,SIZE(vals,1)
      DO j=1,SIZE(vals,2)
        vals(i,j)=''
        DO k=1,SIZE(valsc(:,i,j))
          vals(i,j)=vals(i,j)//valsc(k,i,j)
        ENDDO
      ENDDO
    ENDDO
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
  IF(ALLOCATED(valsc)) DEALLOCATE(valsc)
END PROCEDURE hdf5_read_ca2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st3_helper
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( I4B ), PARAMETER :: rank=3
  INTEGER( I4B ) :: ndims, error
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id
  INTEGER( SIZE_T ) :: max_size

  mem = -1
  dspace_id = -1
  dset_id = -1
  path=dsetname
  CALL h5dopen_f(obj%file_id, path, dset_id, error)
  CALL h5dget_type_f(dset_id,mem,error)
  CALL h5tget_size_f(mem,max_size,error)
  CALL h5dget_space_f(dset_id,dspace_id,error)
  CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
  IF((ndims==rank+1) .AND. (max_size==1)) THEN
    CALL hdf5_read_ca3(obj,dsetname,vals)
  ELSE
    CALL hdf5_read_st3(obj,dsetname,INT(max_size,I4B),vals)
  ENDIF
  CALL h5sclose_f(dspace_id,error)
  CALL h5dclose_f(dset_id,error)
END PROCEDURE hdf5_read_st3_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st3
  CHARACTER( LEN=length_max ), ALLOCATABLE :: valsc(:,:,:)
  INTEGER( I4B ) :: i,j,k,error
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 3 ) :: dims
  INTEGER( I4B ), PARAMETER :: rank=3
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id

  path=dsetname
  ! Allocate character array to size
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE(valsc(dims(1),dims(2),dims(3)))
    CALL h5dget_type_f(dset_id,mem,error)
    CALL h5dread_f(dset_id,mem,valsc,dims,error)
    IF(ALLOCATED(vals)) THEN
      IF(ALL(SHAPE(vals) /= (/dims(1),dims(2),dims(3)/))) THEN
        DEALLOCATE(vals)
        ALLOCATE(vals(dims(1),dims(2),dims(3)))
      END IF
    ELSE
      ALLOCATE(vals(dims(1),dims(2),dims(3)))
    END IF
    DO i=1,SIZE(vals,1)
      DO j=1,SIZE(vals,2)
        DO k=1,SIZE(vals,3)
          vals(i,j,k)=valsc(i,j,k)(1:length_max)
        END DO
      END DO
    END DO
    !Find replace C_NULL_CHARs from HDF5
    DO k=1,SIZE(vals,DIM=3)
      DO j=1,SIZE(vals,DIM=2)
        DO i=1,SIZE(vals,DIM=1)
          vals(i,j,k) = vals(i,j,k)%replace(C_NULL_CHAR,'')
        END DO
      END DO
    END DO
  END IF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_st3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_ca3
  CHARACTER( LEN=1 ), ALLOCATABLE :: valsc(:,:,:,:)
  INTEGER( I4B ) :: i,j,k,m,error
  CHARACTER( LEN=LEN(dsetname)+1 ) :: path
  INTEGER( HSIZE_T ), DIMENSION( 4 ) :: dims
  INTEGER( I4B ), PARAMETER :: rank=4
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id

  path=dsetname
  ! Allocate character array to size
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE(valsc(dims(1),dims(2),dims(3),dims(4)))
    CALL h5dget_type_f(dset_id,mem,error)
    CALL h5dread_f(dset_id,mem,valsc,dims,error)
    ! Allocate space if needed, make sure it is the right size
    IF(ALLOCATED(vals)) THEN
      IF(ALL(SHAPE(vals) /= (/dims(2),dims(3),dims(4)/))) THEN
        DEALLOCATE(vals)
        ALLOCATE(vals(dims(2),dims(3),dims(4)))
      ENDIF
    ELSE
      ALLOCATE(vals(dims(2),dims(3),dims(4)))
    ENDIF
    ! Convert to StringType
    DO i=1,SIZE(vals,1)
      DO j=1,SIZE(vals,2)
        DO m=1,SIZE(vals,3)
          vals(i,j,m)=''
          DO k=1,SIZE(valsc(:,i,j,m))
            vals(i,j,m)=vals(i,j,m)//valsc(k,i,j,m)
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_ca3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_c1
  CHARACTER, ALLOCATABLE :: valsc(:)
  CHARACTER( LEN = LEN(dsetname)+1 ) :: path
  INTEGER( I4B ) :: i, error
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  INTEGER( I4B ), PARAMETER :: rank=1
  INTEGER( HID_T ) :: mem
  INTEGER( HID_T ) :: dspace_id,dset_id

  path=dsetname
  ! Allocate surrogate data
  CALL preRead(obj,path,rank,dset_id,dspace_id,dims,error)
  IF(error >= 0) THEN
    ALLOCATE(valsc(dims(1)))
    ! Read the dataset
    mem=H5T_NATIVE_CHARACTER
    CALL h5dread_f(dset_id,mem,valsc,dims,error)

    ! Convert from surrogate character array to boolean array
    vals(:)=" "
    DO i=1,SIZE(valsc)
      vals(i:i)=valsc(i)
    ENDDO
  ENDIF
  CALL postRead(obj,path,dset_id,dspace_id,error)
END PROCEDURE hdf5_read_c1
END SUBMODULE ReadString