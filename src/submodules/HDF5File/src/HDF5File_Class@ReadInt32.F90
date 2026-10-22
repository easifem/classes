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
USE HDF5, ONLY: H5T_NATIVE_INTEGER, H5DREAD_F
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@ReadInt32.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_n0
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_n0()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 0
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

! Read the dataset
  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, ierr)
  mem = H5T_NATIVE_INTEGER
  IF (ierr .GE. 0) THEN
    CALL H5DREAD_F(dset_id, mem, vals, dims, ierr)
  END IF

  CALL postRead(obj, path%Chars(), dset_id, dspace_id, ierr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_n0

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_n1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_n1()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 1
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

! Allocate space if needed, make sure it is the right size
  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

  IF (error >= 0) THEN
    CALL Reallocate(vals, INT(dims, I4B))
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals, dims, error)
  END IF

  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_n1

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__n1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__n1()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 1
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  tsize = math%zero_i

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN

#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals), myName, &
      'a=dims(1) from hdf5File, b=SIZE(vals)')
#endif

    tsize = INT(dims(1), I4B)
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals(1:tsize), dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__n1

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_n2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_n2()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 2
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN
    CALL Reallocate(vals, INT(dims, I4B))
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals, dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_n2

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__n2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__n2()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 2
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  nrow = math%zero_i
  ncol = math%zero_i

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN

#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
    CALL AssertError3(INT(dims(2), I4B), SIZE(vals, 2), myName, &
      'a=dims(2) from hdf5File, b=SIZE(vals, 2)')
#endif

    nrow = INT(dims(1), I4B)
    ncol = INT(dims(2), I4B)
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals(1:nrow, 1:ncol), dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__n2

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_n3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_n3()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 3
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN
    CALL Reallocate(vals, INT(dims, I4B))
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals, dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_n3

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__n3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__n3()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 3
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  dim1 = math%zero_i
  dim2 = math%zero_i
  dim3 = math%zero_i

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN

#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
    CALL AssertError3(INT(dims(2), I4B), SIZE(vals, 2), myName, &
      'a=dims(2) from hdf5File, b=SIZE(vals, 2)')
    CALL AssertError3(INT(dims(3), I4B), SIZE(vals, 3), myName, &
      'a=dims(3) from hdf5File, b=SIZE(vals, 3)')
#endif

    dim1 = INT(dims(1), I4B)
    dim2 = INT(dims(2), I4B)
    dim3 = INT(dims(3), I4B)
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals(1:dim1, 1:dim2, 1:dim3), dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__n3

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_n4
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_n4()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 4
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

  IF (error >= 0) THEN
    CALL Reallocate(vals, INT(dims, I4B))
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals, dims, error)
  END IF

  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_n4

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__n4
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__n4()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 4
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  dim1 = math%zero_i
  dim2 = math%zero_i
  dim3 = math%zero_i
  dim4 = math%zero_i

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN

#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
    CALL AssertError3(INT(dims(2), I4B), SIZE(vals, 2), myName, &
      'a=dims(2) from hdf5File, b=SIZE(vals, 2)')
    CALL AssertError3(INT(dims(3), I4B), SIZE(vals, 3), myName, &
      'a=dims(3) from hdf5File, b=SIZE(vals, 3)')
    CALL AssertError3(INT(dims(4), I4B), SIZE(vals, 4), myName, &
      'a=dims(4) from hdf5File, b=SIZE(vals, 4)')
#endif

    dim1 = INT(dims(1), I4B)
    dim2 = INT(dims(2), I4B)
    dim3 = INT(dims(3), I4B)
    dim4 = INT(dims(4), I4B)
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals(1:dim1, 1:dim2, 1:dim3, 1:dim4), dims, &
      error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__n4

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_n5
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_n5()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 5
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN
    CALL Reallocate(vals, INT(dims, I4B))
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals, dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_n5

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__n5
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__n5()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 5
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  dim1 = math%zero_i
  dim2 = math%zero_i
  dim3 = math%zero_i
  dim4 = math%zero_i
  dim5 = math%zero_i

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN

#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
    CALL AssertError3(INT(dims(2), I4B), SIZE(vals, 2), myName, &
      'a=dims(2) from hdf5File, b=SIZE(vals, 2)')
    CALL AssertError3(INT(dims(3), I4B), SIZE(vals, 3), myName, &
      'a=dims(3) from hdf5File, b=SIZE(vals, 3)')
    CALL AssertError3(INT(dims(4), I4B), SIZE(vals, 4), myName, &
      'a=dims(4) from hdf5File, b=SIZE(vals, 4)')
    CALL AssertError3(INT(dims(5), I4B), SIZE(vals, 5), myName, &
      'a=dims(5) from hdf5File, b=SIZE(vals, 5)')
#endif

    dim1 = INT(dims(1), I4B)
    dim2 = INT(dims(2), I4B)
    dim3 = INT(dims(3), I4B)
    dim4 = INT(dims(4), I4B)
    dim5 = INT(dims(5), I4B)
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals(1:dim1, 1:dim2, 1:dim3, 1:dim4, &
      1:dim5), dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__n5

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_n6
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_n6()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 6
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN
    CALL Reallocate(vals, INT(dims, I4B))
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals, dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_n6

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__n6
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__n6()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 6
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  dim1 = math%zero_i
  dim2 = math%zero_i
  dim3 = math%zero_i
  dim4 = math%zero_i
  dim5 = math%zero_i
  dim6 = math%zero_i

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN

#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
    CALL AssertError3(INT(dims(2), I4B), SIZE(vals, 2), myName, &
      'a=dims(2) from hdf5File, b=SIZE(vals, 2)')
    CALL AssertError3(INT(dims(3), I4B), SIZE(vals, 3), myName, &
      'a=dims(3) from hdf5File, b=SIZE(vals, 3)')
    CALL AssertError3(INT(dims(4), I4B), SIZE(vals, 4), myName, &
      'a=dims(4) from hdf5File, b=SIZE(vals, 4)')
    CALL AssertError3(INT(dims(5), I4B), SIZE(vals, 5), myName, &
      'a=dims(5) from hdf5File, b=SIZE(vals, 5)')
    CALL AssertError3(INT(dims(6), I4B), SIZE(vals, 6), myName, &
      'a=dims(6) from hdf5File, b=SIZE(vals, 6)')
#endif

    dim1 = INT(dims(1), I4B)
    dim2 = INT(dims(2), I4B)
    dim3 = INT(dims(3), I4B)
    dim4 = INT(dims(4), I4B)
    dim5 = INT(dims(5), I4B)
    dim6 = INT(dims(6), I4B)
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals(1:dim1, 1:dim2, 1:dim3, 1:dim4, &
      1:dim5, 1:dim6), dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__n6

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_n7
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_n7()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 7
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN
    CALL Reallocate(vals, INT(dims, I4B))
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals, dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_n7

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__n7
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__n7()"
#endif
  INTEGER(I4B), PARAMETER :: rank = 7
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem
  INTEGER(HID_T) :: dspace_id, dset_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  dim1 = math%zero_i
  dim2 = math%zero_i
  dim3 = math%zero_i
  dim4 = math%zero_i
  dim5 = math%zero_i
  dim6 = math%zero_i
  dim7 = math%zero_i

  path = dsetname
  CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
  IF (error >= 0) THEN

#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
    CALL AssertError3(INT(dims(2), I4B), SIZE(vals, 2), myName, &
      'a=dims(2) from hdf5File, b=SIZE(vals, 2)')
    CALL AssertError3(INT(dims(3), I4B), SIZE(vals, 3), myName, &
      'a=dims(3) from hdf5File, b=SIZE(vals, 3)')
    CALL AssertError3(INT(dims(4), I4B), SIZE(vals, 4), myName, &
      'a=dims(4) from hdf5File, b=SIZE(vals, 4)')
    CALL AssertError3(INT(dims(5), I4B), SIZE(vals, 5), myName, &
      'a=dims(5) from hdf5File, b=SIZE(vals, 5)')
    CALL AssertError3(INT(dims(6), I4B), SIZE(vals, 6), myName, &
      'a=dims(6) from hdf5File, b=SIZE(vals, 6)')
    CALL AssertError3(INT(dims(7), I4B), SIZE(vals, 7), myName, &
      'a=dims(7) from hdf5File, b=SIZE(vals, 7)')
#endif

    dim1 = INT(dims(1), I4B)
    dim2 = INT(dims(2), I4B)
    dim3 = INT(dims(3), I4B)
    dim4 = INT(dims(4), I4B)
    dim5 = INT(dims(5), I4B)
    dim6 = INT(dims(6), I4B)
    dim7 = INT(dims(7), I4B)
    mem = H5T_NATIVE_INTEGER
    CALL H5DREAD_F(dset_id, mem, vals(1:dim1, 1:dim2, 1:dim3, 1:dim4, &
      1:dim5, 1:dim6, 1:dim7), dims, error)
  END IF
  CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__n7

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadInt32
