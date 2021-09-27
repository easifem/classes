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

SUBMODULE( HDF5File_Class ) WriteReal64
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

#ifdef mem_type
#undef mem_type
#endif
#define mem_type H5T_NATIVE_DOUBLE

MODULE PROCEDURE hdf5_write_d0
#define rank 0
#include "./write.inc"
#undef rank
END PROCEDURE hdf5_write_d0

!----------------------------------------------------------------------------
!                                                                  Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d1
#define rank 1
#include "./write.inc"
#undef rank
END PROCEDURE hdf5_write_d1

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d2
#define rank 2
#include "./write.inc"
#undef rank
END PROCEDURE hdf5_write_d2

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d3
#define rank 3
#include "./write.inc"
#undef rank
END PROCEDURE hdf5_write_d3

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d4
#define rank 4
#include "./write.inc"
#undef rank
END PROCEDURE hdf5_write_d4

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d5
#define rank 5
#include "./write.inc"
#undef rank
END PROCEDURE hdf5_write_d5

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d6
#define rank 6
#include "./write.inc"
#undef rank
END PROCEDURE hdf5_write_d6

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d7
#define rank 7
#include "./write.inc"
#undef rank
END PROCEDURE hdf5_write_d7

END SUBMODULE WriteReal64