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

SUBMODULE(VTKDataArrayEncoder) XYZRank2Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_xyz_rank2_Real32
  INTEGER( I4B ) :: i, j, jj, ii, nbyte
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 )
  nbyte = (SIZE( x ) + SIZE( y ) + SIZE( z )) * BYReal32
#include "./include/ENCODE_XYZ_RANK2.F90"
END PROCEDURE encode_xyz_rank2_Real32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_xyz_rank2_Real64
  INTEGER( I4B ) :: i, j, jj, ii, nbyte
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 )
  nbyte = (SIZE( x ) + SIZE( y ) + SIZE( z )) * BYReal64
#include "./include/ENCODE_XYZ_RANK2.F90"
END PROCEDURE encode_xyz_rank2_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_xyz_rank2_Int64
  INTEGER( I4B ) :: i, j, jj, ii, nbyte
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 )
  nbyte = (SIZE( x ) + SIZE( y ) + SIZE( z )) * BYInt64
#include "./include/ENCODE_XYZ_RANK2.F90"
END PROCEDURE encode_xyz_rank2_Int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_xyz_rank2_Int32
  INTEGER( I4B ) :: i, j, jj, ii, nbyte
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 )
  nbyte = (SIZE( x ) + SIZE( y ) + SIZE( z )) * BYInt32
#include "./include/ENCODE_XYZ_RANK2.F90"
END PROCEDURE encode_xyz_rank2_Int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_xyz_rank2_Int16
  INTEGER( I4B ) :: i, j, jj, ii, nbyte
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 )
  nbyte = (SIZE( x ) + SIZE( y ) + SIZE( z )) * BYInt16
#include "./include/ENCODE_XYZ_RANK2.F90"
END PROCEDURE encode_xyz_rank2_Int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_xyz_rank2_Int8
  INTEGER( I4B ) :: i, j, jj, ii, nbyte
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 )
  nbyte = (SIZE( x ) + SIZE( y ) + SIZE( z )) * BYInt8
#include "./include/ENCODE_XYZ_RANK2.F90"
END PROCEDURE encode_xyz_rank2_Int8

END SUBMODULE XYZRank2Methods
