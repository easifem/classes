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

MODULE VTKDataArrayEncoder
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank1_Real32(x, fmt) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank1_Real32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank1_Real32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank1_Real64(x, fmt) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank1_Real64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank1_Real64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank1_Int8(x, fmt) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank1_Int8
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank1_Int8
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank1_Int16(x, fmt) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank1_Int16
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank1_Int16
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank1_Int32(x, fmt) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank1_Int32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank1_Int32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank1_Int64(x, fmt) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank1_Int64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank1_Int64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank2_Real32(x, fmt) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank2_Real32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank2_Real32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank2_Real64(x, fmt) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank2_Real64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank2_Real64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank2_Int8(x, fmt) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank2_Int8
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank2_Int8
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank2_Int16(x, fmt) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank2_Int16
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank2_Int16
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank2_Int32(x, fmt) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank2_Int32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank2_Int32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank2_Int64(x, fmt) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank2_Int64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank2_Int64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank3Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank3_Real32(x, fmt) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank3_Real32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank3_Real32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank3Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank3_Real64(x, fmt) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank3_Real64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank3_Real64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank3Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank3_Int8(x, fmt) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank3_Int8
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank3_Int8
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank3Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank3_Int16(x, fmt) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank3_Int16
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank3_Int16
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank3Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank3_Int32(x, fmt) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank3_Int32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank3_Int32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank3Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank3_Int64(x, fmt) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank3_Int64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank3_Int64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank4Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank4_Real32(x, fmt) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank4_Real32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank4_Real32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank4Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank4_Real64(x, fmt) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank4_Real64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank4_Real64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank4Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank4_Int8(x, fmt) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank4_Int8
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank4_Int8
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank4Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank4_Int16(x, fmt) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank4_Int16
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank4_Int16
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank4Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank4_Int32(x, fmt) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank4_Int32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank4_Int32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank4Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_rank4_Int64(x, fmt) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_rank4_Int64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_rank4_Int64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank1_Real32(x, y, z, fmt) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(1:)
    REAL(REAL32), INTENT(IN) :: y(1:)
    REAL(REAL32), INTENT(IN) :: z(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank1_Real32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank1_Real32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank1_Real64(x, y, z, fmt) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(1:)
    REAL(REAL64), INTENT(IN) :: y(1:)
    REAL(REAL64), INTENT(IN) :: z(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank1_Real64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank1_Real64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank1_Int8(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(1:)
    INTEGER(INT8), INTENT(IN) :: y(1:)
    INTEGER(INT8), INTENT(IN) :: z(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank1_Int8
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank1_Int8
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank1_Int16(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(1:)
    INTEGER(INT16), INTENT(IN) :: y(1:)
    INTEGER(INT16), INTENT(IN) :: z(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank1_Int16
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank1_Int16
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank1_Int32(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(1:)
    INTEGER(INT32), INTENT(IN) :: y(1:)
    INTEGER(INT32), INTENT(IN) :: z(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank1_Int32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank1_Int32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank1Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank1_Int64(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(1:)
    INTEGER(INT64), INTENT(IN) :: y(1:)
    INTEGER(INT64), INTENT(IN) :: z(1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank1_Int64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank1_Int64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank2_Real32(x, y, z, fmt) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(1:, 1:)
    REAL(REAL32), INTENT(IN) :: y(1:, 1:)
    REAL(REAL32), INTENT(IN) :: z(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank2_Real32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank2_Real32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank2_Real64(x, y, z, fmt) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(1:, 1:)
    REAL(REAL64), INTENT(IN) :: y(1:, 1:)
    REAL(REAL64), INTENT(IN) :: z(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank2_Real64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank2_Real64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank2_Int8(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT8), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT8), INTENT(IN) :: z(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank2_Int8
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank2_Int8
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank2_Int16(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT16), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT16), INTENT(IN) :: z(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank2_Int16
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank2_Int16
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank2_Int32(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT32), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT32), INTENT(IN) :: z(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank2_Int32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank2_Int32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank2_Int64(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT64), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT64), INTENT(IN) :: z(1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank2_Int64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank2_Int64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank3_Real32(x, y, z, fmt) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(1:, 1:, 1:)
    REAL(REAL32), INTENT(IN) :: y(1:, 1:, 1:)
    REAL(REAL32), INTENT(IN) :: z(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank3_Real32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank3_Real32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank3_Real64(x, y, z, fmt) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(1:, 1:, 1:)
    REAL(REAL64), INTENT(IN) :: y(1:, 1:, 1:)
    REAL(REAL64), INTENT(IN) :: z(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank3_Real64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank3_Real64
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank3_Int8(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT8), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT8), INTENT(IN) :: z(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank3_Int8
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank3_Int8
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank3_Int16(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT16), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT16), INTENT(IN) :: z(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank3_Int16
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank3_Int16
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank3_Int32(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT32), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT32), INTENT(IN) :: z(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank3_Int32
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank3_Int32
END INTERFACE EncodeVTKDataArray

!----------------------------------------------------------------------------
!                                           EncodeVTKDataArray@Rank2Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION encode_xyz_rank3_Int64(x, y, z, fmt) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT64), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT64), INTENT(IN) :: z(1:, 1:, 1:)
    CHARACTER(LEN=*), INTENT(IN) :: fmt
    !! fmt is encoding format, ASCII or BINARY
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION encode_xyz_rank3_Int64
END INTERFACE

INTERFACE EncodeVTKDataArray
  MODULE PROCEDURE encode_xyz_rank3_Int64
END INTERFACE EncodeVTKDataArray

END MODULE VTKDataArrayEncoder

