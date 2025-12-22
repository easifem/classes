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

SUBMODULE(VTKDataArrayEncoder) Rank3Methods
USE befor64, ONLY: B64_ENCODE, PACK_DATA
USE penf, ONLY: str
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank3_Real32
INTEGER(I4B) :: ii, jj, kk, nn, mm, ll
INTEGER(INT8), ALLOCATABLE :: xp(:)
!> main
ans = ''; mm = SIZE(x, 1); nn = SIZE(x, 2); ll = SIZE(x, 3)
SELECT CASE (TRIM(fmt))
CASE ("ASCII")
  DO kk = 1, ll
    DO jj = 1, nn
      DO ii = 1, mm - 1
        ans = ans//str(n=x(ii, jj, kk))//' '
      END DO
      ans = ans//' '//str(n=x(mm, jj, kk))
    END DO
  END DO
  !>
CASE ("BINARY")
  nn = nn * mm * ll
  CALL PACK_DATA( &
    & a1=[INT(nn * BYReal32, I4B)], &
    & a2=RESHAPE(x, [nn]), packed=xp)
  CALL B64_ENCODE(n=xp, code=ans)
END SELECT
END PROCEDURE encode_rank3_Real32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank3_Real64
INTEGER(I4B) :: ii, jj, kk, nn, mm, ll
INTEGER(INT8), ALLOCATABLE :: xp(:)
!> main
ans = ''; mm = SIZE(x, 1); nn = SIZE(x, 2); ll = SIZE(x, 3)
SELECT CASE (TRIM(fmt))
CASE ("ASCII")
  DO kk = 1, ll
    DO jj = 1, nn
      DO ii = 1, mm - 1
        ans = ans//str(n=x(ii, jj, kk))//' '
      END DO
      ans = ans//' '//str(n=x(mm, jj, kk))
    END DO
  END DO
  !>
CASE ("BINARY")
  nn = nn * mm * ll
  CALL PACK_DATA( &
    & a1=[INT(nn * BYReal64, I4B)], &
    & a2=RESHAPE(x, [nn]), packed=xp)
  CALL B64_ENCODE(n=xp, code=ans)
END SELECT
END PROCEDURE encode_rank3_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank3_Int64
INTEGER(I4B) :: ii, jj, kk, nn, mm, ll
INTEGER(INT8), ALLOCATABLE :: xp(:)
!> main
ans = ''; mm = SIZE(x, 1); nn = SIZE(x, 2); ll = SIZE(x, 3)
SELECT CASE (TRIM(fmt))
CASE ("ASCII")
  DO kk = 1, ll
    DO jj = 1, nn
      DO ii = 1, mm - 1
        ans = ans//str(n=x(ii, jj, kk))//' '
      END DO
      ans = ans//' '//str(n=x(mm, jj, kk))
    END DO
  END DO
  !>
CASE ("BINARY")
  nn = nn * mm * ll
  CALL PACK_DATA( &
    & a1=[INT(nn * BYInt64, I4B)], &
    & a2=RESHAPE(x, [nn]), packed=xp)
  CALL B64_ENCODE(n=xp, code=ans)
END SELECT
END PROCEDURE encode_rank3_Int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank3_Int32
INTEGER(I4B) :: ii, jj, kk, nn, mm, ll
INTEGER(INT8), ALLOCATABLE :: xp(:)
!> main
ans = ''; mm = SIZE(x, 1); nn = SIZE(x, 2); ll = SIZE(x, 3)
SELECT CASE (TRIM(fmt))
CASE ("ASCII")
  DO kk = 1, ll
    DO jj = 1, nn
      DO ii = 1, mm - 1
        ans = ans//str(n=x(ii, jj, kk))//' '
      END DO
      ans = ans//' '//str(n=x(mm, jj, kk))
    END DO
  END DO
  !>
CASE ("BINARY")
  nn = nn * mm * ll
  xp = TRANSFER([INT(nn * BYInt32, INT32), RESHAPE(x, [nn])], xp)
  CALL B64_ENCODE(n=xp, code=ans)
END SELECT
END PROCEDURE encode_rank3_Int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank3_Int16
INTEGER(I4B) :: ii, jj, kk, nn, mm, ll
INTEGER(INT8), ALLOCATABLE :: xp(:)
!> main
ans = ''; mm = SIZE(x, 1); nn = SIZE(x, 2); ll = SIZE(x, 3)
SELECT CASE (TRIM(fmt))
CASE ("ASCII")
  DO kk = 1, ll
    DO jj = 1, nn
      DO ii = 1, mm - 1
        ans = ans//str(n=x(ii, jj, kk))//' '
      END DO
      ans = ans//' '//str(n=x(mm, jj, kk))
    END DO
  END DO
  !>
CASE ("BINARY")
  nn = nn * mm * ll
  CALL PACK_DATA( &
    & a1=[INT(nn * BYInt16, I4B)], &
    & a2=RESHAPE(x, [nn]), packed=xp)
  CALL B64_ENCODE(n=xp, code=ans)
END SELECT
END PROCEDURE encode_rank3_Int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank3_Int8
INTEGER(I4B) :: ii, jj, kk, nn, mm, ll
INTEGER(INT8), ALLOCATABLE :: xp(:)
!> main
ans = ''; mm = SIZE(x, 1); nn = SIZE(x, 2); ll = SIZE(x, 3)
SELECT CASE (TRIM(fmt))
CASE ("ASCII")
  DO kk = 1, ll
    DO jj = 1, nn
      DO ii = 1, mm - 1
        ans = ans//str(n=x(ii, jj, kk))//' '
      END DO
      ans = ans//' '//str(n=x(mm, jj, kk))
    END DO
  END DO
  !>
CASE ("BINARY")
  nn = nn * mm * ll
  CALL PACK_DATA( &
    & a1=[INT(nn * BYInt8, I4B)], &
    & a2=RESHAPE(x, [nn]), packed=xp)
  CALL B64_ENCODE(n=xp, code=ans)
END SELECT
END PROCEDURE encode_rank3_Int8

END SUBMODULE Rank3Methods

