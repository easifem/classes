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

SUBMODULE(VTKDataArrayEncoder) Rank4Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank4_Real32
  INTEGER( I4B ) :: i, j, k, l, ii, jj, kk, ll
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 );
  kk = SIZE( x, 3 ); ll = SIZE( x, 4 )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO l=1, ll
    DO k=1, kk
      DO j=1, jj
        DO i=1, ii-1
          ans = ans // str( n=x(i, j, k, l) ) // ' '
        END DO
        ans = ans // ' ' // str( n=x(ii, j, k, l) )
      END DO
    END DO
  END DO
  !>
  CASE( "BINARY" )
    jj = jj * ii * kk * ll
    CALL PACK_DATA( &
      & a1=[INT(jj*BYReal32, I4B)], &
      & a2=RESHAPE( x, [jj] ), packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank4_Real32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank4_Real64
  INTEGER( I4B ) :: i, j, k, l, ii, jj, kk, ll
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 );
  kk = SIZE( x, 3 ); ll = SIZE( x, 4 )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO l=1, ll
    DO k=1, kk
      DO j=1, jj
        DO i=1, ii-1
          ans = ans // str( n=x(i, j, k, l) ) // ' '
        END DO
        ans = ans // ' ' // str( n=x(ii, j, k, l) )
      END DO
    END DO
  END DO
  !>
  CASE( "BINARY" )
    jj = jj * ii * kk * ll
    CALL PACK_DATA( &
      & a1=[INT(jj*BYReal64, I4B)], &
      & a2=RESHAPE( x, [jj] ), packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank4_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE encode_rank4_Int64
  INTEGER( I4B ) :: i, j, k, l, ii, jj, kk, ll
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 );
  kk = SIZE( x, 3 ); ll = SIZE( x, 4 )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO l=1, ll
    DO k=1, kk
      DO j=1, jj
        DO i=1, ii-1
          ans = ans // str( n=x(i, j, k, l) ) // ' '
        END DO
        ans = ans // ' ' // str( n=x(ii, j, k, l) )
      END DO
    END DO
  END DO
  !>
  CASE( "BINARY" )
    jj = jj * ii * kk * ll
    CALL PACK_DATA( &
      & a1=[INT(jj*BYInt64, I4B)], &
      & a2=RESHAPE( x, [jj] ), packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank4_Int64
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank4_Int32
  INTEGER( I4B ) :: i, j, k, l, ii, jj, kk, ll
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 );
  kk = SIZE( x, 3 ); ll = SIZE( x, 4 )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO l=1, ll
    DO k=1, kk
      DO j=1, jj
        DO i=1, ii-1
          ans = ans // str( n=x(i, j, k, l) ) // ' '
        END DO
        ans = ans // ' ' // str( n=x(ii, j, k, l) )
      END DO
    END DO
  END DO
  !>
  CASE( "BINARY" )
    jj = jj * ii * kk * ll
    xp = TRANSFER([INT(jj*BYInt32, Int32), reshape(x, [jj])], xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank4_Int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank4_Int16
  INTEGER( I4B ) :: i, j, k, l, ii, jj, kk, ll
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 );
  kk = SIZE( x, 3 ); ll = SIZE( x, 4 )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO l=1, ll
    DO k=1, kk
      DO j=1, jj
        DO i=1, ii-1
          ans = ans // str( n=x(i, j, k, l) ) // ' '
        END DO
        ans = ans // ' ' // str( n=x(ii, j, k, l) )
      END DO
    END DO
  END DO
  !>
  CASE( "BINARY" )
    jj = jj * ii * kk * ll
    CALL PACK_DATA( &
      & a1=[INT(jj*BYInt16, I4B)], &
      & a2=RESHAPE( x, [jj] ), packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank4_Int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank4_Int8
  INTEGER( I4B ) :: i, j, k, l, ii, jj, kk, ll
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''; ii = SIZE( x, 1 ); jj = SIZE( x, 2 );
  kk = SIZE( x, 3 ); ll = SIZE( x, 4 )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO l=1, ll
    DO k=1, kk
      DO j=1, jj
        DO i=1, ii-1
          ans = ans // str( n=x(i, j, k, l) ) // ' '
        END DO
        ans = ans // ' ' // str( n=x(ii, j, k, l) )
      END DO
    END DO
  END DO
  !>
  CASE( "BINARY" )
    jj = jj * ii * kk * ll
    CALL PACK_DATA( &
      & a1=[INT(jj*BYInt8, I4B)], &
      & a2=RESHAPE( x, [jj] ), packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank4_Int8

END SUBMODULE Rank4Methods