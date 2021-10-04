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

SUBMODULE(VTKDataArrayEncoder ) Rank1Methods
USE PENF
USE BeFoR64
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank1_Real32
  INTEGER( I4B ) :: ii, nn
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''
  nn = SIZE( x )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO ii = 1, nn
    ans = ans // str(n=x(ii))
  END DO
  CASE( "BINARY" )
    CALL PACK_DATA( &
      & a1=[INT(nn*BYReal32, I4B)], &
      & a2=x, packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank1_Real32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank1_Real64
  INTEGER( I4B ) :: ii, nn
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''
  nn = SIZE( x )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO ii = 1, nn
    ans = ans // str(n=x(ii))
  END DO
  CASE( "BINARY" )
    CALL PACK_DATA( &
      & a1=[INT(nn*BYReal64, I4B)], &
      & a2=x, packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank1_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank1_Int8
  INTEGER( I4B ) :: ii, nn
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''
  nn = SIZE( x )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO ii = 1, nn
    ans = ans // str(n=x(ii))
  END DO
  CASE( "BINARY" )
    CALL PACK_DATA( &
      & a1=[INT(nn*BYInt8, I4B)], &
      & a2=x, packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank1_Int8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank1_Int16
  INTEGER( I4B ) :: ii, nn
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''
  nn = SIZE( x )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO ii = 1, nn
    ans = ans // str(n=x(ii))
  END DO
  CASE( "BINARY" )
    CALL PACK_DATA( &
      & a1=[INT(nn*BYInt16, I4B)], &
      & a2=x, packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank1_Int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE encode_rank1_Int32
  INTEGER( I4B ) :: ii, nn
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''
  nn = SIZE( x )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO ii = 1, nn
    ans = ans // str(n=x(ii))
  END DO
  CASE( "BINARY" )
    xp = TRANSFER( &
      & [ INT( nn*BYInt32, I4B ), x], &
      & xp )
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank1_Int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE encode_rank1_Int64
  INTEGER( I4B ) :: ii, nn
  INTEGER( Int8 ), ALLOCATABLE :: xp( : )
  !> main
  ans = ''
  nn = SIZE( x )
  SELECT CASE( TRIM(fmt) )
  CASE( "ASCII" )
  DO ii = 1, nn
    ans = ans // str(n=x(ii))
  END DO
  CASE( "BINARY" )
    CALL PACK_DATA( &
      & a1=[INT(nn*BYInt64, I4B)], &
      & a2=x, packed=xp)
    CALL B64_ENCODE( n=xp, code=ans )
  END SELECT
END PROCEDURE encode_rank1_Int64
#endif

END SUBMODULE Rank1Methods