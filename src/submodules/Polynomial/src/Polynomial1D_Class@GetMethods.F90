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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBMODULE(Polynomial1D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Eval
  INTEGER( I4B ) :: ii
  !!
  ans = 0.0_DFP
  !!
  IF( ALLOCATED( obj%x ) ) THEN
    DO ii = 1, SIZE( obj%x )
      ans = ans + (obj%x( ii ) .EVAL. x)
    END DO
  END IF
END PROCEDURE func_Eval

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  INTEGER( I4B ) :: ii
  !!
  ans = 0.0_DFP
  !!
  IF( ALLOCATED( obj%x ) ) THEN
    DO ii = 1, SIZE( obj%x )
      ans = ans + (obj%x( ii ) .GRAD. x)
    END DO
  END IF
END PROCEDURE func_EvalGradient

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Grad
  INTEGER( I4B ) :: ii
  INTEGER( I4B ), ALLOCATABLE :: degree( : )
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  !!
  degree = obj%getDegree( )
  coeff = obj%getCoeff( )
  !!
  coeff = coeff * degree
  !!
  DO ii = 1, SIZE( degree )
    degree( ii ) = MAX( 0_I4B, degree( ii ) - 1_I4B )
  END DO
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, varname=obj%varname%chars())
  !!
  IF(ALLOCATED(degree) ) DEALLOCATE( degree )
  IF(ALLOCATED(coeff) ) DEALLOCATE( coeff )
END PROCEDURE func_Grad

!----------------------------------------------------------------------------
!                                                            GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetStringForUID
  INTEGER( I4B ) :: ii
  ans = ""
  IF( ALLOCATED( obj%x ) ) THEN
    DO ii = 1, SIZE( obj%x )
      ans = TRIM(ans) // TRIM(obj%x(ii)%GetStringForUID())
    END DO
  END IF
END PROCEDURE func_GetStringForUID

!----------------------------------------------------------------------------
!                                                                 GetDegree
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDegree
  INTEGER( I4B ) :: ii, tsize
  !!
  IF( ALLOCATED( obj%x ) ) THEN
    !!
    tsize = SIZE( obj%x )
    !!
    CALL Reallocate(ans, tsize)
    !!
    DO ii = 1, tsize
      ans( ii ) = obj%x(ii)%GetDegree()
    END DO
    !!
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE func_GetDegree

!----------------------------------------------------------------------------
!                                                          GetDisplayString
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDisplayString
  INTEGER( I4B ) :: ii
  !!
  ans=""
  !!
  IF( ALLOCATED( obj%x ) ) THEN
    !!
    DO ii = 1, SIZE( obj%x )
      ans = TRIM( ans ) // TRIM( obj%x(ii)%GetDisplayString() )
    END DO
    !!
  END IF
  !!
END PROCEDURE func_GetDisplayString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetCoeff
  INTEGER( I4B ) :: ii, tsize
  !!
  IF( ALLOCATED( obj%x ) ) THEN
    !!
    tsize = SIZE( obj%x )
    ALLOCATE( ans( tsize ) )
    DO ii = 1, tsize
      ans( ii ) = obj%x(ii)%GetCoeff( )
    END DO
    !!
  ELSE
    !!
    ALLOCATE( ans( 0 ) )
    !!
  END IF
  !!
END PROCEDURE func_GetCoeff

END SUBMODULE GetMethods