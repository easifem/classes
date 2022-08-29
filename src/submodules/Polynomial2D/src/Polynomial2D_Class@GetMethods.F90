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

SUBMODULE(Polynomial2D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalScalar
  REAL( DFP ), ALLOCATABLE :: avar( : )
  avar = x**(obj%degree(:,1)) * y**(obj%degree(:,2))
  ans = DOT_PRODUCT( obj%coeff, avar )
END PROCEDURE func_EvalScalar

!----------------------------------------------------------------------------
!                                                               EvalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  INTEGER( I4B ) :: ii
  ans = 0.0_DFP
  IF( ALLOCATED( obj%x ) ) THEN
    DO ii = 1, SIZE( obj%x )
      ans = ans + (obj%x( ii )%EvalGradient( x=x, y=y, dim=dim ))
    END DO
  END IF
END PROCEDURE func_EvalGradient

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Grad
  INTEGER( I4B ) :: ii
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  TYPE(String) :: varname(2)
  !!
  IF( ALLOCATED( obj%x ) ) THEN
    !!
    degree = obj%degree
    coeff = obj%coeff * obj%degree(:,dim)
    !!
    DO ii = 1, SIZE( coeff )
      degree( ii, dim ) = MAX( 0_I4B, degree( ii, dim ) - 1_I4B )
    END DO
    !!
    varname = obj%GetVarname()
    !!
    CALL ans%Initiate( &
      & coeff=coeff, &
      & degree=degree, &
      & varname1=varname(1)%chars(), &
      & varname2=varname(2)%chars() )
    !!
    DEALLOCATE( coeff, degree )
  END IF
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
  !!
  IF( ALLOCATED( obj%degree ) ) THEN
    ans = obj%degree
  ELSE
    ALLOCATE( ans( 0, 0 ) )
  END IF
END PROCEDURE func_GetDegree

!----------------------------------------------------------------------------
!                                                           GetDisplayString
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDisplayString
  INTEGER( I4B ) :: ii
  REAL( DFP ), PARAMETER :: tol=1.0E-10
  !!
  ans=""
  !!
  IF( ALLOCATED( obj%x ) ) THEN
    !!
    DO ii = 1, SIZE( obj%x )
      IF( SOFTEQ(obj%coeff( ii ), 0.0_DFP, tol) ) THEN
        CYCLE
      ELSEIF( obj%coeff( ii ) .GT. 0.0_DFP ) THEN
        ans = TRIM( ans ) // "+" // &
          & TOSTRING(obj%coeff(ii)) // &
          & TRIM( obj%x(ii)%GetDisplayString() )
      ELSE
        ans = TRIM( ans ) // &
          & TOSTRING(obj%coeff(ii)) // &
          & TRIM( obj%x(ii)%GetDisplayString() )
      END IF
    END DO
    !!
  END IF
  !!
END PROCEDURE func_GetDisplayString

!----------------------------------------------------------------------------
!                                                                   GetCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetCoeff
  IF( ALLOCATED( obj%x ) ) THEN
    ans = obj%coeff
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE func_GetCoeff

!----------------------------------------------------------------------------
!                                                                 GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetOrder
  IF( ALLOCATED( obj%degree ) ) THEN
    ans = MAXVAL( obj%degree(:,1) + obj%degree(:,2) )
  ELSE
    ans = 0_I4B
  END IF
END PROCEDURE func_GetOrder

END SUBMODULE GetMethods