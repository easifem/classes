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

SUBMODULE(Polynomial_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Eval
  ans = func_eval_internally( coeff=obj%coeff, power=obj%power, x = x )
END PROCEDURE func_Eval

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

PURE FUNCTION func_eval_internally( coeff, power, x ) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: coeff(:)
  INTEGER( I4B ), INTENT( IN ) :: power(:,:)
  REAL( DFP ), INTENT( IN ) ::  x( : )
  REAL( DFP ) :: ans
  !!
  INTEGER :: i, j, N, M
  REAL( DFP ) :: val
  !!
  ans = 0.0
  DO i = 1, SIZE(coeff )
    val =coeff( i )
    DO j = 1, SIZE( x )
      val = val * x( j )**(power( i, j ) )
    END DO
    ans = ans + val
  END DO
  !!
END FUNCTION func_eval_internally

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  ! Define internal values
  INTEGER( I4B ) :: I, J
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: power( :, : )
  !!
  DO I = 1, SIZE( obj%power, 2 )
    coeff = obj%coeff*obj%power( :, I )
    power = obj%power
    !!
    DO J = 1, SIZE( obj%power, 1 )
      power( J, I ) = MAX( power( J, I ) - 1, 0 )
    END DO
    !!
    ans( I ) = func_eval_internally( coeff=coeff, power=power, x = x )
    !!
  END DO
  !!
  DEALLOCATE( coeff, power )
  !!
END PROCEDURE func_EvalGradient

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Grad
  ! Define internal values
  INTEGER( I4B ) :: J
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: power( :, : )
  !!
  CALL Reallocate( power, SIZE( obj%power, 1 ), 1 )
  coeff = obj%coeff*obj%power( :, dim )
  power( :, 1 ) = obj%power( :, dim )
  !!
  DO J = 1, SIZE( obj%power, 1 )
    power( J, 1 ) = MAX( obj%power( J, dim ) - 1, 0 )
  END DO
  !!
  ALLOCATE( ans )
  ans%coeff = coeff
  ans%power = power
  !!
  DEALLOCATE( coeff, power )
  !!
END PROCEDURE func_Grad

END SUBMODULE GetMethods