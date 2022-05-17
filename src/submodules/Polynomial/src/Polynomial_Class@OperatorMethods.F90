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

SUBMODULE(Polynomial_Class) OperatorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add
  ans%coeff = obj1%coeff
  CALL APPEND( ans%coeff, obj2%coeff )
  ans%power = rowConcat( obj1%power, obj2%power )
END PROCEDURE func_Add

!----------------------------------------------------------------------------
!                                                                  Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract
  ans%coeff = obj1%coeff
  CALL APPEND( ans%coeff, -obj2%coeff )
  ans%power = rowConcat( obj1%power, obj2%power )
END PROCEDURE func_Subtract

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication
  INTEGER( I4B ) :: m, n, ii, jj, kk
  !!
  m = SIZE( obj1%coeff )
  n = SIZE( obj2%coeff )
  CALL Reallocate( ans%coeff, m*n )
  CALL Reallocate( ans%power, m*n, SIZE( obj1%power, 2 ) )
  kk = 0
  !!
  DO ii = 1, m
    DO jj = 1, n
      kk = kk + 1
      ans%coeff( kk ) = obj1%coeff( ii ) * obj2%coeff( jj )
      ans%power( kk, : ) = obj1%power( ii, : ) + obj2%power( jj, : )
    END DO
  END DO
  !!
END PROCEDURE func_Multiplication

!----------------------------------------------------------------------------
!                                                                 Clean
!----------------------------------------------------------------------------

PURE SUBROUTINE CompactPolynomial( coeff, power, coeff2, power2 )
  REAL( DFP ), INTENT( IN ) :: coeff( : )
  INTEGER( I4B ), INTENT( IN ) :: power( :, : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: coeff2( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: power2( :, : )
  ! !!
  ! INTEGER( I4B ) :: ii, m, jj
  ! REAL( DFP ), ALLOCATABLE :: map( :, : )
  ! LOGICAL( LGT ), ALLOCATABLE :: match( : )
  ! !!
  ! m = SIZE( coeff )
  ! ALLOCATE( match( m ) )
  ! DO ii = 1, m
  !   match = .FALSE.
  !   match( ii ) = .TRUE.
  !   DO jj = ii+1, m
  !     IF( ALL( power( ii, : ) .EQ. power( jj, : ) ) ) THEN
  !       match( jj ) = .TRUE.
  !     END IF
  !   END DO
  ! END DO
  !!
END SUBROUTINE CompactPolynomial

END SUBMODULE OperatorMethods