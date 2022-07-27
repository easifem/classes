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

SUBMODULE(Polynomial1D_Class) OperatorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff1( : )
  REAL( DFP ), ALLOCATABLE :: coeff2( : )
  INTEGER( I4B ), ALLOCATABLE :: degree1( : )
  INTEGER( I4B ), ALLOCATABLE :: degree2( : )
  !!
  coeff1 = obj1%getCoeff()
  coeff2 = obj2%getCoeff()
  CALL APPEND( coeff1, coeff2 )
  degree1 = obj1%getDegree()
  degree2 = obj2%getDegree()
  CALL APPEND( degree1, degree2 )
  !!
  ans = Polynomial1D(coeff=coeff1, degree=degree1, &
    & varname=obj1%varname%chars())
  !!
  IF( ALLOCATED( coeff1 ) ) DEALLOCATE( coeff1 )
  IF( ALLOCATED( coeff2 ) ) DEALLOCATE( coeff2 )
  IF( ALLOCATED( degree1 ) ) DEALLOCATE( degree1 )
  IF( ALLOCATED( degree2 ) ) DEALLOCATE( degree2 )
  !!
END PROCEDURE func_Add_obj_obj

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff1( : )
  REAL( DFP ), ALLOCATABLE :: coeff2( : )
  INTEGER( I4B ), ALLOCATABLE :: degree1( : )
  INTEGER( I4B ), ALLOCATABLE :: degree2( : )
  !!
  coeff1 = obj1%getCoeff()
  coeff2 = obj2%getCoeff()
  degree1 = obj1%getDegree()
  degree2 = obj2%getDegree()
  CALL APPEND( coeff1, -coeff2 )
  CALL APPEND( degree1, degree2 )
  !!
  ans = Polynomial1D(coeff=coeff1, degree=degree1, &
    & varname=obj1%varname%chars())
  !!
  IF( ALLOCATED( coeff1 ) ) DEALLOCATE( coeff1 )
  IF( ALLOCATED( coeff2 ) ) DEALLOCATE( coeff2 )
  IF( ALLOCATED( degree1 ) ) DEALLOCATE( degree1 )
  IF( ALLOCATED( degree2 ) ) DEALLOCATE( degree2 )
  !!
END PROCEDURE func_Subtract_obj_obj

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff1( : )
  REAL( DFP ), ALLOCATABLE :: coeff2( : )
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree1( : )
  INTEGER( I4B ), ALLOCATABLE :: degree2( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( : )
  INTEGER( I4B ) :: ii, jj, kk
  !!
  coeff1 = obj1%getCoeff()
  coeff2 = obj2%getCoeff()
  degree1 = obj1%getDegree()
  degree2 = obj2%getDegree()
  !!
  CALL REALLOCATE( coeff, SIZE( coeff1 )+SIZE( coeff2 ) )
  CALL REALLOCATE( degree, SIZE( degree1 )+SIZE( degree2 ) )
  !!
  kk = 0
  DO ii = 1, SIZE( degree1 )
    DO jj = 1, SIZE( degree2 )
      kk = kk + 1
      degree( kk ) = degree1( ii ) + degree2( jj )
      coeff( kk ) = coeff1( ii ) * coeff2( jj )
    END DO
  END DO
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, &
    & varname=obj1%varname%chars())
  !!
  IF( ALLOCATED( coeff1 ) ) DEALLOCATE( coeff1 )
  IF( ALLOCATED( coeff2 ) ) DEALLOCATE( coeff2 )
  IF( ALLOCATED( degree1 ) ) DEALLOCATE( degree1 )
  IF( ALLOCATED( degree2 ) ) DEALLOCATE( degree2 )
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
END PROCEDURE func_Multiplication_obj_obj

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_Int8
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int8
MODULE PROCEDURE func_Add_obj_Int16
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int16
MODULE PROCEDURE func_Add_obj_Int32
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int32
MODULE PROCEDURE func_Add_obj_Int64
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int64
MODULE PROCEDURE func_Add_obj_Real32
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Real32
MODULE PROCEDURE func_Add_obj_Real64
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_Int8_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int8_obj
MODULE PROCEDURE func_Add_Int16_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int16_obj
MODULE PROCEDURE func_Add_Int32_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int32_obj
MODULE PROCEDURE func_Add_Int64_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int64_obj
MODULE PROCEDURE func_Add_Real32_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real32_obj
MODULE PROCEDURE func_Add_Real64_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real64_obj

!----------------------------------------------------------------------------
!                                                                 Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_Int8
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int8
MODULE PROCEDURE func_Subtract_obj_Int16
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int16
MODULE PROCEDURE func_Subtract_obj_Int32
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int32
MODULE PROCEDURE func_Subtract_obj_Int64
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int64
MODULE PROCEDURE func_Subtract_obj_Real32
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Real32
MODULE PROCEDURE func_Subtract_obj_Real64
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_Int8_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int8_obj
MODULE PROCEDURE func_Subtract_Int16_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int16_obj
MODULE PROCEDURE func_Subtract_Int32_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int32_obj
MODULE PROCEDURE func_Subtract_Int64_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int64_obj
MODULE PROCEDURE func_Subtract_Real32_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real32_obj
MODULE PROCEDURE func_Subtract_Real64_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real64_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_Int8
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int8
MODULE PROCEDURE func_Multiplication_obj_Int16
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int16
MODULE PROCEDURE func_Multiplication_obj_Int32
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int32
MODULE PROCEDURE func_Multiplication_obj_Int64
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int64
MODULE PROCEDURE func_Multiplication_obj_Real32
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real32
MODULE PROCEDURE func_Multiplication_obj_Real64
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_Int8_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int8_obj
MODULE PROCEDURE func_Multiplication_Int16_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int16_obj
MODULE PROCEDURE func_Multiplication_Int32_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int32_obj
MODULE PROCEDURE func_Multiplication_Int64_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int64_obj
MODULE PROCEDURE func_Multiplication_Real32_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real32_obj
MODULE PROCEDURE func_Multiplication_Real64_obj
#include "./inc/Polynomial1D/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real64_obj

END SUBMODULE OperatorMethods