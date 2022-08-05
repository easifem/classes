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

SUBMODULE(Polynomial1D_Class) MultiplicationMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_mono_mono
  !!
  REAL( DFP ) :: coeff( 1 )
  INTEGER( I4B ) :: degree( 1 )
  !!
  coeff = 1.0_DFP
  degree = obj1%getdegree()+obj2%getdegree()
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, &
    & varname=obj1%varname%chars())
  !!
END PROCEDURE func_Multiplication_mono_mono

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_mono_Int8
#include "./inc/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Int8
!!
MODULE PROCEDURE func_Multiplication_mono_Int16
#include "./inc/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Int16
!!
MODULE PROCEDURE func_Multiplication_mono_Int32
#include "./inc/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Int32
!!
MODULE PROCEDURE func_Multiplication_mono_Int64
#include "./inc/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Int64
!!
MODULE PROCEDURE func_Multiplication_mono_Real32
#include "./inc/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Real32
!!
MODULE PROCEDURE func_Multiplication_mono_Real64
#include "./inc/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_Int8_mono
#include "./inc/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int8_mono
!!
MODULE PROCEDURE func_Multiplication_Int16_mono
#include "./inc/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int16_mono
!!
MODULE PROCEDURE func_Multiplication_Int32_mono
#include "./inc/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int32_mono
!!
MODULE PROCEDURE func_Multiplication_Int64_mono
#include "./inc/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int64_mono
!!
MODULE PROCEDURE func_Multiplication_Real32_mono
#include "./inc/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real32_mono
!!
MODULE PROCEDURE func_Multiplication_Real64_mono
#include "./inc/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real64_mono

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_mono_obj
  !!
  INTEGER( I4B ), ALLOCATABLE :: degree( : )
  !!
  degree= obj2%degree + obj1%getdegree()
  !!
  ans = Polynomial1D(coeff=obj2%coeff, degree=degree, &
    & varname=obj1%varname%chars())
  !!
  DEALLOCATE( degree )
  !!
END PROCEDURE func_Multiplication_mono_obj

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_mono
  ans = func_Multiplication_mono_obj( obj1=obj2, obj2=obj1)
END PROCEDURE func_Multiplication_obj_mono

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( : )
  INTEGER( I4B ) :: ii, jj, kk
  !!
  CALL REALLOCATE( coeff, SIZE( obj1%coeff )*SIZE( obj2%coeff ) )
  CALL REALLOCATE( degree, SIZE( obj1%degree )*SIZE( obj2%degree ) )
  !!
  kk = 0
  DO ii = 1, SIZE( obj1%degree )
    DO jj = 1, SIZE( obj2%degree )
      kk = kk + 1
      degree( kk ) =obj1%degree( ii ) + obj2%degree( jj )
      coeff( kk ) = obj1%coeff( ii ) * obj2%coeff( jj )
    END DO
  END DO
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, &
    & varname=obj1%x(1)%varname%chars())
  !!
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
END PROCEDURE func_Multiplication_obj_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_Int8
#include "./inc/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int8
!!
MODULE PROCEDURE func_Multiplication_obj_Int16
#include "./inc/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int16
!!
MODULE PROCEDURE func_Multiplication_obj_Int32
#include "./inc/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int32
!!
MODULE PROCEDURE func_Multiplication_obj_Int64
#include "./inc/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int64
!!
MODULE PROCEDURE func_Multiplication_obj_Real32
#include "./inc/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real32
!!
MODULE PROCEDURE func_Multiplication_obj_Real64
#include "./inc/Polynomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_Int8_obj
#include "./inc/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int8_obj
!!
MODULE PROCEDURE func_Multiplication_Int16_obj
#include "./inc/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int16_obj
!!
MODULE PROCEDURE func_Multiplication_Int32_obj
#include "./inc/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int32_obj
!!
MODULE PROCEDURE func_Multiplication_Int64_obj
#include "./inc/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int64_obj
!!
MODULE PROCEDURE func_Multiplication_Real32_obj
#include "./inc/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real32_obj
!!
MODULE PROCEDURE func_Multiplication_Real64_obj
#include "./inc/Polynomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real64_obj

END SUBMODULE MultiplicationMethods