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

SUBMODULE(Polynomial1D_Class) SubtractMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_mono_mono
  !!
  REAL( DFP ) :: coeff( 2 )
  INTEGER( I4B ) :: degree( 2 )
  !!
  coeff = [1.0_DFP, -1.0_DFP]
  degree = [obj1%getDegree(), obj2%getDegree()]
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, &
    & varname=obj1%varname%chars())
  !!
END PROCEDURE func_Subtract_mono_mono

!----------------------------------------------------------------------------
!                                                                   Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_mono_Int8
#include "./inc/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_mono_Int8
!!
MODULE PROCEDURE func_Subtract_mono_Int16
#include "./inc/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_mono_Int16
!!
MODULE PROCEDURE func_Subtract_mono_Int32
#include "./inc/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_mono_Int32
!!
MODULE PROCEDURE func_Subtract_mono_Int64
#include "./inc/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_mono_Int64
!!
MODULE PROCEDURE func_Subtract_mono_Real32
#include "./inc/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_mono_Real32
!!
MODULE PROCEDURE func_Subtract_mono_Real64
#include "./inc/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_mono_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_Int8_mono
#include "./inc/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int8_mono
!!
MODULE PROCEDURE func_Subtract_Int16_mono
#include "./inc/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int16_mono
!!
MODULE PROCEDURE func_Subtract_Int32_mono
#include "./inc/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int32_mono
!!
MODULE PROCEDURE func_Subtract_Int64_mono
#include "./inc/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int64_mono
!!
MODULE PROCEDURE func_Subtract_Real32_mono
#include "./inc/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real32_mono
!!
MODULE PROCEDURE func_Subtract_Real64_mono
#include "./inc/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real64_mono

!----------------------------------------------------------------------------
!                                                                   Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_mono_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( : )
  !!
  coeff = -obj2%coeff
  degree= obj2%degree
  CALL APPEND( coeff, 1.0_DFP )
  CALL APPEND( degree, obj1%getDegree() )
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, &
    & varname=obj1%varname%chars())
  !!
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
END PROCEDURE func_Subtract_mono_obj

!----------------------------------------------------------------------------
!                                                                   Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_mono
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( : )
  !!
  coeff = obj1%coeff
  degree= obj1%degree
  CALL APPEND( coeff, -1.0_DFP )
  CALL APPEND( degree, obj2%getDegree() )
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, &
    & varname=obj2%varname%chars())
  !!
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
END PROCEDURE func_Subtract_obj_mono

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( : )
  !!
  coeff = obj1%coeff
  degree= obj1%degree
  CALL APPEND( coeff, -obj2%coeff )
  CALL APPEND( degree, obj2%degree )
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, &
    & varname=obj1%x(1)%varname%chars())
  !!
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, &
    & varname=obj1%x(1)%varname%chars())
  !!
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
END PROCEDURE func_Subtract_obj_obj

!----------------------------------------------------------------------------
!                                                                 Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_Int8
#include "./inc/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int8
!!
MODULE PROCEDURE func_Subtract_obj_Int16
#include "./inc/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int16
!!0
MODULE PROCEDURE func_Subtract_obj_Int32
#include "./inc/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int32
!!
MODULE PROCEDURE func_Subtract_obj_Int64
#include "./inc/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int64
!!
MODULE PROCEDURE func_Subtract_obj_Real32
#include "./inc/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Real32
!!
MODULE PROCEDURE func_Subtract_obj_Real64
#include "./inc/Polynomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_Int8_obj
#include "./inc/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int8_obj
!!
MODULE PROCEDURE func_Subtract_Int16_obj
#include "./inc/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int16_obj
!!
MODULE PROCEDURE func_Subtract_Int32_obj
#include "./inc/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int32_obj
!!
MODULE PROCEDURE func_Subtract_Int64_obj
#include "./inc/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int64_obj
!!
MODULE PROCEDURE func_Subtract_Real32_obj
#include "./inc/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real32_obj
!!
MODULE PROCEDURE func_Subtract_Real64_obj
#include "./inc/Polynomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real64_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SubtractMethods