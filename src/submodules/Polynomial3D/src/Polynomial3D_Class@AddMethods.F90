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

SUBMODULE(Polynomial3D_Class) AddMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  TYPE( String ) :: varname( 3 )
  !!
  varname = obj1%GetVarname( )
  !!
  coeff = obj1%coeff
  CALL APPEND( coeff, obj2%coeff )
  !!
  degree = obj1%degree .ROWCONCAT. obj2%degree
  !!
  CALL ans%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & name1=varname(1)%chars(), &
    & name3=varname(3)%chars(), &
    & name2=varname(2)%chars() )
  !!
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
END PROCEDURE func_Add_obj_obj

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_mono_mono
  !!
  REAL( DFP ), PARAMETER :: coeff( 2 ) = 1.0_DFP
  INTEGER( I4B ) :: degree( 2, 3 )
  TYPE( String ) :: varname( 3 )
  !!
  degree( 1, : ) = obj1%GetDegree()
  degree( 2, : ) = obj2%GetDegree()
  varname = obj1%GetVarname()
  !!
  CALL ans%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & name1=varname(1)%chars(), &
    & name3=varname(3)%chars(), &
    & name2=varname(2)%chars() )
  !!
END PROCEDURE func_Add_mono_mono

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_mono_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  TYPE( String ) :: varname( 3 )
  !!
  coeff = obj2%coeff
  CALL APPEND( coeff, 1.0_DFP )
  degree= obj2%degree .ROWCONCAT. obj1%GetDegree()
  varname = obj1%GetVarname()
  !!
  CALL ans%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & name1=varname(1)%chars(), &
    & name3=varname(3)%chars(), &
    & name2=varname(2)%chars() )
  !!
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
END PROCEDURE func_Add_mono_obj

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_mono
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  TYPE( String ) :: varname( 3 )
  !!
  coeff = obj1%coeff
  CALL APPEND( coeff, 1.0_DFP )
  degree= obj1%degree .ROWCONCAT. obj2%GetDegree()
  varname = obj1%GetVarname()
  !!
  CALL ans%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & name1=varname(1)%chars(), &
    & name3=varname(3)%chars(), &
    & name2=varname(2)%chars() )
  !!
  IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
  IF( ALLOCATED( degree ) ) DEALLOCATE( degree )
  !!
END PROCEDURE func_Add_obj_mono

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_mono_Int8
#include "./inc/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_mono_Int8
!!
MODULE PROCEDURE func_Add_mono_Int16
#include "./inc/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_mono_Int16
!!
MODULE PROCEDURE func_Add_mono_Int32
#include "./inc/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_mono_Int32
!!
MODULE PROCEDURE func_Add_mono_Int64
#include "./inc/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_mono_Int64
!!
MODULE PROCEDURE func_Add_mono_Real32
#include "./inc/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_mono_Real32
!!
MODULE PROCEDURE func_Add_mono_Real64
#include "./inc/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_mono_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_Int8_mono
#include "./inc/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int8_mono
!!
MODULE PROCEDURE func_Add_Int16_mono
#include "./inc/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int16_mono
!!
MODULE PROCEDURE func_Add_Int32_mono
#include "./inc/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int32_mono
!!
MODULE PROCEDURE func_Add_Int64_mono
#include "./inc/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int64_mono
!!
MODULE PROCEDURE func_Add_Real32_mono
#include "./inc/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real32_mono
!!
MODULE PROCEDURE func_Add_Real64_mono
#include "./inc/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real64_mono

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_Int8
#include "./inc/Polynomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int8
MODULE PROCEDURE func_Add_obj_Int16
#include "./inc/Polynomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int16
MODULE PROCEDURE func_Add_obj_Int32
#include "./inc/Polynomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int32
MODULE PROCEDURE func_Add_obj_Int64
#include "./inc/Polynomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int64
MODULE PROCEDURE func_Add_obj_Real32
#include "./inc/Polynomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Real32
MODULE PROCEDURE func_Add_obj_Real64
#include "./inc/Polynomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_Int8_obj
#include "./inc/Polynomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int8_obj
MODULE PROCEDURE func_Add_Int16_obj
#include "./inc/Polynomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int16_obj
MODULE PROCEDURE func_Add_Int32_obj
#include "./inc/Polynomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int32_obj
MODULE PROCEDURE func_Add_Int64_obj
#include "./inc/Polynomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int64_obj
MODULE PROCEDURE func_Add_Real32_obj
#include "./inc/Polynomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real32_obj
MODULE PROCEDURE func_Add_Real64_obj
#include "./inc/Polynomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real64_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AddMethods