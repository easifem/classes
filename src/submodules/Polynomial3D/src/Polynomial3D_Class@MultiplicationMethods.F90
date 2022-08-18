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

SUBMODULE(Polynomial3D_Class) MultiplicationMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  TYPE( String ) :: varname( 3 )
  INTEGER( I4B ) :: ii, jj, kk, n1, n2
  !!
  varname = obj1%GetVarname( )
  !!
  n1 = SIZE( obj1%coeff )
  n2 = SIZE( obj2%coeff )
  CALL REALLOCATE( coeff, n1*n2 )
  CALL REALLOCATE( degree, n1*n2, 3 )
  !!
  kk = 0
  DO ii = 1, n1
    DO jj = 1, n2
      kk = kk + 1
      degree( kk, : ) =obj1%degree( ii, : ) + obj2%degree( jj, : )
      coeff( kk ) = obj1%coeff( ii ) * obj2%coeff( jj )
    END DO
  END DO
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
END PROCEDURE func_Multiplication_obj_obj

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_mono_mono
  !!
  REAL( DFP ), PARAMETER :: coeff( 1 ) = 1.0_DFP
  INTEGER( I4B ) :: degree( 1, 3 )
  TYPE( String ) :: varname( 3 )
  !!
  degree( 1, : ) = obj1%GetDegree() + obj2%GetDegree()
  varname = obj1%GetVarname()
  !!
  CALL ans%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & name1=varname(1)%chars(), &
    & name3=varname(3)%chars(), &
    & name2=varname(2)%chars() )
  !!
END PROCEDURE func_Multiplication_mono_mono

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_mono_obj
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  INTEGER( I4B ) :: d0( 3 )
  TYPE( String ) :: varname( 3 )
  !!
  coeff = obj2%coeff
  !!
  d0=obj1%GetDegree()
  degree=obj2%GetDegree()
  degree(:,1)=degree(:,1)+d0(1)
  degree(:,2)=degree(:,2)+d0(2)
  degree(:,3)=degree(:,3)+d0(3)
  !!
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
END PROCEDURE func_Multiplication_mono_obj

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_mono
  !!
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  INTEGER( I4B ) :: d0( 3 )
  TYPE( String ) :: varname( 3 )
  !!
  coeff = obj1%coeff
  !!
  d0=obj2%GetDegree()
  degree=obj1%GetDegree()
  degree(:,1)=degree(:,1)+d0(1)
  degree(:,2)=degree(:,2)+d0(2)
  degree(:,3)=degree(:,3)+d0(3)
  !!
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
END PROCEDURE func_Multiplication_obj_mono

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_mono_Int8
#include "./inc/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Int8
!!
MODULE PROCEDURE func_Multiplication_mono_Int16
#include "./inc/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Int16
!!
MODULE PROCEDURE func_Multiplication_mono_Int32
#include "./inc/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Int32
!!
MODULE PROCEDURE func_Multiplication_mono_Int64
#include "./inc/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Int64
!!
MODULE PROCEDURE func_Multiplication_mono_Real32
#include "./inc/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Real32
!!
MODULE PROCEDURE func_Multiplication_mono_Real64
#include "./inc/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_mono_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_Int8_mono
#include "./inc/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int8_mono
!!
MODULE PROCEDURE func_Multiplication_Int16_mono
#include "./inc/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int16_mono
!!
MODULE PROCEDURE func_Multiplication_Int32_mono
#include "./inc/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int32_mono
!!
MODULE PROCEDURE func_Multiplication_Int64_mono
#include "./inc/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int64_mono
!!
MODULE PROCEDURE func_Multiplication_Real32_mono
#include "./inc/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real32_mono
!!
MODULE PROCEDURE func_Multiplication_Real64_mono
#include "./inc/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real64_mono

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_Int8
#include "./inc/Polynomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int8
MODULE PROCEDURE func_Multiplication_obj_Int16
#include "./inc/Polynomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int16
MODULE PROCEDURE func_Multiplication_obj_Int32
#include "./inc/Polynomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int32
MODULE PROCEDURE func_Multiplication_obj_Int64
#include "./inc/Polynomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int64
MODULE PROCEDURE func_Multiplication_obj_Real32
#include "./inc/Polynomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real32
MODULE PROCEDURE func_Multiplication_obj_Real64
#include "./inc/Polynomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_Int8_obj
#include "./inc/Polynomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int8_obj
MODULE PROCEDURE func_Multiplication_Int16_obj
#include "./inc/Polynomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int16_obj
MODULE PROCEDURE func_Multiplication_Int32_obj
#include "./inc/Polynomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int32_obj
MODULE PROCEDURE func_Multiplication_Int64_obj
#include "./inc/Polynomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int64_obj
MODULE PROCEDURE func_Multiplication_Real32_obj
#include "./inc/Polynomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real32_obj
MODULE PROCEDURE func_Multiplication_Real64_obj
#include "./inc/Polynomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real64_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MultiplicationMethods