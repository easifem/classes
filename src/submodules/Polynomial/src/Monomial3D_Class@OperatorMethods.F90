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

SUBMODULE(Monomial3D_Class) OperatorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_obj
  REAL( DFP ) :: coeff
  INTEGER( I4B ) :: degree( MAX_COMPONENTS )
  INTEGER( I4B ) :: uids1( MAX_COMPONENTS )
  INTEGER( I4B ) :: uids2( MAX_COMPONENTS )
  CHARACTER( LEN = 256 ) :: varname(MAX_COMPONENTS)
  !!
  uids1(1) = obj1%x(1)%uid
  uids1(2) = obj1%x(2)%uid
  uids1(3) = obj1%x(3)%uid
  !!
  uids2(1) = obj2%x(1)%uid
  uids2(2) = obj2%x(2)%uid
  uids2(3) = obj2%x(3)%uid
  !!
  IF( ALL( uids1 .EQ. uids2 ) ) THEN
    coeff = obj1%getCoeff() + obj2%getCoeff()
    degree(1) = obj1%x(1)%getDegree()
    degree(2) = obj1%x(2)%getDegree()
    degree(3) = obj1%x(3)%getDegree()
    varname(1) = obj1%x(1)%varname%chars()
    varname(2) = obj1%x(2)%varname%chars()
    varname(3) = obj1%x(3)%varname%chars()
    ans=Monomial3D(coeff=coeff, degree=degree, varname=varname)
  END IF
  !!
END PROCEDURE func_Add_obj_obj

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_obj
  REAL( DFP ) :: coeff
  INTEGER( I4B ) :: degree( MAX_COMPONENTS )
  INTEGER( I4B ) :: uids1( MAX_COMPONENTS )
  INTEGER( I4B ) :: uids2( MAX_COMPONENTS )
  CHARACTER( LEN = 256 ) :: varname(MAX_COMPONENTS)
  !!
  uids1(1) = obj1%x(1)%uid
  uids1(2) = obj1%x(2)%uid
  uids1(3) = obj1%x(3)%uid
  !!
  uids2(1) = obj2%x(1)%uid
  uids2(2) = obj2%x(2)%uid
  uids2(3) = obj2%x(3)%uid
  !!
  IF( ALL( uids1 .EQ. uids2 ) ) THEN
    coeff = obj1%getCoeff() - obj2%getCoeff()
    degree(1) = obj1%x(1)%getDegree()
    degree(2) = obj1%x(2)%getDegree()
    degree(3) = obj1%x(3)%getDegree()
    varname(1) = obj1%x(1)%varname%chars()
    varname(2) = obj1%x(2)%varname%chars()
    varname(3) = obj1%x(3)%varname%chars()
    ans=Monomial3D(coeff=coeff, degree=degree, varname=varname)
  END IF
  !!
END PROCEDURE func_Subtract_obj_obj

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_obj
  REAL( DFP ) :: coeff
  INTEGER( I4B ) :: degree(MAX_COMPONENTS)
  CHARACTER( LEN = 256 ) :: varname(MAX_COMPONENTS)
  TYPE(String) :: varname1( MAX_COMPONENTS ), varname2( MAX_COMPONENTS )
  !!
  varname1( 1 ) = obj1%x(1)%varname
  varname1( 2 ) = obj1%x(2)%varname
  varname1( 3 ) = obj1%x(3)%varname
  !!
  varname2( 1 ) = obj2%x(1)%varname
  varname2( 2 ) = obj2%x(2)%varname
  varname2( 3 ) = obj2%x(3)%varname
  !!
  IF( ALL( varname1 .EQ. varname2 ) ) THEN
    coeff = obj1%getCoeff() * obj2%getCoeff()
    !!
    degree(1) = obj1%x(1)%getDegree() + obj2%x(1)%getDegree()
    degree(2) = obj1%x(2)%getDegree() + obj2%x(2)%getDegree()
    degree(3) = obj1%x(3)%getDegree() + obj2%x(3)%getDegree()
    !!
    varname(1) = varname1(1)%chars()
    varname(2) = varname1(2)%chars()
    varname(3) = varname1(3)%chars()
    !!
    ans=Monomial3D(coeff=coeff, degree=degree, varname=varname)
  END IF
  !!
END PROCEDURE func_Multiplication_obj_obj

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_Int8
#include "./inc/Monomial3D/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int8
MODULE PROCEDURE func_Add_obj_Int16
#include "./inc/Monomial3D/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int16
MODULE PROCEDURE func_Add_obj_Int32
#include "./inc/Monomial3D/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int32
MODULE PROCEDURE func_Add_obj_Int64
#include "./inc/Monomial3D/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int64
MODULE PROCEDURE func_Add_obj_Real32
#include "./inc/Monomial3D/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Real32
MODULE PROCEDURE func_Add_obj_Real64
#include "./inc/Monomial3D/Monomial3D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_Int8_obj
#include "./inc/Monomial3D/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int8_obj
MODULE PROCEDURE func_Add_Int16_obj
#include "./inc/Monomial3D/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int16_obj
MODULE PROCEDURE func_Add_Int32_obj
#include "./inc/Monomial3D/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int32_obj
MODULE PROCEDURE func_Add_Int64_obj
#include "./inc/Monomial3D/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int64_obj
MODULE PROCEDURE func_Add_Real32_obj
#include "./inc/Monomial3D/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real32_obj
MODULE PROCEDURE func_Add_Real64_obj
#include "./inc/Monomial3D/Monomial3D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real64_obj

!----------------------------------------------------------------------------
!                                                                 Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_Int8
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int8
MODULE PROCEDURE func_Subtract_obj_Int16
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int16
MODULE PROCEDURE func_Subtract_obj_Int32
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int32
MODULE PROCEDURE func_Subtract_obj_Int64
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int64
MODULE PROCEDURE func_Subtract_obj_Real32
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Real32
MODULE PROCEDURE func_Subtract_obj_Real64
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_Int8_obj
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int8_obj
MODULE PROCEDURE func_Subtract_Int16_obj
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int16_obj
MODULE PROCEDURE func_Subtract_Int32_obj
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int32_obj
MODULE PROCEDURE func_Subtract_Int64_obj
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int64_obj
MODULE PROCEDURE func_Subtract_Real32_obj
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real32_obj
MODULE PROCEDURE func_Subtract_Real64_obj
#include "./inc/Monomial3D/Monomial3D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real64_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_Int8
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int8
MODULE PROCEDURE func_Multiplication_obj_Int16
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int16
MODULE PROCEDURE func_Multiplication_obj_Int32
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int32
MODULE PROCEDURE func_Multiplication_obj_Int64
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int64
MODULE PROCEDURE func_Multiplication_obj_Real32
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real32
MODULE PROCEDURE func_Multiplication_obj_Real64
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_Int8_obj
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int8_obj
MODULE PROCEDURE func_Multiplication_Int16_obj
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int16_obj
MODULE PROCEDURE func_Multiplication_Int32_obj
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int32_obj
MODULE PROCEDURE func_Multiplication_Int64_obj
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int64_obj
MODULE PROCEDURE func_Multiplication_Real32_obj
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real32_obj
MODULE PROCEDURE func_Multiplication_Real64_obj
#include "./inc/Monomial3D/Monomial3D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real64_obj

END SUBMODULE OperatorMethods