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

SUBMODULE(Monomial1D_Class) OperatorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_obj
  REAL( DFP ) :: coeff
  INTEGER( I4B ) :: degree
  !!
  IF( obj1%uid .EQ. obj2%uid ) THEN
    coeff = obj1%coeff + obj2%coeff
    degree = obj1%degree
    ans=Monomial1D(coeff=coeff, degree=degree, varname=obj1%varname%chars())
  END IF
  !!
END PROCEDURE func_Add_obj_obj

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_obj
  REAL( DFP ) :: coeff
  INTEGER( I4B ) :: degree
  !!
  IF( obj1%uid .EQ. obj2%uid ) THEN
    coeff = obj1%coeff - obj2%coeff
    degree = obj1%degree
    ans=Monomial1D(coeff=coeff, degree=degree, varname=obj1%varname%chars())
  END IF
  !!
END PROCEDURE func_Subtract_obj_obj

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_obj
  REAL( DFP ) :: coeff
  INTEGER( I4B ) :: degree
  !!
  IF( obj1%varname .EQ. obj2%varname ) THEN
    coeff = obj1%coeff * obj2%coeff
    degree = obj1%degree + obj2%degree
    ans=Monomial1D(coeff=coeff, degree=degree, varname=obj1%varname%chars())
  END IF
  !!
END PROCEDURE func_Multiplication_obj_obj

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_obj_Int8
#include "./inc/Monomial1D/Monomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int8
MODULE PROCEDURE func_Add_obj_Int16
#include "./inc/Monomial1D/Monomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int16
MODULE PROCEDURE func_Add_obj_Int32
#include "./inc/Monomial1D/Monomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int32
MODULE PROCEDURE func_Add_obj_Int64
#include "./inc/Monomial1D/Monomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Int64
MODULE PROCEDURE func_Add_obj_Real32
#include "./inc/Monomial1D/Monomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Real32
MODULE PROCEDURE func_Add_obj_Real64
#include "./inc/Monomial1D/Monomial1D_Class_Add_obj_scalar.inc"
END PROCEDURE func_Add_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Add_Int8_obj
#include "./inc/Monomial1D/Monomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int8_obj
MODULE PROCEDURE func_Add_Int16_obj
#include "./inc/Monomial1D/Monomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int16_obj
MODULE PROCEDURE func_Add_Int32_obj
#include "./inc/Monomial1D/Monomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int32_obj
MODULE PROCEDURE func_Add_Int64_obj
#include "./inc/Monomial1D/Monomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Int64_obj
MODULE PROCEDURE func_Add_Real32_obj
#include "./inc/Monomial1D/Monomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real32_obj
MODULE PROCEDURE func_Add_Real64_obj
#include "./inc/Monomial1D/Monomial1D_Class_Add_scalar_obj.inc"
END PROCEDURE func_Add_Real64_obj

!----------------------------------------------------------------------------
!                                                                 Subtract
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_obj_Int8
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int8
MODULE PROCEDURE func_Subtract_obj_Int16
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int16
MODULE PROCEDURE func_Subtract_obj_Int32
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int32
MODULE PROCEDURE func_Subtract_obj_Int64
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Int64
MODULE PROCEDURE func_Subtract_obj_Real32
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Real32
MODULE PROCEDURE func_Subtract_obj_Real64
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_obj_scalar.inc"
END PROCEDURE func_Subtract_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Subtract_Int8_obj
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int8_obj
MODULE PROCEDURE func_Subtract_Int16_obj
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int16_obj
MODULE PROCEDURE func_Subtract_Int32_obj
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int32_obj
MODULE PROCEDURE func_Subtract_Int64_obj
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Int64_obj
MODULE PROCEDURE func_Subtract_Real32_obj
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real32_obj
MODULE PROCEDURE func_Subtract_Real64_obj
#include "./inc/Monomial1D/Monomial1D_Class_Subtract_scalar_obj.inc"
END PROCEDURE func_Subtract_Real64_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_Int8
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int8
MODULE PROCEDURE func_Multiplication_obj_Int16
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int16
MODULE PROCEDURE func_Multiplication_obj_Int32
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int32
MODULE PROCEDURE func_Multiplication_obj_Int64
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Int64
MODULE PROCEDURE func_Multiplication_obj_Real32
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real32
MODULE PROCEDURE func_Multiplication_obj_Real64
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_obj_scalar.inc"
END PROCEDURE func_Multiplication_obj_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_Int8_obj
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int8_obj
MODULE PROCEDURE func_Multiplication_Int16_obj
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int16_obj
MODULE PROCEDURE func_Multiplication_Int32_obj
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int32_obj
MODULE PROCEDURE func_Multiplication_Int64_obj
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Int64_obj
MODULE PROCEDURE func_Multiplication_Real32_obj
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real32_obj
MODULE PROCEDURE func_Multiplication_Real64_obj
#include "./inc/Monomial1D/Monomial1D_Class_Multiplication_scalar_obj.inc"
END PROCEDURE func_Multiplication_Real64_obj

END SUBMODULE OperatorMethods