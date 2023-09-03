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

MODULE Polynomial2D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractFunction_Class
USE Monomial2D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                              Polynomial2D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 July 2022
! summary: Polynomial2D class is defined
!

TYPE, EXTENDS(AbstractFunction2D_) :: Polynomial2D_
  PRIVATE
  INTEGER(I4B), ALLOCATABLE :: degree(:, :)
    !! power of each monomial
  REAL(DFP), ALLOCATABLE :: coeff(:)
    !! coefficient
  TYPE(Monomial2D_), ALLOCATABLE :: x(:)
    !! Monomial2D
CONTAINS
    !!
    !! @ConstructorMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => func_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => func_Deallocate
  FINAL :: func_Final
    !!
    !! @GetMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: EvalScalar => func_EvalScalar
  PROCEDURE, PUBLIC, PASS(obj) :: EvalGradient => func_EvalGradient
  PROCEDURE, PUBLIC, PASS(obj) :: Grad => func_Grad
  PROCEDURE, PUBLIC, PASS(obj) :: GetStringForUID => func_GetStringForUID
  PROCEDURE, PUBLIC, PASS(obj) :: GetDegree => func_GetDegree
  PROCEDURE, PUBLIC, PASS(obj) :: GetDisplayString => &
    & func_GetDisplayString
  PROCEDURE, PUBLIC, PASS(obj) :: GetCoeff => &
    & func_GetCoeff
  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder => &
    & func_GetOrder
    !!
  GENERIC, PUBLIC :: OPERATOR(.Grad.) => Grad
    !!
    !! @IOMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: Display => func_Display
    !!
    !! @OperatorMethods
    !!
    !! OPERATOR(+)
    !!
  PROCEDURE, PUBLIC, PASS(obj1) :: AddObjObj => func_Add_obj_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: AddMonoObj => &
    & func_Add_mono_obj
  PROCEDURE, PUBLIC, PASS(obj1) :: AddObjMono => &
    & func_Add_obj_mono
  PROCEDURE, PUBLIC, PASS(obj1) :: AddObjInt8 => func_Add_obj_Int8
  PROCEDURE, PUBLIC, PASS(obj1) :: AddObjInt16 => func_Add_obj_Int16
  PROCEDURE, PUBLIC, PASS(obj1) :: AddObjInt32 => func_Add_obj_Int32
  PROCEDURE, PUBLIC, PASS(obj1) :: AddObjInt64 => func_Add_obj_Int64
  PROCEDURE, PUBLIC, PASS(obj1) :: AddObjReal32 => func_Add_obj_Real32
  PROCEDURE, PUBLIC, PASS(obj1) :: AddObjReal64 => func_Add_obj_Real64
  PROCEDURE, PUBLIC, PASS(obj2) :: AddInt8Obj => func_Add_Int8_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: AddInt16Obj => func_Add_Int16_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: AddInt32Obj => func_Add_Int32_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: AddInt64Obj => func_Add_Int64_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: AddReal32Obj => func_Add_Real32_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: AddReal64Obj => func_Add_Real64_obj
    !!
  GENERIC, PUBLIC :: OPERATOR(+) => &
    & AddObjObj, AddObjMono, &
    & AddObjInt8, AddObjInt16, AddObjInt32, AddObjInt64, &
    & AddObjReal32, AddObjReal64, &
    & AddInt8Obj, AddInt16Obj, AddInt32Obj, AddInt64Obj, &
    & AddReal32Obj, AddReal64Obj
    !!
    !! OPERATOR(-)
    !!
  PROCEDURE, PUBLIC, PASS(obj1) :: SubtractObjObj => func_Subtract_obj_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: SubtractMonoObj => &
    & func_Subtract_mono_obj
  PROCEDURE, PUBLIC, PASS(obj1) :: SubtractObjMono => &
    & func_Subtract_obj_mono
  PROCEDURE, PUBLIC, PASS(obj1) :: SubtractObjInt8 => &
    & func_Subtract_obj_Int8
  PROCEDURE, PUBLIC, PASS(obj1) :: SubtractObjInt16 => &
    & func_Subtract_obj_Int16
  PROCEDURE, PUBLIC, PASS(obj1) :: SubtractObjInt32 => &
    & func_Subtract_obj_Int32
  PROCEDURE, PUBLIC, PASS(obj1) :: SubtractObjInt64 => &
    & func_Subtract_obj_Int64
  PROCEDURE, PUBLIC, PASS(obj1) :: SubtractObjReal32 => &
    & func_Subtract_obj_Real32
  PROCEDURE, PUBLIC, PASS(obj1) :: SubtractObjReal64 => &
    & func_Subtract_obj_Real64
  PROCEDURE, PUBLIC, PASS(obj2) :: SubtractInt8Obj => &
    & func_Subtract_Int8_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: SubtractInt16Obj => &
    & func_Subtract_Int16_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: SubtractInt32Obj => &
    & func_Subtract_Int32_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: SubtractInt64Obj => &
    & func_Subtract_Int64_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: SubtractReal32Obj => &
    & func_Subtract_Real32_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: SubtractReal64Obj => &
    & func_Subtract_Real64_obj
    !!
  GENERIC, PUBLIC :: OPERATOR(-) => &
    & SubtractObjObj, SubtractObjMono, &
    & SubtractObjInt8, SubtractObjInt16, &
    & SubtractObjInt32, SubtractObjInt64, &
    & SubtractObjReal32, SubtractObjReal64, &
    & SubtractInt8Obj, SubtractInt16Obj, &
    & SubtractInt32Obj, SubtractInt64Obj, &
    & SubtractReal32Obj, SubtractReal64Obj
    !!
    !! OPERATOR(*)
    !!
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjObj => &
    & func_Multiplication_obj_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: MultiplicationMonoObj => &
    & func_Multiplication_mono_obj
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjMono => &
    & func_Multiplication_obj_mono
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjInt8 => &
    & func_Multiplication_obj_Int8
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjInt16 => &
    & func_Multiplication_obj_Int16
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjInt32 => &
    & func_Multiplication_obj_Int32
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjInt64 => &
    & func_Multiplication_obj_Int64
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjReal32 => &
    & func_Multiplication_obj_Real32
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjReal64 => &
    & func_Multiplication_obj_Real64
  PROCEDURE, PUBLIC, PASS(obj2) :: MultiplicationInt8Obj => &
    & func_Multiplication_Int8_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: MultiplicationInt16Obj => &
    & func_Multiplication_Int16_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: MultiplicationInt32Obj => &
    & func_Multiplication_Int32_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: MultiplicationInt64Obj => &
    & func_Multiplication_Int64_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: MultiplicationReal32Obj => &
    & func_Multiplication_Real32_obj
  PROCEDURE, PUBLIC, PASS(obj2) :: MultiplicationReal64Obj => &
    & func_Multiplication_Real64_obj
    !!
  GENERIC, PUBLIC :: OPERATOR(*) => &
    & MultiplicationObjObj, MultiplicationObjMono, &
    & MultiplicationObjInt8, MultiplicationObjInt16, &
    & MultiplicationObjInt32, MultiplicationObjInt64, &
    & MultiplicationObjReal32, MultiplicationObjReal64, &
    & MultiplicationInt8Obj, MultiplicationInt16Obj, &
    & MultiplicationInt32Obj, MultiplicationInt64Obj, &
    & MultiplicationReal32Obj, MultiplicationReal64Obj
    !!
    !! @AssignmentMethods
    !!
  PROCEDURE, PASS(obj) :: AssignObjObj => func_AssignObjObj
  PROCEDURE, PASS(obj) :: AssignObjMono => func_AssignObjMono
  PROCEDURE, PUBLIC, PASS(obj) :: AssignObjInt8 => &
    & func_AssignObjInt8
  PROCEDURE, PUBLIC, PASS(obj) :: AssignObjInt16 => &
    & func_AssignObjInt16
  PROCEDURE, PUBLIC, PASS(obj) :: AssignObjInt32 => &
    & func_AssignObjInt32
  PROCEDURE, PUBLIC, PASS(obj) :: AssignObjInt64 => &
    & func_AssignObjInt64
  PROCEDURE, PUBLIC, PASS(obj) :: AssignObjReal32 => &
    & func_AssignObjReal32
  PROCEDURE, PUBLIC, PASS(obj) :: AssignObjReal64 => &
    & func_AssignObjReal64
  GENERIC, PUBLIC :: ASSIGNMENT(=) => AssignObjObj, &
    & AssignObjMono, AssignObjInt8, &
    & AssignObjInt16, AssignObjInt32, &
    & AssignObjInt64, AssignObjReal32, &
    & AssignObjReal64
END TYPE Polynomial2D_

PUBLIC :: Polynomial2D_

!----------------------------------------------------------------------------
!                                                       Polynomial2DPointer_
!----------------------------------------------------------------------------

TYPE :: Polynomial2DPointer_
  CLASS(Polynomial2D_), POINTER :: ptr => NULL()
END TYPE Polynomial2DPointer_

PUBLIC :: Polynomial2DPointer_

!----------------------------------------------------------------------------
!                                            Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Initiate

INTERFACE
  MODULE PURE SUBROUTINE func_Initiate(obj, coeff, degree, varname1, varname2)
    CLASS(Polynomial2D_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: coeff(:)
  !! coefficients
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
  !! degrees of x and y
    CHARACTER(*), INTENT(IN) :: varname1
  !! variable x
    CHARACTER(*), INTENT(IN) :: varname2
  !! variable y
  END SUBROUTINE func_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                            Polynomial2D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Polynomial2D
!
!# Introduction
!
! This routine currently does not work properly: See #137

INTERFACE
  MODULE PURE FUNCTION func_Polynomial2D1(coeff, degree, varname1, varname2) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: coeff(:)
  !! coefficients
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
  !! degrees of x and y
    CHARACTER(*), INTENT(IN) :: varname1
  !! variable x
    CHARACTER(*), INTENT(IN) :: varname2
  !! variable y
    TYPE(Polynomial2D_) :: ans
  END FUNCTION func_Polynomial2D1
END INTERFACE

INTERFACE Polynomial2D
  MODULE PROCEDURE func_Polynomial2D1
END INTERFACE Polynomial2D

PUBLIC :: Polynomial2D

!----------------------------------------------------------------------------
!                                    Polynomial2D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Polynomial2D

INTERFACE
  MODULE FUNCTION func_Polynomial2D_Pointer1(coeff, degree, varname1,&
    &  varname2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: coeff(:)
  !! coefficients
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
  !! degree of x and y
    CHARACTER(*), INTENT(IN) :: varname1
  !! x
    CHARACTER(*), INTENT(IN) :: varname2
  !! y
    CLASS(Polynomial2D_), POINTER :: ans
  END FUNCTION func_Polynomial2D_Pointer1
END INTERFACE

INTERFACE Polynomial2D_Pointer
  MODULE PROCEDURE func_Polynomial2D_Pointer1
END INTERFACE Polynomial2D_Pointer

PUBLIC :: Polynomial2D_Pointer

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Deallocate the object

INTERFACE
  MODULE SUBROUTINE func_Deallocate(obj)
    CLASS(Polynomial2D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE Polynomial2DDeallocate
  MODULE PROCEDURE func_Deallocate
END INTERFACE Polynomial2DDeallocate

PUBLIC :: Polynomial2DDeallocate

INTERFACE
  MODULE SUBROUTINE func_Final(obj)
    TYPE(Polynomial2D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE ELEMENTAL FUNCTION func_evalscalar(obj, x, y) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: y
    REAL(DFP) :: ans
  END FUNCTION func_evalscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE PURE FUNCTION func_evalvector(obj, x) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION func_evalvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                   EvalGradient@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function f(x)

INTERFACE
  MODULE ELEMENTAL FUNCTION func_EvalGradient(obj, x, y, dim) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: y
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP) :: ans
  END FUNCTION func_EvalGradient
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Grad@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the gradient of function df/dx

INTERFACE
  MODULE FUNCTION func_Grad(obj, dim) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    TYPE(Polynomial2D_) :: ans
  END FUNCTION func_Grad
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetStringForUID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the gradient of function df/dx

INTERFACE
  MODULE ELEMENTAL FUNCTION func_GetStringForUID(obj) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION func_GetStringForUID
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetDegree@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the gradient of function df/dx

INTERFACE
  MODULE PURE FUNCTION func_GetDegree(obj) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION func_GetDegree
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetStringForUID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Get the display string

INTERFACE
  MODULE FUNCTION func_GetDisplayString(obj) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION func_GetDisplayString
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetCoeff@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Get the coefficient

INTERFACE
  MODULE PURE FUNCTION func_GetCoeff(obj) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION func_GetCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2022
! summary: Get the order of the polynomial
!
!# Introduction
!
! This function returns the order of polynomial, which is
!
! order = MAX( degree1+ degree2 )

INTERFACE
  MODULE ELEMENTAL FUNCTION func_GetOrder(obj) RESULT(ans)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION func_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Display the content of Polynomial2D

INTERFACE
  MODULE SUBROUTINE func_Display(obj, msg, unitno)
    CLASS(Polynomial2D_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE func_Display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./inc/AddOperator.inc"
#include "./inc/SubtractOperator.inc"
#include "./inc/MultiplicationOperator.inc"
#include "./inc/AssignOperator.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Polynomial2D_Class
