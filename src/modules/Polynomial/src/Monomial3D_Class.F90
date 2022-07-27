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

MODULE Monomial3D_Class
USE GlobalData
USE String_Class, ONLY: String
USE AbstractFunction_Class
USE AbstractMonomial_Class
USE Monomial1D_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "Monomial3D_Class"
INTEGER( I4B ), PARAMETER :: MAX_COMPONENTS = 3

!----------------------------------------------------------------------------
!                                                                Monomial3D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Monomial3D class is defined

TYPE, EXTENDS( AbstractMonomial_ ) :: Monomial3D_
  TYPE( Monomial1D_ ) :: x( MAX_COMPONENTS )
  CONTAINS
    !!
    !! @ConstructorMethods
    !!
    PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => func_Deallocate
    FINAL :: func_Final
    !!
    !! @GetMethods
    !!
    PROCEDURE, PUBLIC, PASS( obj ) :: Eval=>func_Eval
    PROCEDURE, PUBLIC, PASS( obj ) :: EvalGradient=>func_EvalGradient
    PROCEDURE, PUBLIC, PASS( obj ) :: Grad => func_Grad
    GENERIC, PUBLIC :: OPERATOR( .Grad. ) => Grad
    PROCEDURE, PUBLIC, PASS( obj ) :: GetStringForUID => func_GetStringForUID
    PROCEDURE, PUBLIC, PASS( obj ) :: GetDegree => func_GetDegree
    PROCEDURE, PUBLIC, PASS( obj ) :: GetCoeff => func_GetCoeff
    !!
    !! @DisplayMethods
    !!
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => func_Display
    !!
    !! @OperatorMethods
    !!
    !! +
    PROCEDURE, PUBLIC, PASS( obj1 ) :: AddObjObj => func_Add_obj_obj
    PROCEDURE, PUBLIC, PASS( obj1 ) :: AddObjInt8 => func_Add_obj_Int8
    PROCEDURE, PUBLIC, PASS( obj1 ) :: AddObjInt16 => func_Add_obj_Int16
    PROCEDURE, PUBLIC, PASS( obj1 ) :: AddObjInt32 => func_Add_obj_Int32
    PROCEDURE, PUBLIC, PASS( obj1 ) :: AddObjInt64 => func_Add_obj_Int64
    PROCEDURE, PUBLIC, PASS( obj1 ) :: AddObjReal32 => func_Add_obj_Real32
    PROCEDURE, PUBLIC, PASS( obj1 ) :: AddObjReal64 => func_Add_obj_Real64
    PROCEDURE, PUBLIC, PASS( obj2 ) :: AddInt8Obj => func_Add_Int8_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: AddInt16Obj => func_Add_Int16_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: AddInt32Obj => func_Add_Int32_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: AddInt64Obj => func_Add_Int64_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: AddReal32Obj => func_Add_Real32_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: AddReal64Obj => func_Add_Real64_obj
    GENERIC, PUBLIC :: OPERATOR( + ) => AddObjObj, AddObjInt8, AddObjInt16, &
      & AddObjInt32, AddObjInt64, AddObjReal32, AddObjReal64, &
      & AddInt8Obj, AddInt16Obj, AddInt32Obj, AddInt64Obj, &
      & AddReal32Obj, AddReal64Obj
    !!
    !! -
    !!
    PROCEDURE, PUBLIC, PASS( obj1 ) :: SubtractObjObj => func_Subtract_obj_obj
    PROCEDURE, PUBLIC, PASS( obj1 ) :: SubtractObjInt8 => &
      & func_Subtract_obj_Int8
    PROCEDURE, PUBLIC, PASS( obj1 ) :: SubtractObjInt16 => &
      & func_Subtract_obj_Int16
    PROCEDURE, PUBLIC, PASS( obj1 ) :: SubtractObjInt32 => &
      & func_Subtract_obj_Int32
    PROCEDURE, PUBLIC, PASS( obj1 ) :: SubtractObjInt64 => &
      & func_Subtract_obj_Int64
    PROCEDURE, PUBLIC, PASS( obj1 ) :: SubtractObjReal32 => &
      & func_Subtract_obj_Real32
    PROCEDURE, PUBLIC, PASS( obj1 ) :: SubtractObjReal64 => &
      & func_Subtract_obj_Real64
    PROCEDURE, PUBLIC, PASS( obj2 ) :: SubtractInt8Obj => &
      & func_Subtract_Int8_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: SubtractInt16Obj => &
      & func_Subtract_Int16_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: SubtractInt32Obj => &
      & func_Subtract_Int32_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: SubtractInt64Obj => &
      & func_Subtract_Int64_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: SubtractReal32Obj => &
      & func_Subtract_Real32_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: SubtractReal64Obj => &
      & func_Subtract_Real64_obj
    GENERIC, PUBLIC :: OPERATOR( - ) => SubtractObjObj, &
      & SubtractObjInt8, SubtractObjInt16, &
      & SubtractObjInt32, SubtractObjInt64, SubtractObjReal32, &
      & SubtractObjReal64, &
      & SubtractInt8Obj, SubtractInt16Obj, SubtractInt32Obj, &
      & SubtractInt64Obj, &
      & SubtractReal32Obj, SubtractReal64Obj
    !!
    !! *
    !!
    PROCEDURE, PUBLIC, PASS( obj1 ) :: MultiplicationObjObj => &
      & func_Multiplication_obj_obj
    PROCEDURE, PUBLIC, PASS( obj1 ) :: MultiplicationObjInt8 => &
      & func_Multiplication_obj_Int8
    PROCEDURE, PUBLIC, PASS( obj1 ) :: MultiplicationObjInt16 => &
      & func_Multiplication_obj_Int16
    PROCEDURE, PUBLIC, PASS( obj1 ) :: MultiplicationObjInt32 => &
      & func_Multiplication_obj_Int32
    PROCEDURE, PUBLIC, PASS( obj1 ) :: MultiplicationObjInt64 => &
      & func_Multiplication_obj_Int64
    PROCEDURE, PUBLIC, PASS( obj1 ) :: MultiplicationObjReal32 => &
      & func_Multiplication_obj_Real32
    PROCEDURE, PUBLIC, PASS( obj1 ) :: MultiplicationObjReal64 => &
      & func_Multiplication_obj_Real64
    PROCEDURE, PUBLIC, PASS( obj2 ) :: MultiplicationInt8Obj => &
      & func_Multiplication_Int8_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: MultiplicationInt16Obj => &
      & func_Multiplication_Int16_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: MultiplicationInt32Obj => &
      & func_Multiplication_Int32_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: MultiplicationInt64Obj => &
      & func_Multiplication_Int64_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: MultiplicationReal32Obj => &
      & func_Multiplication_Real32_obj
    PROCEDURE, PUBLIC, PASS( obj2 ) :: MultiplicationReal64Obj => &
      & func_Multiplication_Real64_obj
    GENERIC, PUBLIC :: OPERATOR( * ) => MultiplicationObjObj, &
      & MultiplicationObjInt8, MultiplicationObjInt16, &
      & MultiplicationObjInt32, MultiplicationObjInt64, &
      & MultiplicationObjReal32, &
      & MultiplicationObjReal64, &
      & MultiplicationInt8Obj, MultiplicationInt16Obj, &
      & MultiplicationInt32Obj, &
      & MultiplicationInt64Obj, &
      & MultiplicationReal32Obj, MultiplicationReal64Obj
    !!
    !! @AssignmentMethods
    !!
    PROCEDURE, PUBLIC, PASS( obj ) :: AssignObjObj => func_AssignObjObj
    GENERIC, PUBLIC :: ASSIGNMENT( = ) => AssignObjObj
END TYPE Monomial3D_

PUBLIC :: Monomial3D_

!----------------------------------------------------------------------------
!                                                         Monomial3DPointer_
!----------------------------------------------------------------------------

TYPE :: Monomial3DPointer_
  CLASS( Monomial3D_ ), POINTER :: ptr => NULL()
END TYPE Monomial3DPointer_

PUBLIC :: Monomial3DPointer_

!----------------------------------------------------------------------------
!                                             Monomial3D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Monomial3D

INTERFACE
MODULE PURE FUNCTION func_Monomial3D1( coeff, degree, varname ) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: coeff
  INTEGER( I4B ), INTENT( IN ) :: degree(MAX_COMPONENTS)
  CHARACTER( LEN = * ), INTENT( IN ) :: varname(MAX_COMPONENTS)
  TYPE( Monomial3D_ ) :: ans
END FUNCTION func_Monomial3D1
END INTERFACE

INTERFACE Monomial3D
  MODULE PROCEDURE func_Monomial3D1
END INTERFACE Monomial3D

PUBLIC :: Monomial3D

!----------------------------------------------------------------------------
!                                             Monomial3D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Monomial3D

INTERFACE
MODULE PURE FUNCTION func_Monomial3D2( coeff, f1, f2, f3 ) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: coeff
  CLASS( Monomial1D_ ), INTENT( IN ) :: f1
  CLASS( Monomial1D_ ), INTENT( IN ) :: f2
  CLASS( Monomial1D_ ), INTENT( IN ) :: f3
  TYPE( Monomial3D_ ) :: ans
END FUNCTION func_Monomial3D2
END INTERFACE

INTERFACE Monomial3D
  MODULE PROCEDURE func_Monomial3D2
END INTERFACE Monomial3D

!----------------------------------------------------------------------------
!                                     Monomial3D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Monomial3D

INTERFACE
MODULE FUNCTION func_Monomial3D_Pointer1( coeff, degree, varname ) &
  & RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: coeff
  INTEGER( I4B ), INTENT( IN ) :: degree(MAX_COMPONENTS)
  CHARACTER( LEN = * ), INTENT( IN ) :: varname(MAX_COMPONENTS)
  CLASS( Monomial3D_ ), POINTER :: ans
END FUNCTION func_Monomial3D_Pointer1
END INTERFACE

INTERFACE Monomial3D_Pointer
  MODULE PROCEDURE func_Monomial3D_Pointer1
END INTERFACE Monomial3D_Pointer

PUBLIC :: Monomial3D_Pointer

!----------------------------------------------------------------------------
!                                      Monomial3D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Monomial3D

INTERFACE
MODULE FUNCTION func_Monomial3D_Pointer2( coeff, f1, f2, f3 ) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: coeff
  CLASS( Monomial1D_ ), INTENT( IN ) :: f1
  CLASS( Monomial1D_ ), INTENT( IN ) :: f2
  CLASS( Monomial1D_ ), INTENT( IN ) :: f3
  CLASS( Monomial3D_ ), POINTER :: ans
END FUNCTION func_Monomial3D_Pointer2
END INTERFACE

INTERFACE Monomial3D_Pointer
  MODULE PROCEDURE func_Monomial3D_Pointer2
END INTERFACE Monomial3D_Pointer

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Deallocate the object

INTERFACE
  MODULE SUBROUTINE func_Deallocate( obj )
    CLASS( Monomial3D_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE Monomial3DDeallocate
  MODULE PROCEDURE func_Deallocate
END INTERFACE Monomial3DDeallocate

PUBLIC :: Monomial3DDeallocate

INTERFACE
  MODULE SUBROUTINE func_Final( obj )
    TYPE( Monomial3D_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE func_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE PURE FUNCTION func_eval( obj, x ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x( : )
    REAL( DFP ) :: ans
  END FUNCTION func_eval
END INTERFACE

!----------------------------------------------------------------------------
!                                                   EvalGradient@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE PURE FUNCTION func_EvalGradient( obj, x ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x( : )
    REAL( DFP ) :: ans( SIZE(x) )
  END FUNCTION func_EvalGradient
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Grad@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE PURE FUNCTION func_Grad( obj, dim ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: dim
    TYPE( Monomial3D_ ) :: ans
  END FUNCTION func_Grad
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Grad@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the gradient of function df/dx

INTERFACE
  MODULE PURE FUNCTION func_GetStringForUID( obj ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    TYPE( String ) :: ans
  END FUNCTION func_GetStringForUID
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetDegree@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the gradient of function df/dx

INTERFACE
  MODULE PURE FUNCTION func_GetDegree( obj ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    INTEGER( I4B ) :: ans
  END FUNCTION func_GetDegree
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetDegree@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Get the coefficient

INTERFACE
  MODULE PURE FUNCTION func_GetCoeff( obj ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    REAL( DFP ) :: ans
  END FUNCTION func_GetCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Display the content of Monomial3D

INTERFACE
MODULE SUBROUTINE func_Display( obj, msg, unitno )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE func_Display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./inc/Monomial3D_AddOperator.inc"
#include "./inc/Monomial3D_SubtractOperator.inc"
#include "./inc/Monomial3D_MultiplicationOperator.inc"
#include "./inc/Monomial3D_AssignOperator.inc"

END MODULE Monomial3D_Class