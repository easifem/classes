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

MODULE Polynomial_Class
USE GlobalData
USE FPL, ONLY: ParameterList_
USE String_Class, ONLY: String
USE ExceptionHandler_Class
USE AbstractFunction_Class
IMPLICIT NONE
PRIVATE

CHARACTER(LEN=*), PARAMETER :: modName = "Polynomial_Class"
TYPE(ExceptionHandler_) :: e

!----------------------------------------------------------------------------
!                                                                Polynomial_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Polynomial class is defined
!
!# Introduction
!
! - The length of Coed is m
! - The shape of Power is ( m, n )
! - n is the total of unknowns
! - Each row of Power denotes the power corresponding to the coefficient.


TYPE, EXTENDS( AbstractFunction_ ) :: Polynomial_
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: power( :, : )
  !!
  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: CheckEssentialParam => &
      & func_CheckEssentialParam
    PROCEDURE, PUBLIC, PASS( obj ) :: Eval=>func_Eval
    PROCEDURE, PUBLIC, PASS( obj ) :: EvalGradient=>func_EvalGradient
    PROCEDURE, PUBLIC, PASS( obj ) :: Grad => func_Grad
    GENERIC, PUBLIC :: OPERATOR( .Grad. ) => Grad
    PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => func_Deallocate
    FINAL :: func_Final
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => func_Display
    PROCEDURE, PUBLIC, PASS( obj1 ) :: Add => func_Add
    GENERIC, PUBLIC :: OPERATOR( + ) => Add
    PROCEDURE, PUBLIC, PASS( obj1 ) :: Subtract => func_Subtract
    GENERIC, PUBLIC :: OPERATOR( - ) => Subtract
    PROCEDURE, PUBLIC, PASS( obj1 ) :: Multiplication => func_Multiplication
    GENERIC, PUBLIC :: OPERATOR( * ) => Multiplication
END TYPE Polynomial_

PUBLIC :: Polynomial_

!----------------------------------------------------------------------------
!                                                         PolynomialPointer_
!----------------------------------------------------------------------------

TYPE :: PolynomialPointer_
  CLASS( Polynomial_ ), POINTER :: ptr => NULL()
END TYPE PolynomialPointer_

PUBLIC :: PolynomialPointer_

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Check essential parameter

INTERFACE
MODULE SUBROUTINE func_CheckEssentialParam( obj, param )
  CLASS( Polynomial_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE func_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                      setPolynomialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Set polynomial parameter

INTERFACE
MODULE SUBROUTINE setPolynomialParam( param, coeff, power )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  REAL( DFP ), INTENT( IN ) :: coeff( : )
  INTEGER( I4B ), INTENT( IN ) :: power( :, : )
END SUBROUTINE setPolynomialParam
END INTERFACE

PUBLIC :: setPolynomialParam

!----------------------------------------------------------------------------
!                                             Polynomial@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the polynomial

INTERFACE
MODULE FUNCTION func_Polynomial1( param ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Polynomial_ ) :: ans
END FUNCTION func_Polynomial1
END INTERFACE

INTERFACE Polynomial
  MODULE PROCEDURE func_Polynomial1
END INTERFACE Polynomial

PUBLIC :: Polynomial

!----------------------------------------------------------------------------
!                                     Polynomial_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the polynomial

INTERFACE
MODULE FUNCTION func_Polynomial_Pointer1( param ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  CLASS( Polynomial_ ), POINTER :: ans
END FUNCTION func_Polynomial_Pointer1
END INTERFACE

INTERFACE Polynomial_Pointer
  MODULE PROCEDURE func_Polynomial_Pointer1
END INTERFACE Polynomial_Pointer

PUBLIC :: Polynomial_Pointer


!----------------------------------------------------------------------------
!                                                            Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE PURE FUNCTION func_eval( obj, x ) RESULT( ans )
    CLASS( Polynomial_ ), INTENT( IN ) :: obj
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
    CLASS( Polynomial_ ), INTENT( IN ) :: obj
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
    CLASS( Polynomial_ ), INTENT( IN ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: dim
    CLASS( Polynomial_ ), POINTER :: ans
  END FUNCTION func_Grad
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Deallocate@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE SUBROUTINE func_Deallocate( obj )
    CLASS( Polynomial_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE
  MODULE SUBROUTINE func_Final( obj )
    TYPE( Polynomial_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE func_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Display the content of polynomial

INTERFACE
MODULE SUBROUTINE func_Display( obj, msg, unitno )
  CLASS( Polynomial_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE func_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Add@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Add two polynomial

INTERFACE
MODULE PURE FUNCTION func_Add( obj1, obj2 ) RESULT( Ans )
  CLASS( Polynomial_ ), INTENT( IN ) :: obj1
  CLASS( Polynomial_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial_ ) :: ans
END FUNCTION func_Add
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Substract@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Subtract two polynomial

INTERFACE
MODULE PURE FUNCTION func_Subtract( obj1, obj2 ) RESULT( Ans )
  CLASS( Polynomial_ ), INTENT( IN ) :: obj1
  CLASS( Polynomial_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial_ ) :: ans
END FUNCTION func_Subtract
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Multiplication@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two polynomial

INTERFACE
MODULE PURE FUNCTION func_Multiplication( obj1, obj2 ) RESULT( Ans )
  CLASS( Polynomial_ ), INTENT( IN ) :: obj1
  CLASS( Polynomial_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial_ ) :: ans
END FUNCTION func_Multiplication
END INTERFACE


END MODULE Polynomial_Class