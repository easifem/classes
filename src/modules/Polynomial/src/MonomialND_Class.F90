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

MODULE MonomialND_Class
USE GlobalData
USE AbstractFunction_Class
USE AbstractMonomial_Class
USE Monomial1D_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "MonomialND_Class"
INTEGER( I4B ), PARAMETER :: MAX_STR_LEN=256

!----------------------------------------------------------------------------
!                                                                MonomialND_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: MonomialND class is defined

TYPE, EXTENDS( AbstractMonomial_ ) :: MonomialND_
  TYPE( Monomial1D_ ), ALLOCATABLE :: x( : )
  CONTAINS
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
END TYPE MonomialND_

PUBLIC :: MonomialND_

!----------------------------------------------------------------------------
!                                                         MonomialNDPointer_
!----------------------------------------------------------------------------

TYPE :: MonomialNDPointer_
  CLASS( MonomialND_ ), POINTER :: ptr => NULL()
END TYPE MonomialNDPointer_

PUBLIC :: MonomialNDPointer_

!----------------------------------------------------------------------------
!                                             MonomialND@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the MonomialND

INTERFACE
MODULE FUNCTION func_MonomialND1( degree, varname ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: degree(:)
  CHARACTER( LEN = * ), INTENT( IN ) :: varname(:)
  TYPE( MonomialND_ ) :: ans
END FUNCTION func_MonomialND1
END INTERFACE

INTERFACE MonomialND
  MODULE PROCEDURE func_MonomialND1
END INTERFACE MonomialND

PUBLIC :: MonomialND

!----------------------------------------------------------------------------
!                                     MonomialND_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the MonomialND

INTERFACE
MODULE FUNCTION func_MonomialND_Pointer1( degree, varname ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: degree(:)
  CHARACTER( LEN = * ), INTENT( IN ) :: varname(:)
  CLASS( MonomialND_ ), POINTER :: ans
END FUNCTION func_MonomialND_Pointer1
END INTERFACE

INTERFACE MonomialND_Pointer
  MODULE PROCEDURE func_MonomialND_Pointer1
END INTERFACE MonomialND_Pointer

PUBLIC :: MonomialND_Pointer

!----------------------------------------------------------------------------
!                                                            Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE PURE FUNCTION func_eval( obj, x ) RESULT( ans )
    CLASS( MonomialND_ ), INTENT( IN ) :: obj
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
    CLASS( MonomialND_ ), INTENT( IN ) :: obj
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
    CLASS( MonomialND_ ), INTENT( IN ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: dim
    CLASS( MonomialND_ ), POINTER :: ans
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
    CLASS( MonomialND_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE
  MODULE SUBROUTINE func_Final( obj )
    TYPE( MonomialND_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE func_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Display the content of MonomialND

INTERFACE
MODULE SUBROUTINE func_Display( obj, msg, unitno )
  CLASS( MonomialND_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE func_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Add@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Add two MonomialND

INTERFACE
MODULE PURE FUNCTION func_Add( obj1, obj2 ) RESULT( Ans )
  CLASS( MonomialND_ ), INTENT( IN ) :: obj1
  CLASS( MonomialND_ ), INTENT( IN ) :: obj2
  TYPE( MonomialND_ ) :: ans
END FUNCTION func_Add
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Substract@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Subtract two MonomialND

INTERFACE
MODULE PURE FUNCTION func_Subtract( obj1, obj2 ) RESULT( Ans )
  CLASS( MonomialND_ ), INTENT( IN ) :: obj1
  CLASS( MonomialND_ ), INTENT( IN ) :: obj2
  TYPE( MonomialND_ ) :: ans
END FUNCTION func_Subtract
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Multiplication@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two MonomialND

INTERFACE
MODULE PURE FUNCTION func_Multiplication( obj1, obj2 ) RESULT( Ans )
  CLASS( MonomialND_ ), INTENT( IN ) :: obj1
  CLASS( MonomialND_ ), INTENT( IN ) :: obj2
  TYPE( MonomialND_ ) :: ans
END FUNCTION func_Multiplication
END INTERFACE

END MODULE MonomialND_Class