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
USE String_Class, ONLY: String
USE GlobalData
USE AbstractFunction_Class
USE AbstractBasis_Class
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

TYPE, EXTENDS( AbstractBasis3D_ ) :: Monomial3D_
  PRIVATE
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
    PROCEDURE, PUBLIC, PASS( obj ) :: GetDisplayString =>  &
      & func_GetDisplayString
    PROCEDURE, PUBLIC, PASS( obj ) :: GetDegree => func_GetDegree
    PROCEDURE, PUBLIC, PASS( obj ) :: GetCoeff => func_GetCoeff
    !!
    !! @DisplayMethods
    !!
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => func_Display
    !!
    !! @OperatorMethods
    !!
    !! OPERATOR(*)
    !!
    PROCEDURE, PUBLIC, PASS( obj1 ) :: MultiplicationObjObj => &
      & func_Multiplication_obj_obj
    GENERIC, PUBLIC :: OPERATOR( * ) => MultiplicationObjObj
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
MODULE ELEMENTAL FUNCTION func_Monomial3D1( n1, n2, n3, name1, name2,  &
  & name3 ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2, n3
  CHARACTER( LEN = * ), INTENT( IN ) :: name1, name2, name3
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
MODULE ELEMENTAL FUNCTION func_Monomial3D2( f1, f2, f3 ) RESULT( ans )
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
MODULE FUNCTION func_Monomial3D_Pointer1( n1, n2, n3, name1, name2, name3 ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2, n3
  CHARACTER( LEN = * ), INTENT( IN ) :: name1, name2, name3
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
MODULE FUNCTION func_Monomial3D_Pointer2( f1, f2, f3 ) RESULT( ans )
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
  MODULE ELEMENTAL FUNCTION func_eval( obj, x, y, z ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ), INTENT( IN ) :: y
    REAL( DFP ), INTENT( IN ) :: z
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
  MODULE ELEMENTAL FUNCTION func_EvalGradient( obj, x, y, z, dim ) &
    & RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ), INTENT( IN ) :: y
    REAL( DFP ), INTENT( IN ) :: z
    INTEGER( I4B ), INTENT( IN ) :: dim
    REAL( DFP ) :: ans
  END FUNCTION func_EvalGradient
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Grad@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Grad( obj, dim ) RESULT( ans )
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
  MODULE ELEMENTAL FUNCTION func_GetStringForUID( obj ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    TYPE( String ) :: ans
  END FUNCTION func_GetStringForUID
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetStringForUID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Get the display string

INTERFACE
  MODULE ELEMENTAL FUNCTION func_GetDisplayString( obj ) RESULT( ans )
    CLASS( Monomial3D_ ), INTENT( IN ) :: obj
    TYPE( String ) :: ans
  END FUNCTION func_GetDisplayString
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetDegree@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the gradient of function df/dx

INTERFACE
  MODULE ELEMENTAL FUNCTION func_GetDegree( obj ) RESULT( ans )
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
  MODULE ELEMENTAL FUNCTION func_GetCoeff( obj ) RESULT( ans )
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
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Monomial1D

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_obj( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Monomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_obj
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjObj( obj, obj2 )
  CLASS( Monomial3D_ ), INTENT( INOUT ) :: obj
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjObj
END INTERFACE

END MODULE Monomial3D_Class