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

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Abstract Function Class is defined

MODULE  AbstractFunction_Class
  USE GlobalData
  IMPLICIT NONE
  PRIVATE

!----------------------------------------------------------------------------
!                                                  AbstractFunction_Class
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Abstract Expression class is defined

TYPE, ABSTRACT :: AbstractFunction_
  CONTAINS
    PROCEDURE(func_Eval), DEFERRED, PUBLIC, PASS( Obj ) :: Eval
    !! Evaluate the function
    GENERIC, PUBLIC :: OPERATOR( .Eval. ) => Eval
    !! Generic operator
    PROCEDURE(func_EvalGradient), DEFERRED, PUBLIC, PASS( Obj ) :: &
      & EvalGradient
    !! Evaluate the jacobian
    GENERIC, PUBLIC :: OPERATOR( .Grad. ) => EvalGradient
    !! Generic operator to get the jacobian
    PROCEDURE(func_Deallocate), DEFERRED, PUBLIC, PASS( Obj ) :: &
      & Deallocate
END TYPE AbstractFunction_

PUBLIC :: AbstractFunction_

!----------------------------------------------------------------------------
!                                                  AbstractFunctionPointer_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractFunctionPointer_
  CLASS( AbstractFunction_ ), POINTER :: ptr => NULL()
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: Eval => funcptr_Eval
  !! Evaluate the function
  GENERIC, PUBLIC :: OPERATOR( .Eval. ) => Eval
  !! Generic operator
  PROCEDURE, PUBLIC, PASS( Obj ) :: EvalGradient => funcptr_EvalGradient
  !! Evaluate the jacobian
  GENERIC, PUBLIC :: OPERATOR( .Grad. ) => EvalGradient
  !! Generic operator to get the jacobian
  PROCEDURE, PUBLIC, PASS( Obj ) :: Deallocate => funcptr_Deallocate
  !! Evaluate the jacobian
END TYPE AbstractFunctionPointer_

PUBLIC :: AbstractFunctionPointer_

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

ABSTRACT INTERFACE
  PURE FUNCTION func_eval( obj, x ) RESULT( ans )
    IMPORT AbstractFunction_, DFP
    CLASS( AbstractFunction_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x( : )
    REAL( DFP ) :: ans
  END FUNCTION func_eval
END INTERFACE

!----------------------------------------------------------------------------
!                                                              EvalGradient
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

ABSTRACT INTERFACE
  PURE FUNCTION func_EvalGradient( obj, x ) RESULT( ans )
    IMPORT AbstractFunction_, DFP
    CLASS( AbstractFunction_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x( : )
    REAL( DFP ) :: ans( SIZE(x) )
  END FUNCTION func_EvalGradient
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

ABSTRACT INTERFACE
  SUBROUTINE func_Deallocate( obj )
    IMPORT AbstractFunction_
    CLASS( AbstractFunction_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE PURE FUNCTION funcptr_eval( obj, x ) RESULT( ans )
    CLASS( AbstractFunctionPointer_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x( : )
    REAL( DFP ) :: ans
  END FUNCTION funcptr_eval
END INTERFACE

!----------------------------------------------------------------------------
!                                                               EvalGradient
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE PURE FUNCTION funcptr_EvalGradient( obj, x ) RESULT( ans )
    CLASS( AbstractFunctionPointer_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x( : )
    REAL( DFP ) :: ans( SIZE(x) )
  END FUNCTION funcptr_EvalGradient
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE SUBROUTINE funcptr_Deallocate( obj )
    CLASS( AbstractFunctionPointer_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE funcptr_Deallocate
END INTERFACE


END MODULE AbstractFunction_Class
