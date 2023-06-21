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

MODULE AbstractFunction_Class
USE String_Class, ONLY: String
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
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_Deallocate
END TYPE AbstractFunction_

PUBLIC :: AbstractFunction_

!----------------------------------------------------------------------------
!                                                  AbstractFunctionPointer_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractFunctionPointer_
  CLASS(AbstractFunction_), POINTER :: ptr => NULL()
END TYPE AbstractFunctionPointer_

PUBLIC :: AbstractFunctionPointer_

!----------------------------------------------------------------------------
!                                                  AbstractFunction_Class
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Abstract Expression class is defined

TYPE, ABSTRACT, EXTENDS(AbstractFunction_) :: AbstractFunction1D_
  TYPE(String) :: varname
CONTAINS
  PRIVATE
  PROCEDURE(func_EvalScalar1), DEFERRED, PUBLIC, PASS(Obj) :: EvalScalar
  GENERIC, PUBLIC :: Eval => EvalScalar !!, EvalVector
  PROCEDURE(func_EvalGradient1), DEFERRED, PUBLIC, PASS(Obj) :: &
    & EvalGradient
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_Deallocate1
  PROCEDURE, PUBLIC, PASS(obj) :: GetVarName => func_GetVarName1
END TYPE AbstractFunction1D_

PUBLIC :: AbstractFunction1D_

!----------------------------------------------------------------------------
!                                                  AbstractFunctionPointer1D_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractFunctionPointer1D_
  CLASS(AbstractFunction1D_), POINTER :: ptr => NULL()
END TYPE AbstractFunctionPointer1D_

PUBLIC :: AbstractFunctionPointer1D_

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for single variable function

ABSTRACT INTERFACE
  RECURSIVE PURE FUNCTION func_evalscalar1(obj, x) RESULT(ans)
    IMPORT AbstractFunction1D_, DFP
    CLASS(AbstractFunction1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION func_evalscalar1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for single variable function

ABSTRACT INTERFACE
  RECURSIVE PURE FUNCTION func_evalvector1(obj, x) RESULT(ans)
    IMPORT AbstractFunction1D_, DFP
    CLASS(AbstractFunction1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION func_evalvector1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              EvalGradient
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate gradient for 1d argument function

ABSTRACT INTERFACE
  RECURSIVE PURE FUNCTION func_EvalGradient1(obj, x) RESULT(ans)
    IMPORT AbstractFunction1D_, DFP
    CLASS(AbstractFunction1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION func_EvalGradient1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE SUBROUTINE func_Deallocate1(obj)
    CLASS(AbstractFunction1D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate1
END INTERFACE

INTERFACE AbstractFunction1DDeallocate
  MODULE PROCEDURE func_Deallocate1
END INTERFACE AbstractFunction1DDeallocate

PUBLIC :: AbstractFunction1DDeallocate

!----------------------------------------------------------------------------
!                                                         GetVarName@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-08
! summary: returns the name of the variables

INTERFACE
  MODULE PURE FUNCTION func_GetVarname1(obj) RESULT(ans)
    CLASS(AbstractFunction1D_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION func_GetVarname1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       AbstractFunction2D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Abstract Expression class is defined

TYPE, ABSTRACT, EXTENDS(AbstractFunction_) :: AbstractFunction2D_
  TYPE(String) :: varname(2)
CONTAINS
  PRIVATE
  PROCEDURE(func_Eval2), DEFERRED, PUBLIC, PASS(Obj) :: EvalScalar
  GENERIC, PUBLIC :: Eval => EvalScalar !!, EvalVector
  PROCEDURE(func_EvalGradient2), DEFERRED, PUBLIC, PASS(Obj) :: &
    & EvalGradient
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_Deallocate2
  PROCEDURE, PUBLIC, PASS(obj) :: GetVarName => func_GetVarName2
END TYPE AbstractFunction2D_

PUBLIC :: AbstractFunction2D_

!----------------------------------------------------------------------------
!                                                  AbstractFunctionPointer1D_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractFunctionPointer2D_
  CLASS(AbstractFunction2D_), POINTER :: ptr => NULL()
END TYPE AbstractFunctionPointer2D_

PUBLIC :: AbstractFunctionPointer2D_

!----------------------------------------------------------------------------
!                                                       AbstractFunction3D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Abstract Expression class is defined

TYPE, ABSTRACT, EXTENDS(AbstractFunction_) :: AbstractFunction3D_
  TYPE(String) :: varname(3)
CONTAINS
  PRIVATE
  PROCEDURE(func_Eval3), DEFERRED, PUBLIC, PASS(Obj) :: EvalScalar
  GENERIC, PUBLIC :: Eval => EvalScalar
  PROCEDURE(func_EvalGradient3), DEFERRED, PUBLIC, PASS(Obj) :: &
    & EvalGradient
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_Deallocate3
  PROCEDURE, PUBLIC, PASS(obj) :: GetVarName => func_GetVarName3
END TYPE AbstractFunction3D_

PUBLIC :: AbstractFunction3D_

!----------------------------------------------------------------------------
!                                                  AbstractFunctionPointer1D_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractFunctionPointer3D_
  CLASS(AbstractFunction3D_), POINTER :: ptr => NULL()
END TYPE AbstractFunctionPointer3D_

PUBLIC :: AbstractFunctionPointer3D_

!----------------------------------------------------------------------------
!                                                       AbstractFunctionND_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Abstract Expression class is defined

TYPE, ABSTRACT, EXTENDS(AbstractFunction_) :: AbstractFunctionND_
  TYPE(String), ALLOCATABLE :: varname(:)
CONTAINS
  PRIVATE
  PROCEDURE(func_EvalN), DEFERRED, PUBLIC, PASS(Obj) :: Eval
  PROCEDURE(func_EvalGradientN), DEFERRED, PUBLIC, PASS(Obj) :: &
    & EvalGradient
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_DeallocateN
  PROCEDURE, PUBLIC, PASS(obj) :: GetVarName => func_GetVarNameN
END TYPE AbstractFunctionND_

PUBLIC :: AbstractFunctionND_

!----------------------------------------------------------------------------
!                                                  AbstractFunctionPointer1D_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractFunctionPointerND_
  CLASS(AbstractFunctionND_), POINTER :: ptr => NULL()
END TYPE AbstractFunctionPointerND_

PUBLIC :: AbstractFunctionPointerND_

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for double variable function

ABSTRACT INTERFACE
  ELEMENTAL FUNCTION func_eval2(obj, x, y) RESULT(ans)
    IMPORT AbstractFunction2D_, DFP
    CLASS(AbstractFunction2D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: y
    REAL(DFP) :: ans
  END FUNCTION func_eval2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for three variable function

ABSTRACT INTERFACE
  ELEMENTAL FUNCTION func_eval3(obj, x, y, z) RESULT(ans)
    IMPORT AbstractFunction3D_, DFP
    CLASS(AbstractFunction3D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: y
    REAL(DFP), INTENT(IN) :: z
    REAL(DFP) :: ans
  END FUNCTION func_eval3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for N variable function

ABSTRACT INTERFACE
  PURE FUNCTION func_evalN(obj, x) RESULT(ans)
    IMPORT AbstractFunctionND_, DFP
    CLASS(AbstractFunctionND_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans
  END FUNCTION func_evalN
END INTERFACE

!----------------------------------------------------------------------------
!                                                              EvalGradient
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate gradient for 2d argument function

ABSTRACT INTERFACE
  ELEMENTAL FUNCTION func_EvalGradient2(obj, x, y, dim) RESULT(ans)
    IMPORT AbstractFunction2D_, DFP, I4B
    CLASS(AbstractFunction2D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: y
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP) :: ans
  END FUNCTION func_EvalGradient2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              EvalGradient
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate gradient for 3d argument function

ABSTRACT INTERFACE
  ELEMENTAL FUNCTION func_EvalGradient3(obj, x, y, z, dim)  &
    & RESULT(ans)
    IMPORT AbstractFunction3D_, DFP, I4B
    CLASS(AbstractFunction3D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: y
    REAL(DFP), INTENT(IN) :: z
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP) :: ans
  END FUNCTION func_EvalGradient3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              EvalGradient
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate gradient for Nd argument function

ABSTRACT INTERFACE
  PURE FUNCTION func_EvalGradientN(obj, x, dim) RESULT(ans)
    IMPORT AbstractFunctionND_, DFP, I4B
    CLASS(AbstractFunctionND_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP) :: ans
  END FUNCTION func_EvalGradientN
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE SUBROUTINE func_Deallocate(obj)
    CLASS(AbstractFunction_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE AbstractFunctionDeallocate
  MODULE PROCEDURE func_Deallocate
END INTERFACE AbstractFunctionDeallocate

PUBLIC :: AbstractFunctionDeallocate

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE SUBROUTINE func_Deallocate2(obj)
    CLASS(AbstractFunction2D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate2
END INTERFACE

INTERFACE AbstractFunction2DDeallocate
  MODULE PROCEDURE func_Deallocate2
END INTERFACE AbstractFunction2DDeallocate

PUBLIC :: AbstractFunction2DDeallocate

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE SUBROUTINE func_Deallocate3(obj)
    CLASS(AbstractFunction3D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate3
END INTERFACE

INTERFACE AbstractFunction3DDeallocate
  MODULE PROCEDURE func_Deallocate3
END INTERFACE AbstractFunction3DDeallocate

PUBLIC :: AbstractFunction3DDeallocate

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE SUBROUTINE func_DeallocateN(obj)
    CLASS(AbstractFunctionND_), INTENT(INOUT) :: obj
  END SUBROUTINE func_DeallocateN
END INTERFACE

INTERFACE AbstractFunctionNDDeallocate
  MODULE PROCEDURE func_DeallocateN
END INTERFACE AbstractFunctionNDDeallocate

PUBLIC :: AbstractFunctionNDDeallocate

!----------------------------------------------------------------------------
!                                                                 GetVarName
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION func_GetVarname2(obj) RESULT(ans)
    CLASS(AbstractFunction2D_), INTENT(IN) :: obj
    TYPE(String) :: ans(2)
  END FUNCTION func_GetVarname2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetVarName
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION func_GetVarname3(obj) RESULT(ans)
    CLASS(AbstractFunction3D_), INTENT(IN) :: obj
    TYPE(String) :: ans(3)
  END FUNCTION func_GetVarname3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetVarName
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION func_GetVarnameN(obj) RESULT(ans)
    CLASS(AbstractFunctionND_), INTENT(IN) :: obj
    TYPE(String), ALLOCATABLE :: ans(:)
  END FUNCTION func_GetVarnameN
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractFunction_Class
