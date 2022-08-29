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

MODULE Monomial1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractFunction_Class
USE AbstractBasis_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                Monomial1D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Monomial1D class is defined
!

TYPE, EXTENDS(AbstractBasis1D_) :: Monomial1D_
  PRIVATE
  INTEGER(I4B) :: degree = 0
CONTAINS
    !!
    !! @ConstructorMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => func_Initiate
    !! Initiate the monomial
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => func_Deallocate
  FINAL :: func_Final
    !!
    !! @GetMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: EvalScalar => func_EvalScalar
  PROCEDURE, PUBLIC, PASS(obj) :: EvalVector => func_EvalVector
  PROCEDURE, PUBLIC, PASS(obj) :: EvalGradient => func_EvalGradient
  PROCEDURE, PUBLIC, PASS(obj) :: Grad => func_Grad
  GENERIC, PUBLIC :: OPERATOR(.Grad.) => Grad
  PROCEDURE, PUBLIC, PASS(obj) :: GetStringForUID => func_GetStringForUID
  PROCEDURE, PUBLIC, PASS(obj) :: GetDegree => func_GetDegree
  PROCEDURE, PUBLIC, PASS(obj) :: GetDisplayString => &
    & func_GetDisplayString
  PROCEDURE, PUBLIC, PASS(obj) :: GetCoeff => &
    & func_GetCoeff
    !!
    !! @IOMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: Display => func_Display
    !!
    !! @OperatorMethods
    !!
    !! OPERATOR(*)
    !!
  PROCEDURE, PUBLIC, PASS(obj1) :: MultiplicationObjObj => &
    & func_Multiplication_obj_obj
  GENERIC, PUBLIC :: OPERATOR(*) => MultiplicationObjObj
    !!
    !! @AssignmentMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: AssignObjObj => func_AssignObjObj
  GENERIC, PUBLIC :: ASSIGNMENT(=) => AssignObjObj
END TYPE Monomial1D_

PUBLIC :: Monomial1D_

!----------------------------------------------------------------------------
!                                                         Monomial1DPointer_
!----------------------------------------------------------------------------

TYPE :: Monomial1DPointer_
  CLASS(Monomial1D_), POINTER :: ptr => NULL()
END TYPE Monomial1DPointer_

PUBLIC :: Monomial1DPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Monomial1D

INTERFACE
  MODULE ELEMENTAL SUBROUTINE func_Initiate(obj, degree, varname)
    CLASS(Monomial1D_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: degree
    CHARACTER(LEN=*), INTENT(IN) :: varname
  END SUBROUTINE func_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Monomial1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Monomial1D

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Monomial1D1(degree, varname) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: degree
    CHARACTER(LEN=*), INTENT(IN) :: varname
    TYPE(Monomial1D_) :: ans
  END FUNCTION func_Monomial1D1
END INTERFACE

INTERFACE Monomial1D
  MODULE PROCEDURE func_Monomial1D1
END INTERFACE Monomial1D

PUBLIC :: Monomial1D

!----------------------------------------------------------------------------
!                                     Monomial1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Monomial1D

INTERFACE
  MODULE FUNCTION func_Monomial1D_Pointer1(degree, varname) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: degree
    CHARACTER(LEN=*), INTENT(IN) :: varname
    CLASS(Monomial1D_), POINTER :: ans
  END FUNCTION func_Monomial1D_Pointer1
END INTERFACE

INTERFACE Monomial1D_Pointer
  MODULE PROCEDURE func_Monomial1D_Pointer1
END INTERFACE Monomial1D_Pointer

PUBLIC :: Monomial1D_Pointer

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Deallocate the object

INTERFACE
  MODULE SUBROUTINE func_Deallocate(obj)
    CLASS(Monomial1D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE Monomial1DDeallocate
  MODULE PROCEDURE func_Deallocate
END INTERFACE Monomial1DDeallocate

PUBLIC :: Monomial1DDeallocate

INTERFACE
  MODULE SUBROUTINE func_Final(obj)
    TYPE(Monomial1D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE ELEMENTAL FUNCTION func_evalscalar(obj, x) RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
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
    CLASS(Monomial1D_), INTENT(IN) :: obj
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
  MODULE ELEMENTAL FUNCTION func_EvalGradient(obj, x) RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
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
  MODULE ELEMENTAL FUNCTION func_Grad(obj) RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj
    TYPE(Monomial1D_) :: ans
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
    CLASS(Monomial1D_), INTENT(IN) :: obj
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
  MODULE ELEMENTAL FUNCTION func_GetDegree(obj) RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION func_GetDegree
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetStringForUID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Get the display string

INTERFACE
  MODULE ELEMENTAL FUNCTION func_GetDisplayString(obj) RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION func_GetDisplayString
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetCoeff@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the gradient of function df/dx

INTERFACE
  MODULE ELEMENTAL FUNCTION func_GetCoeff(obj) RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION func_GetCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Display the content of Monomial1D

INTERFACE
  MODULE SUBROUTINE func_Display(obj, msg, unitno)
    CLASS(Monomial1D_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE func_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Monomial1D

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_obj_obj(obj1, obj2) &
    & RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj1
    CLASS(Monomial1D_), INTENT(IN) :: obj2
    TYPE(Monomial1D_) :: ans
  END FUNCTION func_Multiplication_obj_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Assign@AssignMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE func_AssignObjObj(obj, obj2)
    CLASS(Monomial1D_), INTENT(INOUT) :: obj
    CLASS(Monomial1D_), INTENT(IN) :: obj2
  END SUBROUTINE func_AssignObjObj
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Assign@AssignMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE func_AssignObjVecObjVec(obj, obj2)
    CLASS(Monomial1D_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    CLASS(Monomial1D_), INTENT(IN) :: obj2(:)
  END SUBROUTINE func_AssignObjVecObjVec
END INTERFACE

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE func_AssignObjVecObjVec
END INTERFACE ASSIGNMENT(=)

PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                  Monomials1D@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary:         Generate monomials from $x^0$ to $x^{order}$

INTERFACE
  MODULE PURE FUNCTION func_Monomials1D(order, varname) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname
    !! variable varname
    TYPE(Monomial1D_), ALLOCATABLE :: ans(:)
    !! vector of monomial
  END FUNCTION func_Monomials1D
END INTERFACE

INTERFACE Monomials1D
  MODULE PROCEDURE func_Monomials1D
END INTERFACE Monomials1D

PUBLIC :: Monomials1D

!----------------------------------------------------------------------------
!                                              EvenMonomials1D@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary:         Generate monomials from $x^0$ to $x^{order}$

INTERFACE
  MODULE PURE FUNCTION func_EvenMonomials1D(order, varname) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname
    !! variable varname
    TYPE(Monomial1D_), ALLOCATABLE :: ans(:)
    !! vector of monomial
  END FUNCTION func_EvenMonomials1D
END INTERFACE

INTERFACE EvenMonomials1D
  MODULE PROCEDURE func_EvenMonomials1D
END INTERFACE EvenMonomials1D

PUBLIC :: EvenMonomials1D

!----------------------------------------------------------------------------
!                                              OddMonomials1D@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary:         Generate monomials from $x^0$ to $x^{order}$

INTERFACE
  MODULE PURE FUNCTION func_OddMonomials1D(order, varname) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname
    !! variable varname
    TYPE(Monomial1D_), ALLOCATABLE :: ans(:)
    !! vector of monomial
  END FUNCTION func_OddMonomials1D
END INTERFACE

INTERFACE OddMonomials1D
  MODULE PROCEDURE func_OddMonomials1D
END INTERFACE OddMonomials1D

PUBLIC :: OddMonomials1D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Monomial1D_Class
