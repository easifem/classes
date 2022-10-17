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

MODULE Jacobi1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractOrthopol1D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                 Jacobi1D_
!----------------------------------------------------------------------------

TYPE, EXTENDS(AbstractOrthopol1D_) :: Jacobi1D_
  PRIVATE
  REAL(DFP) :: alpha = 0.0_DFP
    !! alpha + 1 > 0
  REAL(DFP) :: beta = 0.0_DFP
    !! beta + 1 > 0
CONTAINS
  !!
  !! @ConstructorMethods
  !!
  FINAL :: Orthopol_Final
  !! Finalizer
  !!
  !! @GetMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: GetStringForUID => &
    & Orthopol_GetStringForUID
  !! Get string for creating UID
  PROCEDURE, PUBLIC, PASS(obj) :: Weight => Orthopol_Weight
  !! Weight of orthogonal polynomials
  PROCEDURE, PUBLIC, PASS(obj) :: GetRecurrenceCoeff => &
    & Orthopol_GetRecurrenceCoeff
  !! Get the recurrence coefficients
  PROCEDURE, PUBLIC, PASS(obj) :: GetCoeffScale => &
    & Orthopol_GetCoeffScale
  !! Get recurrence coefficient
  PROCEDURE, PUBLIC, PASS(obj) :: Zeros => Orthopol_Zeros
  !! zeros of polynomial
  PROCEDURE, PUBLIC, PASS(obj) :: GaussQuadrature => &
    & Orthopol_GaussQuadrature
  !! Gauss quadrature points and weights
  PROCEDURE, PUBLIC, PASS(obj) :: GaussRadauQuadrature => &
    & Orthopol_GaussRadauQuadrature
  !! Gauss-Radau quadrature points
  PROCEDURE, PUBLIC, PASS(obj) :: GaussLobattoQuadrature => &
    & Orthopol_GaussLobattoQuadrature
  PROCEDURE, PUBLIC, PASS(obj) :: BasisEvalGradientScalar => &
    & Orthopol_BasisEvalGradientScalar
  !! Evaluate grad of all the basis (n=0,1,...,n) at a given point
  PROCEDURE, PUBLIC, PASS(obj) :: BasisEvalGradientVector => &
    & Orthopol_BasisEvalGradientVector
  !! Evaluate grad of all the basis (n=0,1,...,n) at several points
  !!
  !! @SetMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: SetJacobiParam => &
    & Orthopol_SetJacobiParam
END TYPE Jacobi1D_

PUBLIC :: Jacobi1D_

!----------------------------------------------------------------------------
!                                                 Jacobi1DPointer_
!----------------------------------------------------------------------------

TYPE :: Jacobi1DPointer_
  CLASS(Jacobi1D_), POINTER :: ptr => NULL()
END TYPE Jacobi1DPointer_

PUBLIC :: Jacobi1DPointer_

!----------------------------------------------------------------------------
!                                     Jacobi1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Jacobi polynomial 1D

INTERFACE
  MODULE FUNCTION Jacobi1D1(varname, n, alpha, beta, &
    & isMonic, isOrthonormal) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: varname
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    !! 1+alpha > 0
    REAL(DFP), INTENT(IN) :: beta
    !! 1+beta > 0
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isMonic
    !! Default is .FALSE.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrthonormal
    !! Default is .FALSE.
    TYPE(Jacobi1D_) :: ans
    !! tree to be built
  END FUNCTION Jacobi1D1
END INTERFACE

INTERFACE Jacobi1D
  MODULE PROCEDURE Jacobi1D1
END INTERFACE Jacobi1D

PUBLIC :: Jacobi1D

!----------------------------------------------------------------------------
!                              Jacobi1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Jacobi polynomial 1D

INTERFACE
  MODULE FUNCTION Jacobi1D_Pointer1(varname, n, alpha, beta, &
    & isMonic, isOrthonormal) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: varname
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    !! 1+alpha > 0
    REAL(DFP), INTENT(IN) :: beta
    !! 1+beta > 0
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isMonic
    !! Default is .FALSE.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrthonormal
    !! Default is .FALSE.
    CLASS(Jacobi1D_), POINTER :: ans
    !! tree to be built
  END FUNCTION Jacobi1D_Pointer1
END INTERFACE

INTERFACE Jacobi1D_Pointer
  MODULE PROCEDURE Jacobi1D_Pointer1
END INTERFACE Jacobi1D_Pointer

PUBLIC :: Jacobi1D_Pointer

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Orthopol_Final(obj)
    TYPE(Jacobi1D_), INTENT(INOUT) :: obj
  END SUBROUTINE Orthopol_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetStringForUID@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION Orthopol_GetStringForUID(obj) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION Orthopol_GetStringForUID
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Weight@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Evaluate the weight
!

INTERFACE
  MODULE ELEMENTAL FUNCTION Orthopol_Weight(obj, x) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Orthopol_Weight
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetRecurrenceCoeff@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Returns the recurrence coefficient

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetRecurrenceCoeff(obj, n) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(0:n - 1, 1:2)
  END FUNCTION Orthopol_GetRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetCoeffScale@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE Orthopol_GetCoeffScale(obj, n, coeff, scale, &
    & isMonic, isOrthonormal)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(OUT) :: coeff(0:, 1:)
    REAL(DFP), INTENT(OUT) :: scale(0:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isMonic
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrthonormal
  END SUBROUTINE Orthopol_GetCoeffScale
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Zeros@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary:         Returns zeros of jacobi polynomial

INTERFACE
  MODULE FUNCTION Orthopol_Zeros(obj) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION Orthopol_Zeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary:         Returns Gauss quadrature points and weights

INTERFACE
  MODULE FUNCTION Orthopol_GaussQuadrature(obj) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Orthopol_GaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                          GaussRadauQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary:         Returns GaussRadau quadrature points and weights

INTERFACE
  MODULE FUNCTION Orthopol_GaussRadauQuadrature(obj, a) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: a
  !! it should be either + 1 or -1
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Orthopol_GaussRadauQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                          GaussLobattoQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary:         Returns GaussLobatto quadrature points and weights

INTERFACE
  MODULE FUNCTION Orthopol_GaussLobattoQuadrature(obj) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Orthopol_GaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                              BasisEvalGradient@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate grad of all basis at given point

INTERFACE
  MODULE PURE FUNCTION Orthopol_BasisEvalGradientScalar(obj, x, &
    & coeff, scale, n) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x
    !! scalar value of argument
    REAL(DFP), INTENT(IN) :: coeff(0:, 1:)
    !! recurrence coefficient
    REAL(DFP), INTENT(IN) :: scale(0:, 1:)
    !! scale coefficient
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP) :: ans(n + 1)
    !! n+1 values of grad of basis (n=0,1,2,..n)
  END FUNCTION Orthopol_BasisEvalGradientScalar
END INTERFACE

!----------------------------------------------------------------------------
!                                             BasisEvalGradient@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate all basis at given points

INTERFACE
  MODULE PURE FUNCTION Orthopol_BasisEvalGradientVector(obj, x, &
    & coeff, scale, n) RESULT(ans)
    CLASS(Jacobi1D_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    !! several values of x
    REAL(DFP), INTENT(IN) :: coeff(0:, 1:)
    !! recurrence coefficient
    REAL(DFP), INTENT(IN) :: scale(0:, 1:)
    !! scale coefficient
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(SIZE(x), n + 1)
  END FUNCTION Orthopol_BasisEvalGradientVector
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Aug 2022
! summary:         Set the jacobi param, alpha and beta

INTERFACE
  MODULE PURE SUBROUTINE Orthopol_SetJacobiParam(obj, alpha, beta)
    CLASS(Jacobi1D_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
  END SUBROUTINE Orthopol_SetJacobiParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Jacobi1D_Class
