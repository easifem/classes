
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

!> author: Vikas Sharma, Ph. D.
! date: 5 Aug 2022
! summary: Space of Ultraspherical polynomial is defined

MODULE UltrasphericalSpace1D_Class
USE BaseType, ONLY: iface_1DFunction
USE String_Class, ONLY: String
USE GlobalData
USE AbstractOrthopolSpace1D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                     UltrasphericalSpace1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Ultraspherical orthogonal Basis are defined
!
!{!pages/UltrasphericalSpace1D_.md!}

TYPE, EXTENDS(AbstractOrthopolSpace1D_) :: UltrasphericalSpace1D_
  PRIVATE
  REAL(DFP) :: lambda = 0.5_DFP
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => Orthopol_Deallocate
    !! Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => Orthopol_GetParam
    !! Returns alpha and beta
  PROCEDURE, PUBLIC, PASS(obj) :: Display => Orthopol_Display
    !! Display the content of Ultraspherical polynomial
  FINAL :: Orthopol_Final
  !!
  !!
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: GetRecurrenceCoeff => &
    & Orthopol_GetRecurrenceCoeff
  !! returns the recurrence coefficients for monic polynomials
  PROCEDURE, PUBLIC, PASS(obj) :: GetRecurrenceCoeff2 => &
    & Orthopol_GetRecurrenceCoeff2
  !! returns the recurrence coefficients for general case
  !! $P_{n+1}=(a_{n}x+b_{n}) P_{n}-c_{n}P_{n-1}$
  PROCEDURE, PUBLIC, PASS(obj) :: GetAlpha => &
    & Orthopol_GetAlpha
  !! Returns $\alpha_{n}$ for monic case
  PROCEDURE, PUBLIC, PASS(obj) :: GetBeta => &
    & Orthopol_GetBeta
  !! Returns $\beta_{n}$ for monic case
  PROCEDURE, PUBLIC, PASS(obj) :: GetLeadingCoeff => &
    & Orthopol_GetLeadingCoeff
  !! Returns the leading coefficient
  PROCEDURE, PUBLIC, PASS(obj) :: GetLeadingCoeffRatio => &
    & Orthopol_GetLeadingCoeffRatio
  !! Ratio of l!!
  PROCEDURE, PUBLIC, PASS(obj) :: GetNormSqr => &
    & Orthopol_GetNormSqr
  !! Returns the square of norm
  PROCEDURE, PUBLIC, PASS(obj) :: GetNormSqr2 => &
    & Orthopol_GetNormSqr2
  !! Returns the square of norm from order 0 to n
  PROCEDURE, PUBLIC, PASS(obj) :: GetNormSqrRatio => &
    & Orthopol_GetNormSqrRatio
  !! Returns the ratio of square of norm
  PROCEDURE, PUBLIC, PASS(obj) :: GetZeros => &
    & Orthopol_GetZeros
  !! Zeros of Ultraspherical orthogonal polynomials
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadrature => &
    & Orthopol_GetQuadrature
  !! Quadrature points
  PROCEDURE, PRIVATE, PASS(obj) :: EvalAll1 => &
    & Orthopol_EvalAll1
  !! Evaluate polynomials of from order 0 to N at single point
  PROCEDURE, PRIVATE, PASS(obj) :: EvalAll2 => &
    & Orthopol_EvalAll2
  !! Evaluate polynomials of from order 0 to N at several point
  PROCEDURE, PRIVATE, PASS(obj) :: Eval1 => &
    & Orthopol_Eval1
  !! Evaluate polynomials of order n<N at single point
  PROCEDURE, PRIVATE, PASS(obj) :: Eval2 => &
    & Orthopol_Eval2
  !! Evaluate polynomials of order n<N at several points
  PROCEDURE, PRIVATE, PASS(obj) :: EvalSum1 => Orthopol_EvalSum1
  !! Evaluate the series or orthogonal expansion
  PROCEDURE, PRIVATE, PASS(obj) :: EvalSum2 => Orthopol_EvalSum2
  !! Evaluate the series or orthogonal expansion
  PROCEDURE, PRIVATE, PASS(obj) :: GradientEvalAll1 => &
    & Orthopol_GradientEvalAll1
  !! Evaluate Gradient of orthopol order n<N at a single point
  PROCEDURE, PRIVATE, PASS(obj) :: GradientEvalAll2 => &
    & Orthopol_GradientEvalAll2
  !! Evaluate Gradient of orthopol order n<N at several points
  PROCEDURE, PRIVATE, PASS(obj) :: GradientEval1 => &
    & Orthopol_GradientEval1
  !! Evaluate Gradient of orthopol order n<N at a single point
  PROCEDURE, PRIVATE, PASS(obj) :: GradientEval2 => &
    & Orthopol_GradientEval2
  !! Evaluate Gradient of orthopol order n<N at several points
  PROCEDURE, PRIVATE, PASS(obj) :: GradientEvalSum1 => &
    & Orthopol_GradientEvalSum1
  !! Evaluate the gradient of series of orthopol
  PROCEDURE, PRIVATE, PASS(obj) :: GradientEvalSum2 => &
    & Orthopol_GradientEvalSum2
  !! Evaluate the gradient of series of orthopol
  PROCEDURE, PRIVATE, PASS(obj) :: GradientEvalSum3 => &
    & Orthopol_GradientEvalSum3
  !! Evaluate the kth gradient of series of orthopol
  PROCEDURE, PRIVATE, PASS(obj) :: GradientEvalSum4 => &
    & Orthopol_GradientEvalSum4
  !! Evaluate the kth gradient of series of orthopol
  PROCEDURE, PRIVATE, PASS(obj) :: Transform1 => Orthopol_Transform1
  !! Orthogonal transform
  PROCEDURE, PRIVATE, PASS(obj) :: Transform2 => Orthopol_Transform2
  !! Orthogonal transform
  PROCEDURE, PRIVATE, PASS(obj) :: InvTransform1 => Orthopol_InvTransform1
  !! Inverse orthogonal transform
  PROCEDURE, PRIVATE, PASS(obj) :: InvTransform2 => Orthopol_InvTransform2
  !! Inverse orthogonal transform
  PROCEDURE, PRIVATE, PASS(obj) :: GradientCoeff => Orthopol_GradientCoeff
  !! Coefficient of Gradient
  PROCEDURE, PRIVATE, PASS(obj) :: DMatrix => Orthopol_DMatrix
  !! Differentiation matrix
END TYPE UltrasphericalSpace1D_

PUBLIC :: UltrasphericalSpace1D_

!----------------------------------------------------------------------------
!                                              UltrasphericalSpace1DPointer_
!----------------------------------------------------------------------------

TYPE :: UltrasphericalSpace1DPointer_
  CLASS(UltrasphericalSpace1D_), POINTER :: ptr => NULL()
END TYPE UltrasphericalSpace1DPointer_

PUBLIC :: UltrasphericalSpace1DPointer_

!----------------------------------------------------------------------------
!                                              UltrasphericalSpace1D@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Constructor for space of Ultraspherical polynomials

INTERFACE
  MODULE FUNCTION UltrasphericalSpace1D1(lambda) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda > -0.5
    TYPE(UltrasphericalSpace1D_) :: ans
    !! Chebyshev Basis of first kind
  END FUNCTION UltrasphericalSpace1D1
END INTERFACE

INTERFACE UltrasphericalSpace1D
  MODULE PROCEDURE UltrasphericalSpace1D1
END INTERFACE UltrasphericalSpace1D

PUBLIC :: UltrasphericalSpace1D

!----------------------------------------------------------------------------
!                                      UltrasphericalSpace1D_Pointer@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Constructor for space of Ultraspherical polynomials

INTERFACE
  MODULE FUNCTION UltrasphericalSpace1D_Pointer1(lambda) RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda > 0.5
    CLASS(UltrasphericalSpace1D_), POINTER :: ans
    !! Ultraspherical polynomial of order = 0 to n
  END FUNCTION UltrasphericalSpace1D_Pointer1
END INTERFACE

INTERFACE UltrasphericalSpace1D_Pointer
  MODULE PROCEDURE UltrasphericalSpace1D_Pointer1
END INTERFACE UltrasphericalSpace1D_Pointer

PUBLIC :: UltrasphericalSpace1D_Pointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary:  Deallocate the object

INTERFACE
  MODULE SUBROUTINE Orthopol_Deallocate(obj)
    CLASS(UltrasphericalSpace1D_), INTENT(INOUT) :: obj
  END SUBROUTINE Orthopol_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Final@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Finalizer for chebyshev space

INTERFACE
  MODULE SUBROUTINE Orthopol_Final(obj)
    TYPE(UltrasphericalSpace1D_), INTENT(INOUT) :: obj
  END SUBROUTINE Orthopol_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the parameter

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetParam(obj) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    REAL(DFP) :: ans
      !! lambda
  END FUNCTION Orthopol_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE Orthopol_Display(obj, msg, unitno)
    CLASS(UltrasphericalSpace1D_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE Orthopol_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetRecurrenceCoeff@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the recurrence coefficient of monic polynomials

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetRecurrenceCoeff(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(0:n - 1, 1:2)
    !! ans(:,1) = alpha
    !! ans(:,2) = beta
  END FUNCTION Orthopol_GetRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetRecurrenceCoeff2@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the recurrence coefficient of monic polynomials

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetRecurrenceCoeff2(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(0:n - 1, 1:3)
    !! ans(:,1) = a,
    !! ans(:,2) = b
    !! ans(:,3) = c
  END FUNCTION Orthopol_GetRecurrenceCoeff2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetAlpha@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the recurrence coefficient alpha of monic polynomials

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetAlpha(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GetAlpha
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetBeta@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the recurrence coefficient beta of monic polynomials

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetBeta(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GetBeta
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetLeadingCoeff@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the leading coeff of ultraspherical

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetLeadingCoeff(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GetLeadingCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetLeadingCoeffRatio@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the ration of leading coeff of ultraspherical (n+1)/n

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetLeadingCoeffRatio(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GetLeadingCoeffRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNormSqr@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the ration of leading coeff of ultraspherical (n+1)/n

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetNormSqr(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GetNormSqr
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNormSqr2@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the ratio of leading coeff of ultraspherical (n+1)/n

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetNormSqr2(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(0:n)
  END FUNCTION Orthopol_GetNormSqr2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNormSqrRatio@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the ratio of square of norm of ultraspherical (n+1)/n

INTERFACE
  MODULE PURE FUNCTION Orthopol_GetNormSqrRatio(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GetNormSqrRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetZeros@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the zeros of ultraspherical polynomial

INTERFACE
  MODULE FUNCTION Orthopol_GetZeros(obj, n) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n)
  END FUNCTION Orthopol_GetZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetQuadrature@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Returns the zeros of ultraspherical polynomial

INTERFACE
  MODULE FUNCTION Orthopol_GetQuadrature(obj, n, quadType) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! number of points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Gauss, GaussRadauLeft, GaussRadauRight, GaussLobatto
    REAL(DFP) :: ans(n, 2)
    !! ans(:,1) = points
    !! ans(:,2) = weights
  END FUNCTION Orthopol_GetQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                          EvalAll@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the  ultraspherical polynomial (0 to n) at single point.

INTERFACE
  MODULE PURE FUNCTION Orthopol_EvalAll1(obj, n, x) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP) :: ans(n + 1)
  END FUNCTION Orthopol_EvalAll1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          EvalAll@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the ultraspherical polynomial (0 to n) at several points.

INTERFACE
  MODULE PURE FUNCTION Orthopol_EvalAll2(obj, n, x) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate orthopol of order = 0 to n (total n+1) at point x
  END FUNCTION Orthopol_EvalAll2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Eval@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the ultraspherical polynomial (order = n ) at single point.

INTERFACE
  MODULE PURE FUNCTION Orthopol_Eval1(obj, n, x) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP) :: ans
  END FUNCTION Orthopol_Eval1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Eval@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the orthopol (order=n) at several points.

INTERFACE
  MODULE PURE FUNCTION Orthopol_Eval2(obj, n, x) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate orthopol of order = n at several points
  END FUNCTION Orthopol_Eval2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          EvalSum@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the orthopol series at single point.

INTERFACE
  MODULE PURE FUNCTION Orthopol_EvalSum1(obj, n, x, coeff) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum
    REAL(DFP) :: ans
  END FUNCTION Orthopol_EvalSum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          EvalSum@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the orthopol series at several points.

INTERFACE
  MODULE PURE FUNCTION Orthopol_EvalSum2(obj, n, x, coeff) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION Orthopol_EvalSum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GradientEvalAll@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the gradient of orthopol (0 to n) at single point.

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientEvalAll1(obj, n, x) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP) :: ans(n + 1)
  END FUNCTION Orthopol_GradientEvalAll1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GradientEvalAll@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the gradient of orthopol (0 to n) at several points.

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientEvalAll2(obj, n, x) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate orthopol of order = 0 to n (total n+1) at point x
  END FUNCTION Orthopol_GradientEvalAll2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GradientEval@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the gradient of Ultraspherical (order = n ) at single point.

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientEval1(obj, n, x) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GradientEval1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GradientEval@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the gradient of Ultraspherical (order=n) at several points.

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientEval2(obj, n, x) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate orthopol of order = n at several points
  END FUNCTION Orthopol_GradientEval2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GradientEvalSum@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the gradient of Ultraspherical series at single point.

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientEvalSum1(obj, n, x, coeff) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GradientEvalSum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GradientEvalSum@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the gradient of Ultraspherical series at several points.

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientEvalSum2(obj, n, x, coeff) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION Orthopol_GradientEvalSum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GradientEvalSum@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the gradient of Ultraspherical series at single point.

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientEvalSum3(obj, n, x, coeff, k) &
    & RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum
    INTEGER(I4B), INTENT(IN) :: k
    !! order of derivative
    REAL(DFP) :: ans
  END FUNCTION Orthopol_GradientEvalSum3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GradientEvalSum@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 Oct 2022
! summary: Evaluates the gradient of Ultraspherical series at several points.

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientEvalSum4(obj, n, x, coeff, k) &
    & RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of orthopol
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum
    INTEGER(I4B), INTENT(IN) :: k
    !! order of derivative
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION Orthopol_GradientEvalSum4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Transform@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Ultraspherical Transform

INTERFACE
  MODULE PURE FUNCTION Orthopol_Transform1(obj, n, coeff, x, w, quadType) &
    & RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! nodal value (at quad points)
    REAL(DFP), INTENT(IN) :: x(0:n)
    !! quadrature points
    REAL(DFP), INTENT(IN) :: w(0:n)
    !! weights
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type
    !! Gauss, GaussLobatto, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION Orthopol_Transform1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Transform@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Ultraspherical Transform of a function

INTERFACE
  MODULE FUNCTION Orthopol_Transform2(obj, n, f, quadType) &
    & RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    !! orthopol
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION Orthopol_Transform2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      InvTransform@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Ultraspherical Inverse Transform

INTERFACE
  MODULE PURE FUNCTION Orthopol_InvTransform1(obj, n, coeff, x) &
    & RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    !! orthopol
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x
    !! x point in physical space
    REAL(DFP) :: ans
    !! value in physical space
  END FUNCTION Orthopol_InvTransform1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      InvTransform@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Ultraspherical Inverse Transform

INTERFACE
  MODULE PURE FUNCTION Orthopol_InvTransform2(obj, n, coeff, x) &
    & RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    !! orthopol
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x(:)
    !! x point in physical space
    REAL(DFP) :: ans(SIZE(x))
    !! value in physical space
  END FUNCTION Orthopol_InvTransform2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GradientCoeff@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Returns coefficient for gradient of Ultraspherical expansion

INTERFACE
  MODULE PURE FUNCTION Orthopol_GradientCoeff(obj, n, coeff) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    !! orthopol
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! coefficients $\tilde{u}_{n}$ obtained from UltrasphericalTransform
    REAL(DFP) :: ans(0:n)
    !! coefficient of gradient
  END FUNCTION Orthopol_GradientCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                            DMatrix@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Returns coefficient for gradient of Ultraspherical expansion

INTERFACE
  MODULE PURE FUNCTION Orthopol_DMatrix(obj, n, x, quadType) RESULT(ans)
    CLASS(UltrasphericalSpace1D_), INTENT(IN) :: obj
    !! orthpol
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
    REAL(DFP) :: ans(0:n, 0:n)
      !! D matrix
  END FUNCTION Orthopol_DMatrix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UltrasphericalSpace1D_Class
