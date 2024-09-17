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

SUBMODULE(Chebyshev1Space1D_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         Chebyshev1Space1D
!----------------------------------------------------------------------------

MODULE PROCEDURE Chebyshev1Space1D1
CALL ans%setParam(domain=[-1.0_DFP, 1.0_DFP])
END PROCEDURE Chebyshev1Space1D1

!----------------------------------------------------------------------------
!                                                         Chebyshev1Space1D
!----------------------------------------------------------------------------

MODULE PROCEDURE Chebyshev1Space1D_Pointer1
ALLOCATE (ans)
CALL ans%setParam(domain=[-1.0_DFP, 1.0_DFP])
END PROCEDURE Chebyshev1Space1D_Pointer1

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Final
CALL obj%DEALLOCATE()
END PROCEDURE Orthopol_Final

!----------------------------------------------------------------------------
!                                                         GetRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff
CALL GetChebyshev1RecurrenceCoeff(n=n, alphaCoeff=ans(:, 1), betaCoeff=ans(:, 2))
END PROCEDURE Orthopol_GetRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                         GetRecurrenceCoeff2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff2
CALL GetChebyshev1RecurrenceCoeff2(n=n, A=ans(:, 1), B=ans(:, 2), C=ans(:, 3))
END PROCEDURE Orthopol_GetRecurrenceCoeff2

!----------------------------------------------------------------------------
!                                                                   GetAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetAlpha
ans = Chebyshev1Alpha(n=n)
END PROCEDURE Orthopol_GetAlpha

!----------------------------------------------------------------------------
!                                                                    GetBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetBeta
ans = Chebyshev1Beta(n=n)
END PROCEDURE Orthopol_GetBeta

!----------------------------------------------------------------------------
!                                                            GetLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetLeadingCoeff
ans = Chebyshev1LeadingCoeff(n=n)
END PROCEDURE Orthopol_GetLeadingCoeff

!----------------------------------------------------------------------------
!                                                       GetLeadingCoeffRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetLeadingCoeffRatio
ans = Chebyshev1LeadingCoeffRatio(n=n)
END PROCEDURE Orthopol_GetLeadingCoeffRatio

!----------------------------------------------------------------------------
!                                                                  GetNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqr
ans = Chebyshev1NormSqr(n=n)
END PROCEDURE Orthopol_GetNormSqr

!----------------------------------------------------------------------------
!                                                                 GetNormSqr2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqr2
ans = Chebyshev1NormSqr2(n=n)
END PROCEDURE Orthopol_GetNormSqr2

!----------------------------------------------------------------------------
!                                                             GetNormSqrRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqrRatio
ans = Chebyshev1NormSqrRatio(n=n)
END PROCEDURE Orthopol_GetNormSqrRatio

!----------------------------------------------------------------------------
!                                                                   GetZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetZeros
ans = Chebyshev1Zeros(n=n)
END PROCEDURE Orthopol_GetZeros

!----------------------------------------------------------------------------
!                                                              GetQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetQuadrature
CALL Chebyshev1Quadrature(n=n, pt=ans(:, 1), &
  & wt=ans(:, 2), quadType=quadType)
END PROCEDURE Orthopol_GetQuadrature

!----------------------------------------------------------------------------
!                                                                     EvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalAll1
ans = Chebyshev1EvalAll(n=n, x=x)
END PROCEDURE Orthopol_EvalAll1

MODULE PROCEDURE Orthopol_EvalAll2
END PROCEDURE Orthopol_EvalAll2

!----------------------------------------------------------------------------
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Eval1
ans = Chebyshev1Eval(n=n, x=x)
END PROCEDURE Orthopol_Eval1

!----------------------------------------------------------------------------
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Eval2
ans = Chebyshev1Eval(n=n, x=x)
END PROCEDURE Orthopol_Eval2

!----------------------------------------------------------------------------
!                                                                    EvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalSum1
ans = Chebyshev1EvalSum(n=n, x=x, coeff=coeff)
END PROCEDURE Orthopol_EvalSum1

!----------------------------------------------------------------------------
!                                                                    EvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalSum2
ans = Chebyshev1EvalSum(n=n, x=x, coeff=coeff)
END PROCEDURE Orthopol_EvalSum2

!----------------------------------------------------------------------------
!                                                            GradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalAll1
ans = Chebyshev1GradientEvalAll(n=n, x=x)
END PROCEDURE Orthopol_GradientEvalAll1

!----------------------------------------------------------------------------
!                                                            GradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalAll2
ans = Chebyshev1GradientEvalAll(n=n, x=x)
END PROCEDURE Orthopol_GradientEvalAll2

!----------------------------------------------------------------------------
!                                                               GradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEval1
ans = Chebyshev1GradientEval(n=n, x=x)
END PROCEDURE Orthopol_GradientEval1

!----------------------------------------------------------------------------
!                                                               GradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEval2
ans = Chebyshev1GradientEval(n=n, x=x)
END PROCEDURE Orthopol_GradientEval2

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum1
ans = Chebyshev1GradientEvalSum(n=n, x=x, &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientEvalSum1

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum2
ans = Chebyshev1GradientEvalSum(n=n, x=x, &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientEvalSum2

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum3
ans = Chebyshev1GradientEvalSum(n=n, x=x, &
  & coeff=coeff, k=k)
END PROCEDURE Orthopol_GradientEvalSum3

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum4
ans = Chebyshev1GradientEvalSum(n=n, x=x, &
  & coeff=coeff, k=k)
END PROCEDURE Orthopol_GradientEvalSum4

!----------------------------------------------------------------------------
!                                                                  Transform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Transform1
ans = Chebyshev1Transform(n=n, &
                          coeff=coeff, x=x, w=w, quadType=quadType)
END PROCEDURE Orthopol_Transform1

!----------------------------------------------------------------------------
!                                                                  Transform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Transform2
ans = Chebyshev1Transform(n=n, f=f, x1=x1, x2=x2, quadType=quadType)
END PROCEDURE Orthopol_Transform2

!----------------------------------------------------------------------------
!                                                               InvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_InvTransform1
ans = Chebyshev1InvTransform(n=n,  &
  & coeff=coeff, x=x)
END PROCEDURE Orthopol_InvTransform1

!----------------------------------------------------------------------------
!                                                               InvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_InvTransform2
ans = Chebyshev1InvTransform(n=n,  &
  & coeff=coeff, x=x)
END PROCEDURE Orthopol_InvTransform2

!----------------------------------------------------------------------------
!                                                              GradientCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientCoeff
ans = Chebyshev1GradientCoeff(n=n,  &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientCoeff

!----------------------------------------------------------------------------
!                                                                    DMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_DMatrix
ans = Chebyshev1DMatrix(n=n, x=x, &
  & quadType=quadType)
END PROCEDURE Orthopol_DMatrix
END SUBMODULE Methods
