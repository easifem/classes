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

SUBMODULE(UltrasphericalSpace1D_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     UltrasphericalSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalSpace1D1
ans%lambda = lambda
CALL ans%setParam(domain=[-1.0_DFP, 1.0_DFP])
END PROCEDURE UltrasphericalSpace1D1

!----------------------------------------------------------------------------
!                                                     UltrasphericalSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalSpace1D_Pointer1
ALLOCATE (ans)
ans%lambda = lambda
CALL ans%setParam(domain=[-1.0_DFP, 1.0_DFP])
END PROCEDURE UltrasphericalSpace1D_Pointer1

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Deallocate
CALL AbstractOrthopolSpace1DDeallocate(obj)
obj%lambda = 0.5_DFP
END PROCEDURE Orthopol_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Final
CALL obj%Deallocate()
END PROCEDURE Orthopol_Final

!----------------------------------------------------------------------------
!                                                                 GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetParam
ans = obj%lambda
END PROCEDURE Orthopol_GetParam

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Display
CALL AbstractOrthopolSpace1DDisplay(obj=obj, msg=msg, unitno=unitno)
CALL Display(obj%lambda, "lambda=", unitno=unitno)
END PROCEDURE Orthopol_Display

!----------------------------------------------------------------------------
!                                                         GetRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff
CALL GetUltrasphericalRecurrenceCoeff(n=n, lambda=obj%lambda, &
  & alphaCoeff=ans(:, 1), betaCoeff=ans(:, 2))
END PROCEDURE Orthopol_GetRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                         GetRecurrenceCoeff2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff2
CALL GetUltrasphericalRecurrenceCoeff2(n=n, lambda=obj%lambda, &
  &  A=ans(:, 1), B=ans(:, 2), C=ans(:, 3))
END PROCEDURE Orthopol_GetRecurrenceCoeff2

!----------------------------------------------------------------------------
!                                                                   GetAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetAlpha
ans = UltrasphericalAlpha(n=n, lambda=obj%lambda)
END PROCEDURE Orthopol_GetAlpha

!----------------------------------------------------------------------------
!                                                                    GetBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetBeta
ans = UltrasphericalBeta(n=n, lambda=obj%lambda)
END PROCEDURE Orthopol_GetBeta

!----------------------------------------------------------------------------
!                                                            GetLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetLeadingCoeff
ans = UltrasphericalLeadingCoeff(n=n, lambda=obj%lambda)
END PROCEDURE Orthopol_GetLeadingCoeff

!----------------------------------------------------------------------------
!                                                       GetLeadingCoeffRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetLeadingCoeffRatio
ans = UltrasphericalLeadingCoeffRatio(n=n, lambda=obj%lambda)
END PROCEDURE Orthopol_GetLeadingCoeffRatio

!----------------------------------------------------------------------------
!                                                                  GetNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqr
ans = UltrasphericalNormSqr(n=n, lambda=obj%lambda)
END PROCEDURE Orthopol_GetNormSqr

!----------------------------------------------------------------------------
!                                                                 GetNormSqr2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqr2
ans = UltrasphericalNormSqr2(n=n, lambda=obj%lambda)
END PROCEDURE Orthopol_GetNormSqr2

!----------------------------------------------------------------------------
!                                                             GetNormSqrRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqrRatio
ans = UltrasphericalNormSqrRatio(n=n, lambda=obj%lambda)
END PROCEDURE Orthopol_GetNormSqrRatio

!----------------------------------------------------------------------------
!                                                                   GetZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetZeros
ans = UltrasphericalZeros(n=n, lambda=obj%lambda)
END PROCEDURE Orthopol_GetZeros

!----------------------------------------------------------------------------
!                                                              GetQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetQuadrature
CALL UltrasphericalQuadrature(n=n, lambda=obj%lambda, pt=ans(:, 1), &
  & wt=ans(:, 2), quadType=quadType)
END PROCEDURE Orthopol_GetQuadrature

!----------------------------------------------------------------------------
!                                                                     EvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalAll1
ans = UltrasphericalEvalAll(n=n, lambda=obj%lambda, x=x)
END PROCEDURE Orthopol_EvalAll1

MODULE PROCEDURE Orthopol_EvalAll2
END PROCEDURE Orthopol_EvalAll2

!----------------------------------------------------------------------------
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Eval1
ans = UltrasphericalEval(n=n, lambda=obj%lambda, x=x)
END PROCEDURE Orthopol_Eval1

!----------------------------------------------------------------------------
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Eval2
ans = UltrasphericalEval(n=n, lambda=obj%lambda, x=x)
END PROCEDURE Orthopol_Eval2

!----------------------------------------------------------------------------
!                                                                    EvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalSum1
ans = UltrasphericalEvalSum(n=n, lambda=obj%lambda, x=x, coeff=coeff)
END PROCEDURE Orthopol_EvalSum1

!----------------------------------------------------------------------------
!                                                                    EvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalSum2
ans = UltrasphericalEvalSum(n=n, lambda=obj%lambda, x=x, coeff=coeff)
END PROCEDURE Orthopol_EvalSum2

!----------------------------------------------------------------------------
!                                                            GradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalAll1
ans = UltrasphericalGradientEvalAll(n=n, lambda=obj%lambda, x=x)
END PROCEDURE Orthopol_GradientEvalAll1

!----------------------------------------------------------------------------
!                                                            GradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalAll2
ans = UltrasphericalGradientEvalAll(n=n, lambda=obj%lambda, x=x)
END PROCEDURE Orthopol_GradientEvalAll2

!----------------------------------------------------------------------------
!                                                               GradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEval1
ans = UltrasphericalGradientEval(n=n, lambda=obj%lambda, x=x)
END PROCEDURE Orthopol_GradientEval1

!----------------------------------------------------------------------------
!                                                               GradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEval2
ans = UltrasphericalGradientEval(n=n, lambda=obj%lambda, x=x)
END PROCEDURE Orthopol_GradientEval2

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum1
ans = UltrasphericalGradientEvalSum(n=n, lambda=obj%lambda, x=x, &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientEvalSum1

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum2
ans = UltrasphericalGradientEvalSum(n=n, lambda=obj%lambda, x=x, &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientEvalSum2

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum3
ans = UltrasphericalGradientEvalSum(n=n, lambda=obj%lambda, x=x, &
  & coeff=coeff, k=k)
END PROCEDURE Orthopol_GradientEvalSum3

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum4
ans = UltrasphericalGradientEvalSum(n=n, lambda=obj%lambda, x=x, &
  & coeff=coeff, k=k)
END PROCEDURE Orthopol_GradientEvalSum4

!----------------------------------------------------------------------------
!                                                                  Transform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Transform1
ans = UltrasphericalTransform(n=n, lambda=obj%lambda, &
  & coeff=coeff, x=x, w=w, quadType=quadType)
END PROCEDURE Orthopol_Transform1

!----------------------------------------------------------------------------
!                                                                  Transform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Transform2
ans = UltrasphericalTransform(n=n, lambda=obj%lambda, &
  & f=f, quadType=quadType)
END PROCEDURE Orthopol_Transform2

!----------------------------------------------------------------------------
!                                                               InvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_InvTransform1
ans = UltrasphericalInvTransform(n=n, lambda=obj%lambda, &
  & coeff=coeff, x=x)
END PROCEDURE Orthopol_InvTransform1

!----------------------------------------------------------------------------
!                                                               InvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_InvTransform2
ans = UltrasphericalInvTransform(n=n, lambda=obj%lambda, &
  & coeff=coeff, x=x)
END PROCEDURE Orthopol_InvTransform2

!----------------------------------------------------------------------------
!                                                              GradientCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientCoeff
ans = UltrasphericalGradientCoeff(n=n, lambda=obj%lambda, &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientCoeff

!----------------------------------------------------------------------------
!                                                                    DMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_DMatrix
ans = UltrasphericalDMatrix(n=n, lambda=obj%lambda, x=x, &
  & quadType=quadType)
END PROCEDURE Orthopol_DMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
