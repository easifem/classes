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

SUBMODULE(JacobiSpace1D_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             JacobiSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiSpace1D1
CHARACTER(LEN=*), PARAMETER :: myName = "JacobiSpace1D1"
!!
IF (alpha .LE. -1.0_DFP) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'alpha should be greater than -1.0')
END IF
!!
IF (beta .LE. -1.0_DFP) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'beta should be greater than -1.0')
END IF
!!
ans%alpha = alpha
ans%beta = beta
CALL ans%setParam(domain=[-1.0_DFP, 1.0_DFP])
END PROCEDURE JacobiSpace1D1

!----------------------------------------------------------------------------
!                                                              JacobiSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiSpace1D_Pointer1
CHARACTER(LEN=*), PARAMETER :: myName = "JacobiSpace1D_Pointer1"
!!
IF (alpha .LE. -1.0_DFP) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'alpha should be greater than -1.0')
END IF
!!
IF (beta .LE. -1.0_DFP) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'beta should be greater than -1.0')
END IF
!!
ALLOCATE (ans)
ans%alpha = alpha
ans%beta = beta
CALL ans%setParam(domain=[-1.0_DFP, 1.0_DFP])
END PROCEDURE JacobiSpace1D_Pointer1

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Deallocate
CALL AbstractOrthopolSpace1DDeallocate(obj)
obj%alpha = 0.0_DFP
obj%beta = 0.0_DFP
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
ans(1) = obj%alpha
ans(2) = obj%beta
END PROCEDURE Orthopol_GetParam

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Display
CALL AbstractOrthopolSpace1DDisplay(obj=obj, msg=msg, unitno=unitno)
CALL Display(obj%alpha, "alpha=", unitno=unitno)
CALL Display(obj%beta, "beta=", unitno=unitno)
END PROCEDURE Orthopol_Display

!----------------------------------------------------------------------------
!                                                         GetRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff
CALL GetJacobiRecurrenceCoeff(n=n, alpha=obj%alpha, &
  & beta=obj%beta, alphaCoeff=ans(:, 1), betaCoeff=ans(:, 2))
END PROCEDURE Orthopol_GetRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                         GetRecurrenceCoeff2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff2
CALL GetJacobiRecurrenceCoeff2(n=n, alpha=obj%alpha, &
  & beta=obj%beta, A=ans(:, 1), B=ans(:, 2), C=ans(:, 3))
END PROCEDURE Orthopol_GetRecurrenceCoeff2

!----------------------------------------------------------------------------
!                                                                   GetAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetAlpha
ans = JacobiAlpha(n=n, alpha=obj%alpha, beta=obj%beta)
END PROCEDURE Orthopol_GetAlpha

!----------------------------------------------------------------------------
!                                                                    GetBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetBeta
ans = JacobiBeta(n=n, alpha=obj%alpha, beta=obj%beta)
END PROCEDURE Orthopol_GetBeta

!----------------------------------------------------------------------------
!                                                            GetLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetLeadingCoeff
ans = JacobiLeadingCoeff(n=n, alpha=obj%alpha, beta=obj%beta)
END PROCEDURE Orthopol_GetLeadingCoeff

!----------------------------------------------------------------------------
!                                                       GetLeadingCoeffRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetLeadingCoeffRatio
ans = JacobiLeadingCoeffRatio(n=n, alpha=obj%alpha, beta=obj%beta)
END PROCEDURE Orthopol_GetLeadingCoeffRatio

!----------------------------------------------------------------------------
!                                                                  GetNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqr
ans = JacobiNormSqr(n=n, alpha=obj%alpha, beta=obj%beta)
END PROCEDURE Orthopol_GetNormSqr

!----------------------------------------------------------------------------
!                                                                 GetNormSqr2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqr2
ans = JacobiNormSqr2(n=n, alpha=obj%alpha, beta=obj%beta)
END PROCEDURE Orthopol_GetNormSqr2

!----------------------------------------------------------------------------
!                                                             GetNormSqrRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqrRatio
ans = JacobiNormSqrRatio(n=n, alpha=obj%alpha, beta=obj%beta)
END PROCEDURE Orthopol_GetNormSqrRatio

!----------------------------------------------------------------------------
!                                                                   GetZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetZeros
ans = JacobiZeros(n=n, alpha=obj%alpha, beta=obj%beta)
END PROCEDURE Orthopol_GetZeros

!----------------------------------------------------------------------------
!                                                              GetQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetQuadrature
CALL JacobiQuadrature(n=n, alpha=obj%alpha, beta=obj%beta, pt=ans(:, 1), &
  & wt=ans(:, 2), quadType=quadType)
END PROCEDURE Orthopol_GetQuadrature

!----------------------------------------------------------------------------
!                                                                     EvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalAll1
ans = JacobiEvalAll(n=n, alpha=obj%alpha, beta=obj%beta, x=x)
END PROCEDURE Orthopol_EvalAll1

MODULE PROCEDURE Orthopol_EvalAll2
END PROCEDURE Orthopol_EvalAll2

!----------------------------------------------------------------------------
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Eval1
ans = JacobiEval(n=n, alpha=obj%alpha, beta=obj%beta, x=x)
END PROCEDURE Orthopol_Eval1

!----------------------------------------------------------------------------
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Eval2
ans = JacobiEval(n=n, alpha=obj%alpha, beta=obj%beta, x=x)
END PROCEDURE Orthopol_Eval2

!----------------------------------------------------------------------------
!                                                                    EvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalSum1
ans = JacobiEvalSum(n=n, alpha=obj%alpha, beta=obj%beta, x=x, coeff=coeff)
END PROCEDURE Orthopol_EvalSum1

!----------------------------------------------------------------------------
!                                                                    EvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalSum2
ans = JacobiEvalSum(n=n, alpha=obj%alpha, beta=obj%beta, x=x, coeff=coeff)
END PROCEDURE Orthopol_EvalSum2

!----------------------------------------------------------------------------
!                                                            GradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalAll1
ans = JacobiGradientEvalAll(n=n, alpha=obj%alpha, beta=obj%beta, x=x)
END PROCEDURE Orthopol_GradientEvalAll1

!----------------------------------------------------------------------------
!                                                            GradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalAll2
ans = JacobiGradientEvalAll(n=n, alpha=obj%alpha, beta=obj%beta, x=x)
END PROCEDURE Orthopol_GradientEvalAll2

!----------------------------------------------------------------------------
!                                                               GradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEval1
ans = JacobiGradientEval(n=n, alpha=obj%alpha, beta=obj%beta, x=x)
END PROCEDURE Orthopol_GradientEval1

!----------------------------------------------------------------------------
!                                                               GradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEval2
ans = JacobiGradientEval(n=n, alpha=obj%alpha, beta=obj%beta, x=x)
END PROCEDURE Orthopol_GradientEval2

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum1
ans = JacobiGradientEvalSum(n=n, alpha=obj%alpha, beta=obj%beta, x=x, &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientEvalSum1

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum2
ans = JacobiGradientEvalSum(n=n, alpha=obj%alpha, beta=obj%beta, x=x, &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientEvalSum2

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum3
ans = JacobiGradientEvalSum(n=n, alpha=obj%alpha, beta=obj%beta, x=x, &
  & coeff=coeff, k=k)
END PROCEDURE Orthopol_GradientEvalSum3

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum4
ans = JacobiGradientEvalSum(n=n, alpha=obj%alpha, beta=obj%beta, x=x, &
  & coeff=coeff, k=k)
END PROCEDURE Orthopol_GradientEvalSum4

!----------------------------------------------------------------------------
!                                                                  Transform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Transform1
ans = JacobiTransform(n=n, alpha=obj%alpha, beta=obj%beta, &
  & coeff=coeff, x=x, w=w, quadType=quadType)
END PROCEDURE Orthopol_Transform1

!----------------------------------------------------------------------------
!                                                                  Transform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Transform2
ans = JacobiTransform(n=n, alpha=obj%alpha, beta=obj%beta, &
  & f=f, quadType=quadType)
END PROCEDURE Orthopol_Transform2

!----------------------------------------------------------------------------
!                                                               InvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_InvTransform1
ans = JacobiInvTransform(n=n, alpha=obj%alpha, beta=obj%beta, &
  & coeff=coeff, x=x)
END PROCEDURE Orthopol_InvTransform1

!----------------------------------------------------------------------------
!                                                               InvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_InvTransform2
ans = JacobiInvTransform(n=n, alpha=obj%alpha, beta=obj%beta, &
  & coeff=coeff, x=x)
END PROCEDURE Orthopol_InvTransform2

!----------------------------------------------------------------------------
!                                                              GradientCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientCoeff
ans = JacobiGradientCoeff(n=n, alpha=obj%alpha, beta=obj%beta, &
  & coeff=coeff)
END PROCEDURE Orthopol_GradientCoeff

!----------------------------------------------------------------------------
!                                                                    DMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_DMatrix
ans = JacobiDMatrix(n=n, alpha=obj%alpha, beta=obj%beta, x=x, &
  & quadType=quadType)
END PROCEDURE Orthopol_DMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
