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

SUBMODULE(AbstractOrthopolSpace1D_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Deallocate
obj%domain = [-1.0, 1.0]
END PROCEDURE Orthopol_Deallocate

!----------------------------------------------------------------------------
!                                                                SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_SetParam
IF (PRESENT(domain)) obj%domain = domain
END PROCEDURE Orthopol_SetParam

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Display
CALL Display(obj%domain, "domain=", unitno=unitno)
END PROCEDURE Orthopol_Display

!----------------------------------------------------------------------------
!                                                         GetRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff
END PROCEDURE Orthopol_GetRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                         GetRecurrenceCoeff2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff2
END PROCEDURE Orthopol_GetRecurrenceCoeff2

!----------------------------------------------------------------------------
!                                                                   GetAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetAlpha
END PROCEDURE Orthopol_GetAlpha

!----------------------------------------------------------------------------
!                                                                    GetBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetBeta
END PROCEDURE Orthopol_GetBeta

!----------------------------------------------------------------------------
!                                                            GetLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetLeadingCoeff
END PROCEDURE Orthopol_GetLeadingCoeff

!----------------------------------------------------------------------------
!                                                       GetLeadingCoeffRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetLeadingCoeffRatio
END PROCEDURE Orthopol_GetLeadingCoeffRatio

!----------------------------------------------------------------------------
!                                                                  GetNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqr
END PROCEDURE Orthopol_GetNormSqr

!----------------------------------------------------------------------------
!                                                                 GetNormSqr2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqr2
END PROCEDURE Orthopol_GetNormSqr2

!----------------------------------------------------------------------------
!                                                             GetNormSqrRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetNormSqrRatio
END PROCEDURE Orthopol_GetNormSqrRatio

!----------------------------------------------------------------------------
!                                                                   GetZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetZeros
END PROCEDURE Orthopol_GetZeros

!----------------------------------------------------------------------------
!                                                              GetQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetQuadrature
END PROCEDURE Orthopol_GetQuadrature

!----------------------------------------------------------------------------
!                                                                     EvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalAll1
END PROCEDURE Orthopol_EvalAll1

MODULE PROCEDURE Orthopol_EvalAll2
END PROCEDURE Orthopol_EvalAll2

!----------------------------------------------------------------------------
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Eval1
END PROCEDURE Orthopol_Eval1

MODULE PROCEDURE Orthopol_Eval2
END PROCEDURE Orthopol_Eval2

!----------------------------------------------------------------------------
!                                                                    EvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalSum1
END PROCEDURE Orthopol_EvalSum1

MODULE PROCEDURE Orthopol_EvalSum2
END PROCEDURE Orthopol_EvalSum2

!----------------------------------------------------------------------------
!                                                            GradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalAll1
END PROCEDURE Orthopol_GradientEvalAll1

MODULE PROCEDURE Orthopol_GradientEvalAll2
END PROCEDURE Orthopol_GradientEvalAll2

!----------------------------------------------------------------------------
!                                                               GradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEval1
END PROCEDURE Orthopol_GradientEval1

MODULE PROCEDURE Orthopol_GradientEval2
END PROCEDURE Orthopol_GradientEval2

!----------------------------------------------------------------------------
!                                                            GradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientEvalSum1
END PROCEDURE Orthopol_GradientEvalSum1

MODULE PROCEDURE Orthopol_GradientEvalSum2
END PROCEDURE Orthopol_GradientEvalSum2

MODULE PROCEDURE Orthopol_GradientEvalSum3
END PROCEDURE Orthopol_GradientEvalSum3

MODULE PROCEDURE Orthopol_GradientEvalSum4
END PROCEDURE Orthopol_GradientEvalSum4

!----------------------------------------------------------------------------
!                                                                  Transform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Transform1
END PROCEDURE Orthopol_Transform1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Transform2
END PROCEDURE Orthopol_Transform2

!----------------------------------------------------------------------------
!                                                               InvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_InvTransform1
END PROCEDURE Orthopol_InvTransform1

MODULE PROCEDURE Orthopol_InvTransform2
END PROCEDURE Orthopol_InvTransform2

!----------------------------------------------------------------------------
!                                                              GradientCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GradientCoeff
END PROCEDURE Orthopol_GradientCoeff

!----------------------------------------------------------------------------
!                                                                    DMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_DMatrix
END PROCEDURE Orthopol_DMatrix

END SUBMODULE Methods
