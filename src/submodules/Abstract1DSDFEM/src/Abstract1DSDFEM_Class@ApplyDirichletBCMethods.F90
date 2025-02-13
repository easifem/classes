! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(Abstract1DSDFEM_Class) ApplyDirichletBCMethods

USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

USE GlobalData, ONLY: stdout, &
                      CHAR_LF, &
                      DOF_FMT, &
                      NONE, &
                      LIS_GMRES, &
                      CHAR_SLASH

USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToInteger, &
                                    BaseType_ToInteger, &
                                    BaseType_ToChar, &
                                    BaseInterpolation_ToChar

USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_

USE ReallocateUtility, ONLY: Reallocate

USE ProductUtility, ONLY: OuterProd_, OTimesTilda

USE BaseType, ONLY: elem => TypeElemNameOpt

USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate, &
                                  Quad_Size => Size, &
                                  Quad_Display => Display

USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                OrthogonalElemShapeData

USE SwapUtility, ONLY: SWAP

USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate, &
                            CSRMatrix_Add => Add, &
                            CSRMatrix_GetSubMatrix => GetSubMatrix, &
                            CSRMatrix_Display => Display, &
                            CSRMatrix_Size => Size, &
                            CSRMatrix_SetSparsity => SetSparsity, &
                            CSRMatrix_ApplyDBC => ApplyDBC, &
                            CSRMatrix_Set => Set, &
                            CSRMatrix_Matvec => Matvec, &
                            CSRMatrix_LinSolve => CSRMatrix_GMRES, &
                            CSRMatrixLinSolveInitiate

USE DOF_Method, ONLY: DOF_Initiate => Initiate, &
                      DOF_SIZE => Size, &
                      DOF_GetIndex_ => GetIndex_, &
                      DOF_GetNodeLoc => GetNodeLoc

USE RealVector_Method, ONLY: RealVector_Initiate => Initiate, &
                             RealVector_Add => Add, &
                             RealVector_GetValue_ => GetValue_, &
                             RealVector_Set => Set, &
                             RealVector_Display => Display, &
                             RealVector_Scale => SCAL

USE LagrangePolynomialUtility, ONLY: InterpolationPoint_

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC()"
#endif

LOGICAL(LGT) :: isDirichletLeft, isDirichletRight
INTEGER(I4B) :: tsize, tsize_dbc_value
REAL(DFP) :: args(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isDirichletLeft = ASSOCIATED(obj%displacementLeft)
isDirichletRight = ASSOCIATED(obj%displacementRight)
tsize_dbc_value = 0

args = obj%currentTime + obj%timeStepSize(obj%currentTimeStep)

IF (isDirichletLeft) THEN
  CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof, tsize=tsize, &
                     nodenum=1)
  CALL obj%displacementLeft%Get(val=obj%dbc_coeff(1), args=args)
  tsize_dbc_value = tsize_dbc_value + 1
END IF

IF (isDirichletRight) THEN
  CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof(tsize_dbc_value + 1:), &
                     tsize=tsize, &
                     nodenum=obj%totalSpaceNodes)
  CALL obj%displacementRight%Get(val=obj%dbc_coeff(2), args=args)
  tsize_dbc_value = tsize_dbc_value + 1
END IF

IF (tsize_dbc_value .NE. 0) THEN
  CALL CSRMatrix_GetSubMatrix(obj=obj%tanmat, &
                              cols=obj%dbc_idof(1:tsize_dbc_value), &
                              submat=obj%submat, subIndices=obj%subIndices)

  CALL CSRMatrix_ApplyDBC(obj=obj%tanmat, &
                          dbcptrs=obj%dbc_idof(1:tsize_dbc_value))

  CALL RealVector_Set(obj=obj%sol, VALUE=0.0_DFP)
  CALL RealVector_Set(obj=obj%sol, nodenum=obj%dbc_idof(1:tsize_dbc_value), &
                      VALUE=obj%dbc_coeff(1:tsize_dbc_value))

  CALL CSRMatrix_Matvec(obj=obj%submat, x=obj%sol, y=obj%rhs, &
                        scale=minus_one, addContribution=.TRUE.)

  CALL RealVector_Set(obj=obj%rhs, nodenum=obj%dbc_idof(1:tsize_dbc_value), &
                      VALUE=obj%dbc_coeff(1:tsize_dbc_value))

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ApplyDirichletBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ApplyDirichletBCMethods
