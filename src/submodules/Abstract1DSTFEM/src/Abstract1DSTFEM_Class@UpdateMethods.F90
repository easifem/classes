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

SUBMODULE(Abstract1DSTFEM_Class) UpdateMethods

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
USE InputUtility

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Update
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Update
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Update()"
#endif

INTEGER(I4B) :: ii, nnt
REAL(DFP) :: scale, dt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dt = obj%timeElemLength(obj%currentTimeStep)
obj%currentTime = obj%currentTime + dt
obj%currentTimeStep = obj%currentTimeStep + 1

nnt = obj%elemsdForTime%nns

IF (obj%saveErrorNorm(1)) &
  CALL RealVector_Set(obj=obj%um1, VALUE=obj%u0)
IF (obj%saveErrorNorm(2)) &
  CALL RealVector_Set(obj=obj%vm1, VALUE=obj%v0)
! IF (obj%saveErrorNorm(3)) &
!   CALL RealVector_Set(obj=obj%am1, VALUE=obj%a0)

CALL RealVector_Set(obj=obj%a0, VALUE=zero)
CALL RealVector_Set(obj=obj%v0, VALUE=zero)

CALL RealVector_Scale(obj%u0, obj%at_right)

DO ii = 1, nnt
  scale = obj%timeShapeFuncGradBndy(ii, 2)
  CALL RealVector_Add(obj1=obj%a0, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%sol, dofobj2=obj%dof, idof2=ii, scale=scale)

  scale = obj%timeShapeFuncBndy(ii, 2)
  CALL RealVector_Add(obj1=obj%v0, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%sol, dofobj2=obj%dof, idof2=ii, scale=scale)

  scale = obj%bt_right(ii) * dt
  CALL RealVector_Add(obj1=obj%u0, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%sol, dofobj2=obj%dof, idof2=ii, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Update

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE UpdateMethods
