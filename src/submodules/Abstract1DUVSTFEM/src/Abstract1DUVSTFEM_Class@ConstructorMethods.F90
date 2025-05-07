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

SUBMODULE(Abstract1DUVSTFEM_Class) ConstructorMethods

USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

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
!                              -                     obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                   obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isConnectivity = .FALSE.
obj%baseContinuityForSpace = "H1"
obj%baseContinuityForTime = "H1"
obj%baseInterpolationForSpace = "LAGR"
obj%baseInterpolationForTime = "LAGR"
obj%verbosity = 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                              InitiateFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFields
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFields()"
#endif

INTEGER(I4B), PARAMETER :: storageFMT = DOF_FMT
CHARACTER(LEN=1), PARAMETER :: names(2) = ["u", "v"]
INTEGER(I4B) :: tnodes(2), spaceCompo(2), timeCompo(2), nrow, ncol, ii, &
                con(256), tcon, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

spaceCompo(:) = 1
timeCompo(:) = obj%totalDOFTime(timeElemNum)
tnodes(:) = obj%totalVertexDOFSpace + obj%totalEdgeDOFSpace

CALL DOF_Initiate(obj=obj%dof, tNodes=tnodes, names=names, &
            spacecompo=spacecompo, timecompo=timecompo, storageFMT=storageFMT)

nrow = DOF_SIZE(obj%dof)
ncol = nrow

CALL CSRMatrix_Initiate(obj=obj%tanmat, ncol=ncol, nrow=nrow, &
                        idof=obj%dof, jdof=obj%dof)

DO ii = 1, obj%totalSpaceElements
  CALL obj%GetConnectivity(spaceElemNum=ii, ans=con, tsize=tcon)
  DO jj = 1, tcon
    CALL CSRMatrix_SetSparsity(obj=obj%tanmat, row=con(jj), col=con(1:tcon))
  END DO
END DO

CALL CSRMatrix_SetSparsity(obj=obj%tanmat)

CALL RealVector_Initiate(obj%sol, nrow)
CALL realvector_set(obj%sol, zero)
CALL RealVector_Initiate(obj%rhs, nrow)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateFields

!----------------------------------------------------------------------------
!                                                      InitiateConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateConnectivity()"
#endif

INTEGER(I4B) :: ii, jj, icount, iedgedof, tnodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isConnectivity) RETURN

obj%isConnectivity = .TRUE.

tnodes = 0
DO ii = 1, obj%totalSpaceElements
  tnodes = tnodes + obj%totalDOFSpace(ii)
END DO

CALL Reallocate(obj%conIA, obj%totalSpaceElements + 1)
CALL Reallocate(obj%conJA, tnodes)
obj%conIA(1) = 1

icount = 1
iedgedof = obj%totalVertexDOFSpace

DO ii = 1, obj%totalSpaceElements
  obj%conJA(icount) = ii
  icount = icount + 1
  obj%conJA(icount) = ii + 1
  icount = icount + 1
  obj%conIA(ii + 1) = obj%conIA(ii) + obj%totalDOFSpace(ii)

  DO jj = 1, obj%totalDOFSpace(ii) - 2
    iedgedof = iedgedof + 1
    obj%conJA(icount) = iedgedof
    icount = icount + 1
  END DO
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateConnectivity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
