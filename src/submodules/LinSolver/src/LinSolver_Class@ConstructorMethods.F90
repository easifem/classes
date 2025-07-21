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

SUBMODULE(LinSolver_Class) ConstructorMethods
USE BaseType, ONLY: TypePrecondOpt, &
                    TypeConvergenceOpt
USE InputUtility, ONLY: Input
USE AbstractLinSolver_Class, ONLY: GetAbstractLinSolverParam, &
                                   AbstractLinSolverDeallocate

USE LinSolverOpt_Class, ONLY: TypeLinSolverOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     SetPreconditionOption
!----------------------------------------------------------------------------

SUBROUTINE SetPreconditionOption(IPAR, PRECOND_TYPE)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: PRECOND_TYPE

  SELECT CASE (PRECOND_TYPE)
  CASE (TypePrecondOpt%NONE)
    IPAR(2) = 0
  CASE (TypePrecondOpt%left)
    IPAR(2) = 1
  CASE (TypePrecondOpt%right)
    IPAR(2) = 2
  CASE (TypePrecondOpt%both)
    IPAR(2) = 3
  CASE DEFAULT
    IPAR(2) = 0
  END SELECT
END SUBROUTINE SetPreconditionOption

!----------------------------------------------------------------------------
!                                                     SetKrylovSubspaceSize
!----------------------------------------------------------------------------

SUBROUTINE SetKrylovSubspaceSize(IPAR, m)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: m
  IPAR(5) = INPUT(default=TypeLinSolverOpt%krylovSubspaceSize, option=m)
END SUBROUTINE SetKrylovSubspaceSize

!----------------------------------------------------------------------------
!                                                                 SetMatIter
!----------------------------------------------------------------------------

SUBROUTINE SetMaxIter(IPAR, maxIter)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: maxIter
  IPAR(6) = maxIter
END SUBROUTINE SetMaxIter

!----------------------------------------------------------------------------
!                                                          SetConvergenceType
!----------------------------------------------------------------------------

SUBROUTINE SetConvergenceType(IPAR, convergenceIn, convergenceType, &
                              relativeToRHS)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: convergenceIn
  INTEGER(I4B), INTENT(IN) :: convergenceType
  LOGICAL(LGT), INTENT(IN) :: relativeToRHS

  IPAR(3) = 1
  SELECT CASE (convergenceType)
  CASE (TypeConvergenceOpt%absolute)

    IF (convergenceIn .EQ. TypeConvergenceOpt%sol) THEN
      IPAR(3) = -1
    ELSE IF (convergenceIn .EQ. TypeConvergenceOpt%res) THEN
      IPAR(3) = 1
    END IF

  CASE (TypeConvergenceOpt%relative)

    IF (convergenceIn .EQ. TypeConvergenceOpt%sol) THEN
      IF (relativeToRHS) THEN
        IPAR(3) = -2
      ELSE
        IPAR(3) = -1
      END IF

    ELSE IF (convergenceIn .EQ. TypeConvergenceOpt%res) THEN

      IF (relativeToRHS) THEN
        IPAR(3) = 2
      ELSE
        IPAR(3) = 1
      END IF

    END IF

  END SELECT
END SUBROUTINE SetConvergenceType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetTolerance(FPAR, atol, rtol)
  REAL(DFP), INTENT(INOUT) :: fpar(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: atol
  REAL(DFP), OPTIONAL, INTENT(IN) :: rtol

  IF (PRESENT(atol)) FPAR(2) = atol
  IF (PRESENT(rtol)) FPAR(1) = rtol
END SUBROUTINE SetTolerance

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
INTEGER(I4B) :: solverName, preconditionOption, convergenceIn, &
                convergenceType, maxIter, KrylovSubspaceSize
REAL(DFP) :: rtol, atol
LOGICAL(LGT) :: relativeToRHS

CALL obj%DEALLOCATE()

CALL obj%CheckEssentialParam(param)

CALL GetAbstractLinSolverParam( &
  param=param, &
  prefix=myprefix, &
  solverName=solverName, &
  preconditionOption=preconditionOption, &
  convergenceIn=convergenceIn, &
  convergenceType=convergenceType, &
  maxIter=maxIter, &
  relativeToRHS=relativeToRHS, &
  KrylovSubspaceSize=KrylovSubspaceSize, &
  rtol=rtol, &
  atol=atol)

CALL obj%SetParam( &
  isInitiated=.TRUE., &
  engine="NATIVE_SERIAL", &
  ierr=0_I4B, &
  iter=0_I4B, &
  solverName=solverName, &
  preconditionOption=preconditionOption, &
  convergenceIn=convergenceIn, &
  convergenceType=convergenceType, &
  maxIter=maxIter, &
  relativeToRHS=relativeToRHS, &
  KrylovSubspaceSize=KrylovSubspaceSize, &
  atol=atol, &
  rtol=rtol)

obj%IPAR = 0
CALL SetPreconditionOption(obj%IPAR, preconditionOption)
CALL SetConvergenceType(obj%IPAR, convergenceIn, convergenceType, &
                        relativeToRHS)

obj%IPAR(5) = KrylovSubspaceSize

CALL SetMaxIter(obj%IPAR, maxIter)

obj%FPAR = 0.0_DFP

CALL SetTolerance(fpar=obj%fpar, rtol=rtol, atol=atol)
! CALL Reallocate(obj%RES, maxIter)
! CALL Reallocate(obj%dbcIndx, 0)

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL AbstractLinSolverDeallocate(obj)
obj%ipar = 0
obj%fpar = 0.0_DFP
IF (ALLOCATED(obj%w)) DEALLOCATE (obj%w)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_final
CALL obj%DEALLOCATE()
END PROCEDURE obj_final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
