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
USE BaseType, ONLY: TypePrecondOpt, TypeConvergenceOpt
USE InputUtility, ONLY: Input
USE AbstractLinSolver_Class, ONLY: AbstractLinSolverDeallocate, &
                                   AbstractLinSolverInitiate

USE LinSolverOpt_Class, ONLY: TypeLinSolverOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       SetPreconditionOption
!----------------------------------------------------------------------------

SUBROUTINE SetPreconditionOption(ipar, PRECOND_TYPE)
  INTEGER(I4B), INTENT(INOUT) :: ipar(:)
  INTEGER(I4B), INTENT(IN) :: PRECOND_TYPE

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetPreconditionOption()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (PRECOND_TYPE)
  CASE (TypePrecondOpt%NONE)
    ipar(2) = 0
  CASE (TypePrecondOpt%left)
    ipar(2) = 1
  CASE (TypePrecondOpt%right)
    ipar(2) = 2
  CASE (TypePrecondOpt%both)
    ipar(2) = 3
  CASE DEFAULT
    ipar(2) = 0
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetPreconditionOption

!----------------------------------------------------------------------------
!                                                       SetKrylovSubspaceSize
!----------------------------------------------------------------------------

SUBROUTINE SetKrylovSubspaceSize(ipar, m)
  INTEGER(I4B), INTENT(INOUT) :: ipar(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: m

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetKrylovSubspaceSize()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ipar(5) = Input(default=TypeLinSolverOpt%krylovSubspaceSize, option=m)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetKrylovSubspaceSize

!----------------------------------------------------------------------------
!                                                                 SetMatIter
!----------------------------------------------------------------------------

SUBROUTINE SetMaxIter(ipar, maxIter)
  INTEGER(I4B), INTENT(INOUT) :: ipar(:)
  INTEGER(I4B), INTENT(IN) :: maxIter

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetMaxIter()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ipar(6) = maxIter

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetMaxIter

!----------------------------------------------------------------------------
!                                                          SetConvergenceType
!----------------------------------------------------------------------------

SUBROUTINE SetConvergenceType(ipar, convergenceIn, convergenceType, &
                              relativeToRHS)
  INTEGER(I4B), INTENT(INOUT) :: ipar(:)
  INTEGER(I4B), INTENT(IN) :: convergenceIn
  INTEGER(I4B), INTENT(IN) :: convergenceType
  LOGICAL(LGT), INTENT(IN) :: relativeToRHS

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetConvergenceType()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ipar(3) = 1

  SELECT CASE (convergenceType)
  CASE (TypeConvergenceOpt%absolute)

    IF (convergenceIn .EQ. TypeConvergenceOpt%sol) THEN
      ipar(3) = -1
    ELSE IF (convergenceIn .EQ. TypeConvergenceOpt%res) THEN
      ipar(3) = 1
    END IF

  CASE (TypeConvergenceOpt%relative)

    IF (convergenceIn .EQ. TypeConvergenceOpt%sol) THEN
      IF (relativeToRHS) THEN
        ipar(3) = -2
      ELSE
        ipar(3) = -1
      END IF

    ELSE IF (convergenceIn .EQ. TypeConvergenceOpt%res) THEN

      IF (relativeToRHS) THEN
        ipar(3) = 2
      ELSE
        ipar(3) = 1
      END IF

    END IF

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetConvergenceType

!----------------------------------------------------------------------------
!                                                                SetTolerance
!----------------------------------------------------------------------------

SUBROUTINE SetTolerance(fpar, atol, rtol)
  REAL(DFP), INTENT(INOUT) :: fpar(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: atol
  REAL(DFP), OPTIONAL, INTENT(IN) :: rtol

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetTolerance()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (PRESENT(atol)) fpar(2) = atol
  IF (PRESENT(rtol)) fpar(1) = rtol

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetTolerance

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

INTEGER(I4B) :: preconditionOption0, maxIter0, convergenceIn0, &
                convergenceType0, krylovSubspaceSize0
REAL(DFP) :: rtol0, atol0
LOGICAL(LGT) :: relativeToRHS0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractLinSolverInitiate(obj=obj)

CALL obj%GetParam( &
  preconditionOption=preconditionOption0, convergenceIn=convergenceIn0, &
  convergenceType=convergenceType0, relativeToRHS=relativeToRHS0, &
  krylovSubspaceSize=krylovSubspaceSize0, maxIter=maxIter0, rtol=rtol0, &
  atol=atol0)

CALL SetPreconditionOption(ipar=obj%ipar, PRECOND_TYPE=preconditionOption0)

CALL SetConvergenceType( &
  ipar=obj%ipar, convergenceIn=convergenceIn0, &
  convergenceType=convergenceType0, relativeToRHS=relativeToRHS0)

obj%ipar(5) = krylovSubspaceSize0
CALL SetMaxIter(ipar=obj%ipar, maxIter=maxIter0)
CALL SetTolerance(fpar=obj%fpar, rtol=rtol0, atol=atol0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractLinSolverDeallocate(obj)
obj%ipar = 0
obj%fpar = 0.0_DFP
IF (ALLOCATED(obj%w)) DEALLOCATE (obj%w)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                       Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_final
CALL obj%DEALLOCATE()
END PROCEDURE obj_final

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
