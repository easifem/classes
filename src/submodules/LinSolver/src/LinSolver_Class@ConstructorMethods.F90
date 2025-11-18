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
                                   AbstractLinSolverDeallocate, &
                                   AbstractLinSolverInitiate

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

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

INTEGER(I4B) :: preconditionOption0, maxIter0, convergenceIn0, &
                convergenceType0, krylovSubspaceSize0
REAL(DFP) :: rtol0, atol0
LOGICAL(LGT) :: relativeToRHS0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL AbstractLinSolverInitiate( &
  obj=obj, engine=engine, solverName=solverName, &
  preconditionOption=preconditionOption, maxIter=maxIter, atol=atol, &
  rtol=rtol, convergenceIn=convergenceIn, convergenceType=convergenceType, &
  relativeToRHS=relativeToRHS, krylovSubspaceSize=krylovSubspaceSize, &
  scale=scale, initx_zeros=initx_zeros, bicgstab_ell=bicgstab_ell, &
  sor_omega=sor_omega, p_name=p_name, p_ilu_lfil=p_ilu_lfil, &
  p_ilu_mbloc=p_ilu_mbloc, p_ilu_droptol=p_ilu_droptol, &
  p_ilu_permtol=p_ilu_permtol, p_ilu_alpha=p_ilu_alpha, &
  p_ilu_fill=p_ilu_fill, p_ssor_omega=p_ssor_omega, p_hybrid_i=p_hybrid_i, &
  p_hybrid_maxiter=p_hybrid_maxiter, p_hybrid_tol=p_hybrid_tol, &
  p_hybrid_omega=p_hybrid_omega, p_hybrid_ell=p_hybrid_ell, &
  p_hybrid_restart=p_hybrid_restart, p_is_alpha=p_is_alpha, p_is_m=p_is_m, &
  p_sainv_drop=p_sainv_drop, p_saamg_unsym=p_saamg_unsym, &
  p_saamg_theta=p_saamg_theta, p_iluc_drop=p_iluc_drop, &
  p_iluc_rate=p_iluc_rate, p_adds=p_adds, p_adds_iter=p_adds_iter)

CALL obj%SetParam(ierr=0_I4B, iter=0_I4B)

CALL obj%GetParam( &
  preconditionOption=preconditionOption0, convergenceIn=convergenceIn0, &
  convergenceType=convergenceType0, relativeToRHS=relativeToRHS0, &
  krylovSubspaceSize=krylovSubspaceSize0, maxIter=maxIter0)

CALL SetPreconditionOption(IPAR=obj%IPAR, PRECOND_TYPE=preconditionOption0)

CALL SetConvergenceType( &
  IPAR=obj%IPAR, convergenceIn=convergenceIn0, &
  convergenceType=convergenceType0, relativeToRHS=relativeToRHS0)

obj%IPAR(5) = krylovSubspaceSize0
CALL SetMaxIter(IPAR=obj%IPAR, maxIter=maxIter0)
CALL SetTolerance(fpar=obj%fpar, rtol=rtol0, atol=atol0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

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
