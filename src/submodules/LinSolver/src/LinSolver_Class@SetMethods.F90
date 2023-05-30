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

SUBMODULE(LinSolver_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This subroutine allocates the workspace required for the linear solver
!
!# Introduction
!
! This routine allocates the workspace required for the linear solver

SUBROUTINE AllocateWorkSpace(W, IPAR, solverName, n)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: W(:)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: solverName
  INTEGER(I4B), INTENT(IN) :: n

  INTEGER(I4B) :: i, m

  SELECT CASE (solverName)
  CASE (LIS_CG, LIS_CGNR)
    i = 5 * n
  CASE (LIS_BICG)
    i = 7 * n
  CASE (LIS_DBICG)
    i = 11 * n
  CASE (LIS_BICGSTAB)
    i = 8 * n
  CASE (LIS_TFQMR)
    i = 11 * n
  CASE (LIS_ORTHOMIN, LIS_GMRES)
    m = INPUT(default=15, option=IPAR(5))
    i = (n + 3) * (m + 2) + (m + 1) * m / 2
  CASE (LIS_FGMRES)
    m = INPUT(default=15, option=IPAR(5))
    i = 2 * n * (m + 1) + (m + 1) * m / 2 + 3 * m + 2
  CASE (LIS_DQGMRES)
    m = INPUT(default=15, option=IPAR(5)) + 1
    i = n + m * (2 * n + 4)
  END SELECT
  IPAR(4) = i
  CALL Reallocate(W, i)
END SUBROUTINE AllocateWorkSpace

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Set
CHARACTER(*), PARAMETER :: myName = "ls_Set"
INTEGER(I4B) :: s(2)
INTEGER(I4B) :: solverName

s = Amat%SHAPE()
CALL obj%SetParam(Amat=Amat, &
& localNumColumn=s(2), localNumRow=s(1), &
& globalNumRow=s(1), globalNumColumn=s(2))

CALL obj%GetParam(solverName=solverName)

CALL AllocateWorkSpace( &
  & W=obj%W, &
  & n=s(1), &
  & solverName=solverName, &
  & IPAR=obj%IPAR)

END PROCEDURE ls_Set

!----------------------------------------------------------------------------
!                                                         setLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setLinSolverParam
CHARACTER(*), PARAMETER :: myName = "setLinSolverParam"

IF (.NOT. PRESENT(solverName)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'solverName should be present')
END IF

IF (solverName .EQ. LIS_SOR) THEN
  IF (.NOT. PRESENT(sor_omega)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'For solverName LIS_SOR sor_omega should be present')
  END IF
END IF

IF (solverName .EQ. LIS_BICGSTABL) THEN
  IF (.NOT. PRESENT(bicgstab_ell)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'For solverName LIS_BICGSTABL bicgstab_ell should be present')
  END IF
END IF

IF (.NOT. PRESENT(preconditionOption)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'preconditionOption should be present')
END IF

IF (preconditionOption .NE. PRECOND_NONE) THEN
  IF (.NOT. PRESENT(p_name)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'preconditionOption is active, therefore, '// &
      & 'precondition name (p_name) should be present')
  END IF
END IF

SELECT CASE (p_name)
CASE (PRECOND_ILUT)
  IF (.NOT. PRESENT(p_ilu_droptol) .OR. &
    & .NOT. PRESENT(p_ilu_lfil)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILUT (sparsekit) p_ilu_droptol and p_ilu_lfil should be present!!!')
  END IF
CASE (PRECOND_ILUTP)
  IF (.NOT. PRESENT(p_ilu_droptol) .OR. &
    & .NOT. PRESENT(p_ilu_lfil) .OR. &
    & .NOT. PRESENT(p_ilu_permtol) .OR. &
    & .NOT. PRESENT(p_ilu_mbloc)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILUTP (sparsekit) p_ilu_droptol, p_ilu_lfil, p_ilu_permtol, '// &
    & 'p_ilu_mbloc should be present!!!')
  END IF
CASE (PRECOND_ILUD)
  IF (.NOT. PRESENT(p_ilu_droptol) .OR. &
    & .NOT. PRESENT(p_ilu_alpha)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILUTP (sparsekit) p_ilu_droptol and p_ilu_alpha should be present!!!')
  END IF
CASE (PRECOND_ILUDP)
  IF (.NOT. PRESENT(p_ilu_droptol) .OR. &
    & .NOT. PRESENT(p_ilu_alpha) .OR. &
    & .NOT. PRESENT(p_ilu_permtol) .OR. &
    & .NOT. PRESENT(p_ilu_mbloc)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILUDP  (sparsekit) p_ilu_droptol, p_ilu_alpha, p_ilu_permtol, '// &
    & 'p_ilu_mbloc should be present!!!')
  END IF
CASE (PRECOND_ILUK)
  IF (.NOT. PRESENT(p_ilu_lfil)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILUK  (sparsekit) p_ilu_lfil should be present!!!')
  END IF
!
! LIS LIB
!
CASE (PRECOND_ILU)
  IF (.NOT. PRESENT(p_ilu_fill)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILU (LIS) p_ilu_fill should be present!!!')
  END IF
CASE (PRECOND_SSOR)
  IF (.NOT. PRESENT(p_ssor_omega)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_SSOR (LIS) p_ssor_omega should be present!!!')
  END IF
CASE (PRECOND_HYBRID)

  IF (.NOT. PRESENT(p_hybrid_i) .OR. &
    & .NOT. PRESENT(p_hybrid_tol) .OR. &
    & .NOT. PRESENT(p_hybrid_maxiter)  &
    &  ) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_HYBRID (LIS) p_hybrid_i p_hybrid_maxiter p_hybrid_tol'// &
    & ' should be present!!!')
  END IF

  IF (p_hybrid_i .EQ. LIS_SOR) THEN
    IF (.NOT. PRESENT(p_hybrid_omega)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
     & 'for PRECOND_HYBRID (LIS) and p_hybrid_i=LIS_SOR,  p_hybrid_omega'// &
      & ' should be present!!!')
    END IF
  END IF

  IF (p_hybrid_i .EQ. LIS_BICGSTABL) THEN
    IF (.NOT. PRESENT(p_hybrid_ell)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'for PRECOND_HYBRID (LIS) and p_hybrid_i=LIS_BICGSTABL,'// &
      & ' p_hybrid_ell should be present!!!')
    END IF
  END IF

  IF (ANY(p_hybrid_i .EQ. [LIS_GMRES, LIS_ORTHOMIN, LIS_FGMRES])) THEN
    IF (.NOT. PRESENT(p_hybrid_restart)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'for PRECOND_HYBRID (LIS), '// &
      & 'and p_hybrid_i=LIS_GMRES LIS_ORTHOMIN LIS_FGMRES, '// &
      & 'p_hybrid_restart should be present!!!')
    END IF
  END IF

CASE (PRECOND_IS)
  IF (.NOT. PRESENT(p_is_alpha) .OR. &
    & .NOT. PRESENT(p_is_m)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_IS (LIS) p_is_alpha, p_is_m should be present!!!')
  END IF

CASE (PRECOND_SAINV)
  IF (.NOT. PRESENT(p_sainv_drop)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_SAINV (LIS) p_sainv_drop should be present!!!')
  END IF

CASE (PRECOND_SAAMG)

  IF ( &
    & .NOT. PRESENT(p_saamg_theta) .OR. &
    & .NOT. PRESENT(p_saamg_unsym)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_SAAMG (LIS) p_saamg_theta, p_saamg_unsym'// &
    & 'should be present!!!')
  END IF

CASE (PRECOND_ILUC)

  IF ( &
    & .NOT. PRESENT(p_iluc_drop) .OR. &
    & .NOT. PRESENT(p_iluc_rate)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILUC (LIS) p_iluc_drop, p_iluc_rate'// &
    & 'should be present!!!')
  END IF

CASE (PRECOND_ADDS)

  IF ( &
    & .NOT. PRESENT(p_adds) .OR. &
    & .NOT. PRESENT(p_adds_iter)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ADDS (LIS) p_adds, p_adds_iter'// &
    & 'should be present!!!')
  END IF

CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for given precondition name')
END SELECT

CALL setAbstractLinSolverParam( &
& param=param, &
& prefix="LinSolver", &
& engine="NATIVE_SERIAL", &
& solverName=solverName, &
& preconditionOption=preconditionOption, &
& p_name=p_name, &
& convergenceIn=INPUT(option=convergenceIn, default=default_convergenceIn), &
& convergenceType=INPUT(option=convergenceType, default=default_convergenceType), &
& maxIter=INPUT(option=maxIter, default=default_maxIter), &
& relativeToRHS=INPUT(option=relativeToRHS, default=default_relativeToRHS), &
& KrylovSubspaceSize=INPUT(option=KrylovSubspaceSize, default=default_KrylovSubspaceSize), &
& rtol=INPUT(option=rtol, default=default_rtol), &
& atol=INPUT(option=atol, default=default_atol), &
& scale=Input(option=scale, default=default_scale), &
& initx_zeros=input(option=initx_zeros, default=default_initx_zeros), &
& bicgstab_ell=input(option=bicgstab_ell, default=default_bicgstab_ell), &
& sor_omega=input(option=sor_omega, default=default_sor_omega), &
& p_ilu_lfil=input(option=p_ilu_lfil, default=default_ilu_lfil), &
& p_ilu_mbloc=input(option=p_ilu_mbloc, default=default_ilu_mbloc), &
& p_ilu_droptol=input(option=p_ilu_droptol, default=default_ilu_droptol), &
& p_ilu_permtol=input(option=p_ilu_permtol, default=default_ilu_permtol), &
& p_ilu_alpha=input(option=p_ilu_alpha, default=default_ilu_alpha), &
& p_ilu_fill=input(option=p_ilu_fill, default=default_ilu_fill), &
& p_ssor_omega=input(option=p_ssor_omega, default=default_ssor_omega), &
& p_hybrid_i=input(option=p_hybrid_i, default=default_hybrid_i), &
& p_hybrid_maxiter=input(option=p_hybrid_maxiter,default=default_hybrid_maxiter), &
& p_hybrid_tol=input(option=p_hybrid_tol, default=default_hybrid_tol), &
& p_hybrid_omega=input(option=p_hybrid_omega, default=default_hybrid_omega), &
& p_hybrid_ell=input(option=p_hybrid_ell, default=default_hybrid_ell), &
& p_hybrid_restart=input(option=p_hybrid_restart,default=default_hybrid_restart), &
& p_is_alpha=input(option=p_is_alpha, default=default_is_alpha), &
& p_is_m=input(option=p_is_m, default=default_is_m), &
& p_sainv_drop=input(option=p_sainv_drop, default=default_sainv_drop), &
& p_saamg_unsym=input(option=p_saamg_unsym, default=default_saamg_unsym), &
& p_saamg_theta=input(option=p_saamg_theta, default=default_saamg_theta), &
& p_iluc_drop=input(option=p_iluc_drop, default=default_iluc_drop), &
& p_iluc_rate=input(option=p_iluc_rate, default=default_iluc_rate), &
& p_adds=input(option=p_adds, default=default_adds), &
& p_adds_iter=input(option=p_adds_iter, default=default_adds_iter) &
& )
END PROCEDURE setLinSolverParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
