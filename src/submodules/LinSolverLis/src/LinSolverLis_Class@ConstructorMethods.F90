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

SUBMODULE(LinSolverLis_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Initiate
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "ls_Initiate"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: solverName
INTEGER(I4B) :: preconditionOption
INTEGER(I4B) :: convergenceIn
INTEGER(I4B) :: convergenceType
INTEGER(I4B) :: maxIter
LOGICAL(LGT) :: relativeToRHS
INTEGER(I4B) :: KrylovSubspaceSize
REAL(DFP) :: rtol
REAL(DFP) :: atol
INTEGER(I4B) :: scale
! LIS, Solver digonal scaling
! scale_none: No scaling
! scale_jacobi: jacobi scaling inv(D)Ax = inv(D)b
! scale_symm_diag: sqrt(inv(D)) A sqrt(inv(D)) x = sqrt(inv(D))b
LOGICAL(LGT) :: initx_zeros
! if True, then we set sol=0.0 as initial guess.
INTEGER(I4B) :: bicgstab_ell
!
REAL(DFP) :: sor_omega
INTEGER(I4B) :: p_name
! Name of preconditioner
INTEGER(I4B) :: p_ilu_lfil
! Sparsekit, ilu
INTEGER(I4B) :: p_ilu_mbloc
! Sparsekit, ilu
REAL(DFP) :: p_ilu_droptol
! Sparsekit, ilu
REAL(DFP) :: p_ilu_permtol
! Sparsekit, ilu
REAL(DFP) :: p_ilu_alpha
! Sparsekit, ilu, alpha
INTEGER(I4B) :: p_ilu_fill
! ILU, fill-in
REAL(DFP) :: p_ssor_omega
! The relaxation coefficient omega in (0.0, 2.0)
INTEGER(I4B) :: p_hybrid_i
! Hybrid, the linear solver, for example, SSOR, GMRES,
INTEGER(I4B) :: p_hybrid_maxiter
! Hybrid, maximum number of iterations
REAL(DFP) :: p_hybrid_tol
! Hybrid, convergence tolerance
REAL(DFP) :: p_hybrid_omega
! Hybrid, The relaxation coefficient omega of the SOR
! omega should be in (0.0, 2.0)
INTEGER(I4B) :: p_hybrid_ell
!Hybrid, The degree l of the BiCGSTAB(l)
INTEGER(I4B) :: p_hybrid_restart
! Hybrid, The restart value of GMRES and Orthomin
REAL(DFP) :: p_is_alpha
! I+S, The parameter alpha of $I + \alpha {S}^{m}$
INTEGER(I4B) :: p_is_m
! I+S, The parameter m of $I + \alpha {S}^{m}$
REAL(DFP) :: p_sainv_drop
! SA-AMG, The drop criteria
LOGICAL(LGT) :: p_saamg_unsym
! SA-AMG, Select the unsymmetric version
! The matrix structure must be symmetric
REAL(DFP) :: p_saamg_theta
! SA-AMG, The drop criteria
REAL(DFP) :: p_iluc_drop
! Crout ILU, default is 0.05, The drop criteria
REAL(DFP) :: p_iluc_rate
! Crout ILU, The ratio of the maximum fill-in
LOGICAL(LGT) :: p_adds
! ilut Additive Schwarz, default is true
INTEGER(I4B) :: p_adds_iter
! default value is 1
! ILUT Additive Schwarz number of iteration
TYPE(String) :: opt

CALL LinSolverInitiate(obj, param)

CALL lis_solver_create(obj%lis_solver, ierr)
CALL chkerr(ierr)

CALL getAbstractLinSolverParam( &
  & param=param, &
  & prefix=myPrefix, &
  & solverName=solverName, &
  & preconditionOption=preconditionOption, &
  & maxIter=maxIter, &
  & atol=atol, &
  & rtol=rtol, &
  & convergenceIn=convergenceIn, &
  & convergenceType=convergenceType, &
  & relativeToRHS=relativeToRHS, &
  & KrylovSubspaceSize=KrylovSubspaceSize, &
  & scale=scale, &
  & initx_zeros=initx_zeros, &
  & bicgstab_ell=bicgstab_ell, &
  & sor_omega=sor_omega, &
  & p_name=p_name, &
  & p_ilu_lfil=p_ilu_lfil, &
  & p_ilu_mbloc=p_ilu_mbloc, &
  & p_ilu_droptol=p_ilu_droptol, &
  & p_ilu_permtol=p_ilu_permtol, &
  & p_ilu_alpha=p_ilu_alpha, &
  & p_ilu_fill=p_ilu_fill, &
  & p_ssor_omega=p_ssor_omega, &
  & p_hybrid_i=p_hybrid_i, &
  & p_hybrid_maxiter=p_hybrid_maxiter, &
  & p_hybrid_tol=p_hybrid_tol, &
  & p_hybrid_omega=p_hybrid_omega, &
  & p_hybrid_ell=p_hybrid_ell, &
  & p_hybrid_restart=p_hybrid_restart, &
  & p_is_alpha=p_is_alpha, &
  & p_is_m=p_is_m, &
  & p_sainv_drop=p_sainv_drop, &
  & p_saamg_unsym=p_saamg_unsym, &
  & p_saamg_theta=p_saamg_theta, &
  & p_iluc_drop=p_iluc_drop, &
  & p_iluc_rate=p_iluc_rate, &
  & p_adds=p_adds, &
  & p_adds_iter=p_adds_iter &
  & )

opt = ""

SELECT CASE (SolverName)

CASE (LIS_BICGSTABL)

  opt = opt//' -i bicgstabl -ell '//tostring(bicgstab_ell)

CASE (LIS_ORTHOMIN, LIS_GMRES, LIS_FGMRES)

  opt = ' -i '//tostring(SolverName)// &
      & ' -restart '//tostring(KrylovSubspaceSize)

CASE (LIS_IDRS)

  opt = ' -i '//tostring(SolverName)// &
      & ' -irestart '//tostring(KrylovSubspaceSize)

CASE (LIS_SOR)
  opt = ' -i sor -omega '//tostring(sor_omega)

CASE DEFAULT
  opt = ' -i '//tostring(SolverName)

END SELECT

opt = opt//' -maxiter '//tostring(maxIter)//" -print 3 "// &
  & " -scale "//tostring(scale)//' -tol '//tostring(rtol)

IF (initx_zeros) THEN
  opt = opt//' -initx_zeros true '
ELSE
  opt = opt//' -initx_zeros false '
END IF

IF (relativeToRHS) THEN
  opt = opt//" -conv_cond 1 "
ELSE
  opt = opt//" -conv_cond 0 "
END IF

IF (preconditionOption .NE. NO_PRECONDITION) THEN

  SELECT CASE (p_name)
  CASE (PRECOND_NONE)
    opt = opt//' -p none '
  CASE (PRECOND_JACOBI)
    opt = opt//' -p jacobi '
  CASE (PRECOND_ILU)
    opt = opt//' -p ilu -ilu_fill '//tostring(p_ilu_fill)
  CASE (PRECOND_SSOR)
    opt = opt//' -p ssor -ssor_omega '//tostring(p_ssor_omega)

  CASE (PRECOND_HYBRID)
    opt = opt//' -p hybrid -hybrid_i '//tostring(p_hybrid_i)// &
      & ' -hybrid_maxiter '//tostring(p_hybrid_maxiter)// &
      & ' -hybrid_ell '//tostring(p_hybrid_ell)// &
      & ' -hybrid_restart '//tostring(p_hybrid_restart)// &
      & ' -hybrid_tol '//tostring(p_hybrid_tol)// &
      & ' -hybrid_omega '//tostring(p_hybrid_omega)

  CASE (PRECOND_IS)
    opt = opt//' -p is ' &
      & //' -is_m '//tostring(p_is_m) &
      & //' -is_alpha '//tostring(p_is_alpha)

  CASE (PRECOND_SAINV)
    opt = opt//' -p sainv -sainv_drop '//tostring(p_sainv_drop)

  CASE (PRECOND_SAAMG)
    IF (p_saamg_unsym) THEN
      opt = opt//' -p saamg -sammg_unsym true -saamg_theta ' &
          & //tostring(p_saamg_theta)
    ELSE
      opt = opt//' -p saamg -sammg_unsym false -saamg_theta ' &
          & //tostring(p_saamg_theta)
    END IF

  CASE (PRECOND_ILUC)
    opt = opt//' -p iluc -iluc_drop ' &
        & //tostring(p_iluc_drop) &
        & //' -iluc_rate ' &
        & //tostring(p_iluc_rate)

  CASE (PRECOND_ADDS)
    opt = opt//' -p ilut -adds true -adds_iter ' &
          //tostring(p_adds_iter)

  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Unknown precondition option')

  END SELECT

END IF

CALL lis_solver_set_option(opt%chars(), obj%lis_solver, ierr)
CALL chkerr(ierr)

END PROCEDURE ls_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Deallocate
INTEGER(I4B) :: ierr
CALL lis_solver_destroy(obj%lis_solver, ierr)
CALL chkerr(ierr)
CALL LinSolverDeallocate(obj)
END PROCEDURE ls_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_final
CALL obj%DEALLOCATE()
END PROCEDURE ls_final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
