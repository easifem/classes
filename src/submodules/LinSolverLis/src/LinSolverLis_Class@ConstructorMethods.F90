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
USE BaseType, ONLY: TypePrecondOpt, &
                    TypeConvergenceOpt, &
                    TypeSolverNameOpt
USE InputUtility, ONLY: Input
USE AbstractLinSolver_Class, ONLY: GetAbstractLinSolverParam, &
                                   AbstractLinSolverDeallocate
USE LinSolver_Class, ONLY: LinSolverInitiate, &
                           LinSolverDeallocate
USE Display_Method, ONLY: ToString
USE String_Class, ONLY: String

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"

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

CALL LinSolverInitiate(obj=obj, param=param)

CALL lis_solver_create(obj%lis_solver, ierr)

CALL CHKERR(ierr)

CALL GetAbstractLinSolverParam( &
  param=param, &
  prefix=myPrefix, &
  solverName=solverName, &
  preconditionOption=preconditionOption, &
  maxIter=maxIter, &
  atol=atol, &
  rtol=rtol, &
  convergenceIn=convergenceIn, &
  convergenceType=convergenceType, &
  relativeToRHS=relativeToRHS, &
  KrylovSubspaceSize=KrylovSubspaceSize, &
  scale=scale, &
  initx_zeros=initx_zeros, &
  bicgstab_ell=bicgstab_ell, &
  sor_omega=sor_omega, &
  p_name=p_name, &
  p_ilu_lfil=p_ilu_lfil, &
  p_ilu_mbloc=p_ilu_mbloc, &
  p_ilu_droptol=p_ilu_droptol, &
  p_ilu_permtol=p_ilu_permtol, &
  p_ilu_alpha=p_ilu_alpha, &
  p_ilu_fill=p_ilu_fill, &
  p_ssor_omega=p_ssor_omega, &
  p_hybrid_i=p_hybrid_i, &
  p_hybrid_maxiter=p_hybrid_maxiter, &
  p_hybrid_tol=p_hybrid_tol, &
  p_hybrid_omega=p_hybrid_omega, &
  p_hybrid_ell=p_hybrid_ell, &
  p_hybrid_restart=p_hybrid_restart, &
  p_is_alpha=p_is_alpha, &
  p_is_m=p_is_m, &
  p_sainv_drop=p_sainv_drop, &
  p_saamg_unsym=p_saamg_unsym, &
  p_saamg_theta=p_saamg_theta, &
  p_iluc_drop=p_iluc_drop, &
  p_iluc_rate=p_iluc_rate, &
  p_adds=p_adds, &
  p_adds_iter=p_adds_iter)

opt = ""

SELECT CASE (SolverName)

CASE (TypeSolverNameOpt%BICGSTABL)

  opt = opt//' -i bicgstabl -ell '//tostring(bicgstab_ell)

CASE (TypeSolverNameOpt%ORTHOMIN, TypeSolverNameOpt%GMRES, &
      TypeSolverNameOpt%FGMRES)

  opt = ' -i '//tostring(SolverName)// &
        ' -restart '//tostring(KrylovSubspaceSize)

CASE (TypeSolverNameOpt%IDRS)

  opt = ' -i '//tostring(SolverName)// &
        ' -irestart '//tostring(KrylovSubspaceSize)

CASE (TypeSolverNameOpt%SOR)
  opt = ' -i sor -omega '//tostring(sor_omega)

CASE DEFAULT
  opt = ' -i '//tostring(SolverName)

END SELECT

opt = opt//' -maxiter '//tostring(maxIter)//" -print 3 "// &
      " -scale "//tostring(scale)//' -tol '//tostring(rtol)

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

IF (preconditionOption .NE. TypePrecondOpt%NONE) THEN

  SELECT CASE (p_name)
  CASE (TypePrecondOpt%NONE)
    opt = opt//' -p none '
  CASE (TypePrecondOpt%JACOBI)
    opt = opt//' -p jacobi '
  CASE (TypePrecondOpt%ILU)
    opt = opt//' -p ilu -ilu_fill '//tostring(p_ilu_fill)
  CASE (TypePrecondOpt%SSOR)
    opt = opt//' -p ssor -ssor_omega '//tostring(p_ssor_omega)

  CASE (TypePrecondOpt%HYBRID)
    opt = opt//' -p hybrid -hybrid_i '//tostring(p_hybrid_i)// &
          ' -hybrid_maxiter '//tostring(p_hybrid_maxiter)// &
          ' -hybrid_ell '//tostring(p_hybrid_ell)// &
          ' -hybrid_restart '//tostring(p_hybrid_restart)// &
          ' -hybrid_tol '//tostring(p_hybrid_tol)// &
          ' -hybrid_omega '//tostring(p_hybrid_omega)

  CASE (TypePrecondOpt%IS)
    opt = opt//' -p is ' &
          //' -is_m '//tostring(p_is_m) &
          //' -is_alpha '//tostring(p_is_alpha)

  CASE (TypePrecondOpt%SAINV)
    opt = opt//' -p sainv -sainv_drop '//tostring(p_sainv_drop)

  CASE (TypePrecondOpt%SAAMG)
    IF (p_saamg_unsym) THEN
      opt = opt//' -p saamg -sammg_unsym true -saamg_theta ' &
            //tostring(p_saamg_theta)
    ELSE
      opt = opt//' -p saamg -sammg_unsym false -saamg_theta ' &
            //tostring(p_saamg_theta)
    END IF

  CASE (TypePrecondOpt%ILUC)
    opt = opt//' -p iluc -iluc_drop ' &
          //tostring(p_iluc_drop) &
          //' -iluc_rate ' &
          //tostring(p_iluc_rate)

  CASE (TypePrecondOpt%ADDS)
    opt = opt//' -p ilut -adds true -adds_iter ' &
          //tostring(p_adds_iter)

  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: Unknown precondition option')
    RETURN

  END SELECT

END IF

CALL lis_solver_set_option(opt%chars(), obj%lis_solver, ierr)

CALL CHKERR(ierr)

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ierr
CALL lis_solver_destroy(obj%lis_solver, ierr)
CALL CHKERR(ierr)
CALL LinSolverDeallocate(obj)
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
