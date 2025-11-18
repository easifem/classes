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
                    TypeSolverNameOpt
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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

INTEGER(I4B) :: ierr, solverName0, preconditionOption0, convergenceIn0, &
                convergenceType0, maxIter0, krylovSubspaceSize0
LOGICAL(LGT) :: relativeToRHS0
REAL(DFP) :: rtol0, atol0
INTEGER(I4B) :: scale0
! LIS, Solver digonal scaling
! scale_none: No scaling
! scale_jacobi: jacobi scaling inv(D)Ax = inv(D)b
! scale_symm_diag: sqrt(inv(D)) A sqrt(inv(D)) x = sqrt(inv(D))b
INTEGER(I4B) :: bicgstab_ell0
INTEGER(I4B) :: p_name0
! Name of preconditioner
INTEGER(I4B) :: p_ilu_lfil0
! Sparsekit, ilu
INTEGER(I4B) :: p_ilu_mbloc0
! Sparsekit, ilu
INTEGER(I4B) :: p_ilu_fill0
! ILU, fill-in
INTEGER(I4B) :: p_hybrid_i0
! Hybrid, the linear solver, for example, SSOR, GMRES,
INTEGER(I4B) :: p_hybrid_maxiter0
! Hybrid, maximum number of iterations
INTEGER(I4B) :: p_hybrid_ell0
!Hybrid, The degree l of the BiCGSTAB(l)
INTEGER(I4B) :: p_hybrid_restart0
! Hybrid, The restart value of GMRES and Orthomin
INTEGER(I4B) :: p_is_m0
! I+S, The parameter m of $I + \alpha {S}^{m}$
INTEGER(I4B) :: p_adds_iter0
! default value is 1
! ILUT Additive Schwarz number of iteration
REAL(DFP) :: sor_omega0
REAL(DFP) :: p_ilu_droptol0
! Sparsekit, ilu
REAL(DFP) :: p_ilu_permtol0
! Sparsekit, ilu
REAL(DFP) :: p_ilu_alpha0
! Sparsekit, ilu, alpha
REAL(DFP) :: p_ssor_omega0
! The relaxation coefficient omega in (0.0, 2.0)
REAL(DFP) :: p_hybrid_tol0
! Hybrid, convergence tolerance
REAL(DFP) :: p_hybrid_omega0
! Hybrid, The relaxation coefficient omega of the SOR
! omega should be in (0.0, 2.0)
REAL(DFP) :: p_is_alpha0
! I+S, The parameter alpha of $I + \alpha {S}^{m}$
REAL(DFP) :: p_sainv_drop0
! SA-AMG, The drop criteria
LOGICAL(LGT) :: p_saamg_unsym0
! SA-AMG, Select the unsymmetric version
! The matrix structure must be symmetric
REAL(DFP) :: p_saamg_theta0
! SA-AMG, The drop criteria
REAL(DFP) :: p_iluc_drop0
! Crout ILU, default is 0.05, The drop criteria
REAL(DFP) :: p_iluc_rate0
! Crout ILU, The ratio of the maximum fill-in
LOGICAL(LGT) :: p_adds0
! ilut Additive Schwarz, default is true

LOGICAL(LGT) :: initx_zeros0
! if True, then we set sol=0.0 as initial guess.

LOGICAL(LGT) :: isPrecondition
! If true then precondition is used

TYPE(String) :: opt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL LinSolverInitiate( &
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

CALL lis_solver_create(obj%lis_solver, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL obj%GetParam( &
  solverName=solverName0, preconditionOption=preconditionOption0, &
  maxIter=maxIter0, atol=atol0, rtol=rtol0, convergenceIn=convergenceIn0, &
  convergenceType=convergenceType0, relativeToRHS=relativeToRHS0, &
  krylovSubspaceSize=krylovSubspaceSize0, scale=scale0, &
  initx_zeros=initx_zeros0, bicgstab_ell=bicgstab_ell0, &
  sor_omega=sor_omega0, p_name=p_name0, p_ilu_lfil=p_ilu_lfil0, &
  p_ilu_mbloc=p_ilu_mbloc0, p_ilu_droptol=p_ilu_droptol0, &
  p_ilu_permtol=p_ilu_permtol0, p_ilu_alpha=p_ilu_alpha0, &
  p_ilu_fill=p_ilu_fill0, p_ssor_omega=p_ssor_omega0, &
  p_hybrid_i=p_hybrid_i0, p_hybrid_maxiter=p_hybrid_maxiter0, &
  p_hybrid_tol=p_hybrid_tol0, p_hybrid_omega=p_hybrid_omega0, &
  p_hybrid_ell=p_hybrid_ell0, p_hybrid_restart=p_hybrid_restart0, &
  p_is_alpha=p_is_alpha0, p_is_m=p_is_m0, p_sainv_drop=p_sainv_drop0, &
  p_saamg_unsym=p_saamg_unsym0, p_saamg_theta=p_saamg_theta0, &
  p_iluc_drop=p_iluc_drop0, p_iluc_rate=p_iluc_rate0, &
  p_adds=p_adds0, p_adds_iter=p_adds_iter0)

opt = ""

SELECT CASE (solverName0)

CASE (TypeSolverNameOpt%BICGSTABL)
  opt = ' -i bicgstabl -ell '//ToString(bicgstab_ell0)

CASE (TypeSolverNameOpt%ORTHOMIN, TypeSolverNameOpt%GMRES, &
      TypeSolverNameOpt%FGMRES)
  opt = ' -i '//ToString(solverName0)//' -restart '// &
        ToString(krylovSubspaceSize0)

CASE (TypeSolverNameOpt%IDRS)
  opt = ' -i '//ToString(solverName0)//' -irestart '// &
        ToString(krylovSubspaceSize0)

CASE (TypeSolverNameOpt%SOR)
  opt = ' -i sor -omega '//ToString(sor_omega0)

CASE DEFAULT
  opt = ' -i '//ToString(solverName0)

END SELECT

opt = opt//' -maxiter '//ToString(maxIter0)//" -print 3 "// &
      " -scale "//ToString(scale0)//' -tol '//ToString(rtol0)

IF (initx_zeros0) THEN
  opt = opt//' -initx_zeros true '
ELSE
  opt = opt//' -initx_zeros false '
END IF

IF (relativeToRHS0) THEN
  opt = opt//" -conv_cond 1 "
ELSE
  opt = opt//" -conv_cond 0 "
END IF

isPrecondition = preconditionOption0 .NE. TypePrecondOpt%NONE
IF (.NOT. isPrecondition) THEN
  CALL lis_solver_set_option(opt%chars(), obj%lis_solver, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

SELECT CASE (p_name0)
CASE (TypePrecondOpt%NONE)
  opt = opt//' -p none '
CASE (TypePrecondOpt%JACOBI)
  opt = opt//' -p jacobi '
CASE (TypePrecondOpt%ILU)
  opt = opt//' -p ilu -ilu_fill '//ToString(p_ilu_fill0)
CASE (TypePrecondOpt%SSOR)
  opt = opt//' -p ssor -ssor_omega '//ToString(p_ssor_omega0)

CASE (TypePrecondOpt%HYBRID)
  opt = opt//' -p hybrid -hybrid_i '//ToString(p_hybrid_i0)// &
        ' -hybrid_maxiter '//ToString(p_hybrid_maxiter0)// &
        ' -hybrid_ell '//ToString(p_hybrid_ell0)// &
        ' -hybrid_restart '//ToString(p_hybrid_restart0)// &
        ' -hybrid_tol '//ToString(p_hybrid_tol0)// &
        ' -hybrid_omega '//ToString(p_hybrid_omega0)

CASE (TypePrecondOpt%IS)
  opt = opt//' -p is '//' -is_m '//ToString(p_is_m0)// &
        ' -is_alpha '//ToString(p_is_alpha0)

CASE (TypePrecondOpt%SAINV)
  opt = opt//' -p sainv -sainv_drop '//ToString(p_sainv_drop0)

CASE (TypePrecondOpt%SAAMG)
  IF (p_saamg_unsym0) THEN
    opt = opt//' -p saamg -sammg_unsym true -saamg_theta '// &
          ToString(p_saamg_theta0)
  ELSE
    opt = opt//' -p saamg -sammg_unsym false -saamg_theta '// &
          ToString(p_saamg_theta0)
  END IF

CASE (TypePrecondOpt%ILUC)
  opt = opt//' -p iluc -iluc_drop '//ToString(p_iluc_drop0)// &
        ' -iluc_rate '//ToString(p_iluc_rate0)

CASE (TypePrecondOpt%ADDS)
  opt = opt//' -p ilut -adds true -adds_iter '//ToString(p_adds_iter0)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, "No case found for p_name0")
#endif

END SELECT

CALL lis_solver_set_option(opt%chars(), obj%lis_solver, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL lis_solver_destroy(obj%lis_solver, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL LinSolverDeallocate(obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_final
CALL obj%DEALLOCATE()
END PROCEDURE obj_final

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
