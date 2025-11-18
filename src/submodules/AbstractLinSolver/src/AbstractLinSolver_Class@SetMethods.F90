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

SUBMODULE(AbstractLinSolver_Class) SetMethods
USE InputUtility, ONLY: Input
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             SerParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(isInitiated)) obj%isInit = isInitiated
IF (PRESENT(ierr)) obj%ierr = ierr
IF (PRESENT(iter)) obj%iter = iter
IF (PRESENT(tol)) obj%tol = tol
IF (PRESENT(normRes)) obj%normRes = normRes
IF (PRESENT(error0)) obj%error0 = error0
IF (PRESENT(error)) obj%error = error
IF (PRESENT(res)) obj%res = res
IF (PRESENT(amat)) obj%amat => amat

CALL obj%opt%SetParam( &
  isInitiated=isInitiated, engine=engine, solverName=solverName, &
  preconditionOption=preconditionOption, maxIter=maxIter, atol=atol, &
  rtol=rtol, convergenceIn=convergenceIn, convergenceType=convergenceType, &
  relativeToRHS=relativeToRHS, krylovSubspaceSize=krylovSubspaceSize, &
  globalNumRow=globalNumRow, globalNumColumn=globalNumColumn, &
  localNumRow=localNumRow, localNumColumn=localNumColumn, scale=scale, &
  initx_zeros=initx_zeros, bicgstab_ell=bicgstab_ell, sor_omega=sor_omega, &
  p_name=p_name, p_ilu_lfil=p_ilu_lfil, p_ilu_mbloc=p_ilu_mbloc, &
  p_ilu_droptol=p_ilu_droptol, p_ilu_permtol=p_ilu_permtol, &
  p_ilu_alpha=p_ilu_alpha, p_ilu_fill=p_ilu_fill, &
  p_ssor_omega=p_ssor_omega, p_hybrid_i=p_hybrid_i, &
  p_hybrid_maxiter=p_hybrid_maxiter, p_hybrid_tol=p_hybrid_tol, &
  p_hybrid_omega=p_hybrid_omega, p_hybrid_ell=p_hybrid_ell, &
  p_hybrid_restart=p_hybrid_restart, p_is_alpha=p_is_alpha, &
  p_is_m=p_is_m, p_sainv_drop=p_sainv_drop, p_saamg_unsym=p_saamg_unsym, &
  p_saamg_theta=p_saamg_theta, p_iluc_drop=p_iluc_drop, &
  p_iluc_rate=p_iluc_rate, p_adds=p_adds, p_adds_iter=p_adds_iter)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
