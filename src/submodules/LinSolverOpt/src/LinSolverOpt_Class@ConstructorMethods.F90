! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(LinSolverOpt_Class) ConstructorMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
CALL obj%SetParam( &
  isInitiated=.TRUE., engine=engine, solverName=solverName, &
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
  p_hybrid_restart=p_hybrid_restart, p_is_alpha=p_is_alpha, &
  p_is_m=p_is_m, p_sainv_drop=p_sainv_drop, p_saamg_unsym=p_saamg_unsym, &
  p_saamg_theta=p_saamg_theta, p_iluc_drop=p_iluc_drop, &
  p_iluc_rate=p_iluc_rate, p_adds=p_adds, p_adds_iter=p_adds_iter)

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

obj%isInit = .FALSE.
obj%engine = TypeLinSolverOpt%engine
obj%solverName = TypeLinSolverOpt%solverName
obj%preconditionOption = TypeLinSolverOpt%preconditionOption
obj%maxIter = TypeLinSolverOpt%maxIter
obj%convergenceIn = TypeLinSolverOpt%convergenceIn
obj%convergenceType = TypeLinSolverOpt%convergenceType
obj%krylovSubspaceSize = TypeLinSolverOpt%krylovSubspaceSize
obj%globalNumColumn = 0
obj%globalNumRow = 0
obj%localNumColumn = 0
obj%localNumRow = 0
obj%comm = 0
obj%myRank = 0
obj%numProcs = 1
obj%scale = TypeLinSolverOpt%scale
obj%atol = TypeLinSolverOpt%atol
obj%rtol = TypeLinSolverOpt%rtol
obj%relativeToRHS = TypeLinSolverOpt%relativeToRHS

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
