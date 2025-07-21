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

SUBMODULE(AbstractLinSolver_Class) GetMethods
USE BaseType, ONLY: TypePrecondOpt, TypeConvergenceOpt

USE StringUtility, ONLY: UpperCase

USE FPL_Method, ONLY: GetValue

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                           GetMatrixPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMatrixPointer
ans => obj%amat
END PROCEDURE obj_GetMatrixPointer

!----------------------------------------------------------------------------
!                                                     solverName_ToInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE solverName_ToInteger
ans = obj%GetLinSolverCodeFromName(name)
END PROCEDURE solverName_ToInteger

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                                 GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
IF (PRESENT(isInitiated)) isInitiated = obj%isInit
IF (PRESENT(engine)) engine = obj%engine%chars()
IF (PRESENT(solverName)) solverName = obj%solverName
IF (PRESENT(ierr)) ierr = obj%ierr
IF (PRESENT(preconditionOption)) preconditionOption = obj%preconditionOption
IF (PRESENT(iter)) iter = obj%iter
IF (PRESENT(maxIter)) maxIter = obj%maxIter
IF (PRESENT(atol)) atol = obj%atol
IF (PRESENT(rtol)) rtol = obj%rtol
IF (PRESENT(tol)) tol = obj%tol
IF (PRESENT(normRes)) normRes = obj%normRes
IF (PRESENT(error0)) error0 = obj%error0
IF (PRESENT(error)) error = obj%error
IF (PRESENT(convergenceIn)) convergenceIn = obj%convergenceIn
IF (PRESENT(convergenceType)) convergenceType = obj%convergenceType
IF (PRESENT(relativeToRHS)) relativeToRHS = obj%relativeToRHS
IF (PRESENT(KrylovSubspaceSize)) KrylovSubspaceSize = obj%KrylovSubspaceSize
IF (PRESENT(globalNumRow)) globalNumRow = obj%globalNumRow
IF (PRESENT(globalNumColumn)) globalNumColumn = obj%globalNumColumn
IF (PRESENT(localNumRow)) localNumRow = obj%localNumRow
IF (PRESENT(localNumColumn)) localNumColumn = obj%localNumColumn
IF (PRESENT(res)) res = obj%res
IF (PRESENT(Amat)) amat => obj%amat
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                     getPreconditionOption
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPreconditionOption
ans = obj%preconditionOption
END PROCEDURE obj_GetPreconditionOption

!----------------------------------------------------------------------------
!                                                 getAbstractLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE GetAbstractLinSolverParam
IF (PRESENT(engine)) &
  CALL GetValue(obj=param, prefix=prefix, key="engine", &
                VALUE=engine)

IF (PRESENT(solverName)) &
  CALL GetValue(obj=param, prefix=prefix, key="solverName", &
                VALUE=solverName)

IF (PRESENT(preconditionOption)) &
  CALL GetValue(obj=param, prefix=prefix, key="preconditionOption", &
                VALUE=preconditionOption)

IF (PRESENT(maxIter)) &
  CALL GetValue(obj=param, prefix=prefix, key="maxIter", &
                VALUE=maxIter)

IF (PRESENT(atol)) &
  CALL GetValue(obj=param, prefix=prefix, key="atol", &
                VALUE=atol)

IF (PRESENT(rtol)) &
  CALL GetValue(obj=param, prefix=prefix, key="rtol", &
                VALUE=rtol)

IF (PRESENT(convergenceIn)) &
  CALL GetValue(obj=param, prefix=prefix, key="convergenceIn", &
                VALUE=convergenceIn)

IF (PRESENT(convergenceType)) &
  CALL GetValue(obj=param, prefix=prefix, key="convergenceType", &
                VALUE=convergenceType)

IF (PRESENT(relativeToRHS)) &
  CALL GetValue(obj=param, prefix=prefix, key="relativeToRHS", &
                VALUE=relativeToRHS)

IF (PRESENT(KrylovSubspaceSize)) &
  CALL GetValue(obj=param, prefix=prefix, key="KrylovSubspaceSize", &
                VALUE=KrylovSubspaceSize)

IF (PRESENT(scale)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/scale", &
                VALUE=scale)

IF (PRESENT(initx_zeros)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/initx_zeros", &
                VALUE=initx_zeros)

IF (PRESENT(bicgstab_ell)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/bicgstab_ell", &
                VALUE=bicgstab_ell)

IF (PRESENT(sor_omega)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/sor_omega", &
                VALUE=sor_omega)

IF (PRESENT(p_name)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/name", &
                VALUE=p_name)

IF (PRESENT(p_ilu_lfil)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/ilu_lfil", &
                VALUE=p_ilu_lfil)

IF (PRESENT(p_ilu_mbloc)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/ilu_mbloc", &
                VALUE=p_ilu_mbloc)

IF (PRESENT(p_ilu_droptol)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/ilu_droptol", &
                VALUE=p_ilu_droptol)

IF (PRESENT(p_ilu_permtol)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/ilu_permtol", &
                VALUE=p_ilu_permtol)

IF (PRESENT(p_ilu_alpha)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/ilu_alpha", &
                VALUE=p_ilu_alpha)

IF (PRESENT(p_ilu_fill)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/ilu_fill", &
                VALUE=p_ilu_fill)

IF (PRESENT(p_ssor_omega)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/ssor_omega", &
                VALUE=p_ssor_omega)

IF (PRESENT(p_hybrid_i)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/hybrid_i", &
                VALUE=p_hybrid_i)

IF (PRESENT(p_hybrid_maxiter)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/hybrid_maxiter", &
                VALUE=p_hybrid_maxiter)

IF (PRESENT(p_hybrid_tol)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/hybrid_tol", &
                VALUE=p_hybrid_tol)

IF (PRESENT(p_hybrid_omega)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/hybrid_omega", &
                VALUE=p_hybrid_omega)

IF (PRESENT(p_hybrid_ell)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/hybrid_ell", &
                VALUE=p_hybrid_ell)

IF (PRESENT(p_hybrid_restart)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/hybrid_restart", &
                VALUE=p_hybrid_restart)

IF (PRESENT(p_is_alpha)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/is_alpha", &
                VALUE=p_is_alpha)

IF (PRESENT(p_is_m)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/is_m", &
                VALUE=p_is_m)

IF (PRESENT(p_sainv_drop)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/sainv_drop", &
                VALUE=p_sainv_drop)

IF (PRESENT(p_saamg_unsym)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/saamg_unsym", &
                VALUE=p_saamg_unsym)

IF (PRESENT(p_saamg_theta)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/saamg_theta", &
                VALUE=p_saamg_theta)

IF (PRESENT(p_iluc_drop)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/iluc_drop", &
                VALUE=p_iluc_drop)

IF (PRESENT(p_iluc_rate)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/iluc_rate", &
                VALUE=p_iluc_rate)

IF (PRESENT(p_adds)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/adds", &
                VALUE=p_adds)

IF (PRESENT(p_adds_iter)) &
  CALL GetValue(obj=param, prefix=prefix, key="Precond/adds_iter", &
                VALUE=p_adds_iter)

END PROCEDURE getAbstractLinSolverParam

END SUBMODULE GetMethods
