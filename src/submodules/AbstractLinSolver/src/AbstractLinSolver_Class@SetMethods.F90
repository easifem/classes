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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SerParam
!----------------------------------------------------------------------------

MODULE PROCEDURE als_SetParam
IF (PRESENT(isInitiated)) obj%isInitiated = isInitiated
IF (PRESENT(engine)) obj%engine = TRIM(engine)
IF (PRESENT(solverName)) obj%solverName = solverName
IF (PRESENT(ierr)) obj%ierr = ierr
IF (PRESENT(preconditionOption)) obj%preconditionOption = preconditionOption
IF (PRESENT(iter)) obj%iter = iter
IF (PRESENT(maxIter)) obj%maxIter = maxIter
IF (PRESENT(atol)) obj%atol = atol
IF (PRESENT(rtol)) obj%rtol = rtol
IF (PRESENT(tol)) obj%tol = tol
IF (PRESENT(normRes)) obj%normRes = normRes
IF (PRESENT(error0)) obj%error0 = error0
IF (PRESENT(error)) obj%error = error
IF (PRESENT(convergenceIn)) obj%convergenceIn = convergenceIn
IF (PRESENT(convergenceType)) obj%convergenceType = convergenceType
IF (PRESENT(relativeToRHS)) obj%relativeToRHS = relativeToRHS
IF (PRESENT(KrylovSubspaceSize)) obj%KrylovSubspaceSize = KrylovSubspaceSize
IF (PRESENT(globalNumRow)) obj%globalNumRow = globalNumRow
IF (PRESENT(globalNumColumn)) obj%globalNumColumn = globalNumColumn
IF (PRESENT(localNumRow)) obj%localNumRow = localNumRow
IF (PRESENT(localNumColumn)) obj%localNumColumn = localNumColumn
IF (PRESENT(RES)) obj%RES = RES
IF (PRESENT(Amat)) obj%Amat => Amat
END PROCEDURE als_SetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE als_setTolerance
IF (PRESENT(atol)) obj%atol = atol
IF (PRESENT(rtol)) obj%rtol = rtol
END PROCEDURE als_setTolerance

!----------------------------------------------------------------------------
!                                                  setAbstractLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setAbstractLinSolverParam
INTEGER(I4B) :: ierr
!
! engine
!
ierr = param%set( &
  & key=TRIM(prefix)//"/engine", &
  & VALUE=TRIM(engine))
!
! solverName
!
IF (PRESENT(solverName)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/solverName", &
  & VALUE=solverName)
!
! preconditionOption
!
IF (PRESENT(preconditionOption)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/preconditionOption", &
  & VALUE=preconditionOption)
!
! maxIter
!
IF (PRESENT(maxIter)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/maxIter", &
  & VALUE=maxIter)
!
! rtol
!
IF (PRESENT(rtol)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/rtol", &
  & VALUE=rtol)
!
! atol
!
IF (PRESENT(atol)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/atol", &
  & VALUE=atol)
!
! convergenceIn
!
IF (PRESENT(convergenceIn)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/convergenceIn", &
  & VALUE=convergenceIn)
!
! convergenceType
!
IF (PRESENT(convergenceType)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/convergenceType", &
  & VALUE=convergenceType)
!
! relativeToRHS
!
IF (PRESENT(relativeToRHS)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/relativeToRHS", &
  & VALUE=relativeToRHS)
!
! KrylovSubspaceSize
!
IF (PRESENT(KrylovSubspaceSize)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/KrylovSubspaceSize", &
  & VALUE=KrylovSubspaceSize)
!
! scale
!
IF (PRESENT(scale)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/scale", &
  & VALUE=scale)
!
! initx_zeros
!
IF (PRESENT(initx_zeros)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/initx_zeros", &
  & VALUE=initx_zeros)
!
! bicgstab_ell
!
IF (PRESENT(bicgstab_ell)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/bicgstab_ell", &
  & VALUE=bicgstab_ell)
!
! sor_omega
!
IF (PRESENT(sor_omega)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/sor_omega", &
  & VALUE=sor_omega)
!
! p_name
!
IF (PRESENT(p_name)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/name", &
  & VALUE=p_name)
!
! p_ilu_lfil
!
IF (PRESENT(p_ilu_lfil)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/ilu_lfil", &
  & VALUE=p_ilu_lfil)
!
! p_ilu_mbloc
!
IF (PRESENT(p_ilu_mbloc)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/ilu_mbloc", &
  & VALUE=p_ilu_mbloc)
!
! p_ilu_droptol
!
IF (PRESENT(p_ilu_droptol)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/ilu_droptol", &
  & VALUE=p_ilu_droptol)
!
! p_ilu_permtol
!
IF (PRESENT(p_ilu_permtol)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/ilu_permtol", &
  & VALUE=p_ilu_permtol)
!
! p_ilu_alpha
!
IF (PRESENT(p_ilu_alpha)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/ilu_alpha", &
  & VALUE=p_ilu_alpha)
!
! p_ilu_fill
!
IF (PRESENT(p_ilu_fill)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/ilu_fill", &
  & VALUE=p_ilu_fill)
!
! p_ssor_omega
!
IF (PRESENT(p_ssor_omega)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/ssor_omega", &
  & VALUE=p_ssor_omega)
!
! p_hybrid_i
!
IF (PRESENT(p_hybrid_i)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/hybrid_i", &
  & VALUE=p_hybrid_i)
!
! p_hybrid_maxiter
!
IF (PRESENT(p_hybrid_maxiter)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/hybrid_maxiter", &
  & VALUE=p_hybrid_maxiter)
!
! p_hybrid_tol
!
IF (PRESENT(p_hybrid_tol)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/hybrid_tol", &
  & VALUE=p_hybrid_tol)
!
! p_hybrid_omega
!
IF (PRESENT(p_hybrid_omega)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/hybrid_omega", &
  & VALUE=p_hybrid_omega)
!
! p_hybrid_ell
!
IF (PRESENT(p_hybrid_ell)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/hybrid_ell", &
  & VALUE=p_hybrid_ell)
!
! p_hybrid_restart
!
IF (PRESENT(p_hybrid_restart)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/hybrid_restart", &
  & VALUE=p_hybrid_restart)
!
! p_is_alpha
!
IF (PRESENT(p_is_alpha)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/is_alpha", &
  & VALUE=p_is_alpha)
!
! p_is_m
!
IF (PRESENT(p_is_m)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/is_m", &
  & VALUE=p_is_m)
!
! p_sainv_drop
!
IF (PRESENT(p_sainv_drop)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/sainv_drop", &
  & VALUE=p_sainv_drop)
!
! p_saamg_unsym
!
IF (PRESENT(p_saamg_unsym)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/saamg_unsym", &
  & VALUE=p_saamg_unsym)
!
! p_saamg_theta
!
IF (PRESENT(p_saamg_theta)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/saamg_theta", &
  & VALUE=p_saamg_theta)
!
! p_iluc_drop
!
IF (PRESENT(p_iluc_drop)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/iluc_drop", &
  & VALUE=p_iluc_drop)
!
! p_iluc_rate
!
IF (PRESENT(p_iluc_rate)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/iluc_rate", &
  & VALUE=p_iluc_rate)
!
! p_adds
!
IF (PRESENT(p_adds)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/adds", &
  & VALUE=p_adds)
!
! p_adds_iter
!
IF (PRESENT(p_adds_iter)) &
  & ierr = param%set( &
  & key=TRIM(prefix)//"/Precond/adds_iter", &
  & VALUE=p_adds_iter)

END PROCEDURE setAbstractLinSolverParam

END SUBMODULE SetMethods
