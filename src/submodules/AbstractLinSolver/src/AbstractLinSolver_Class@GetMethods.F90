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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE als_GetParam
IF (PRESENT(isInitiated)) isInitiated = obj%isInitiated
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
IF (PRESENT(RES)) RES = obj%RES
IF (PRESENT(Amat)) Amat => obj%Amat
END PROCEDURE als_GetParam

!----------------------------------------------------------------------------
!                                                     getPreconditionOption
!----------------------------------------------------------------------------

MODULE PROCEDURE als_getPreconditionOption
ans = obj%preconditionOption
END PROCEDURE als_getPreconditionOption

!----------------------------------------------------------------------------
!                                                 getAbstractLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE getAbstractLinSolverParam
INTEGER(I4B) :: ierr
CHARACTER(:), ALLOCATABLE :: char_var

IF (PRESENT(engine)) THEN

  ALLOCATE ( &
    & CHARACTER(LEN=param%DataSizeInBytes( &
    & key=TRIM(prefix)//"/engine")) :: char_var)
  ierr = param%get( &
    & key=TRIM(prefix)//"/engine", &
    & VALUE=char_var)
  engine = TRIM(char_var)
  DEALLOCATE (char_var)

END IF

IF (PRESENT(solverName)) THEN
  ierr = param%get(key=TRIM(prefix)//"/solverName", VALUE=solverName)
END IF

IF (PRESENT(preconditionOption)) THEN
  ierr = param%get(key=TRIM(prefix)//"/preconditionOption", &
  & VALUE=preconditionOption)
END IF

IF (PRESENT(maxIter)) THEN
  ierr = param%get(key=TRIM(prefix)//"/maxIter", &
  & VALUE=maxIter)
END IF

IF (PRESENT(atol)) THEN
  ierr = param%get(key=TRIM(prefix)//"/atol", &
  & VALUE=atol)
END IF

IF (PRESENT(rtol)) THEN
  ierr = param%get(key=TRIM(prefix)//"/rtol", &
  & VALUE=rtol)
END IF

IF (PRESENT(convergenceIn)) THEN
  ierr = param%get(key=TRIM(prefix)//"/convergenceIn", &
  & VALUE=convergenceIn)
END IF

IF (PRESENT(convergenceType)) THEN
  ierr = param%get(key=TRIM(prefix)//"/convergenceType", &
  & VALUE=convergenceType)
END IF

IF (PRESENT(relativeToRHS)) THEN
  ierr = param%get(key=TRIM(prefix)//"/relativeToRHS", &
  & VALUE=relativeToRHS)
END IF

IF (PRESENT(KrylovSubspaceSize)) THEN
  ierr = param%get(key=TRIM(prefix)//"/KrylovSubspaceSize", &
  & VALUE=KrylovSubspaceSize)
END IF

!
! scale
!
IF (PRESENT(scale)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/scale", &
  & VALUE=scale)
!
! initx_zeros
!
IF (PRESENT(initx_zeros)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/initx_zeros", &
  & VALUE=initx_zeros)
!
! bicgstab_ell
!
IF (PRESENT(bicgstab_ell)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/bicgstab_ell", &
  & VALUE=bicgstab_ell)
!
! sor_omega
!
IF (PRESENT(sor_omega)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/sor_omega", &
  & VALUE=sor_omega)
!
! p_name
!
IF (PRESENT(p_name)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/name", &
  & VALUE=p_name)
!
! p_ilu_lfil
!
IF (PRESENT(p_ilu_lfil)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/ilu_lfil", &
  & VALUE=p_ilu_lfil)
!
! p_ilu_mbloc
!
IF (PRESENT(p_ilu_mbloc)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/ilu_mbloc", &
  & VALUE=p_ilu_mbloc)
!
! p_ilu_droptol
!
IF (PRESENT(p_ilu_droptol)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/ilu_droptol", &
  & VALUE=p_ilu_droptol)
!
! p_ilu_permtol
!
IF (PRESENT(p_ilu_permtol)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/ilu_permtol", &
  & VALUE=p_ilu_permtol)
!
! p_ilu_alpha
!
IF (PRESENT(p_ilu_alpha)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/ilu_alpha", &
  & VALUE=p_ilu_alpha)
!
! p_ilu_fill
!
IF (PRESENT(p_ilu_fill)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/ilu_fill", &
  & VALUE=p_ilu_fill)
!
! p_ssor_omega
!
IF (PRESENT(p_ssor_omega)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/ssor_omega", &
  & VALUE=p_ssor_omega)
!
! p_hybrid_i
!
IF (PRESENT(p_hybrid_i)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/hybrid_i", &
  & VALUE=p_hybrid_i)
!
! p_hybrid_maxiter
!
IF (PRESENT(p_hybrid_maxiter)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/hybrid_maxiter", &
  & VALUE=p_hybrid_maxiter)
!
! p_hybrid_tol
!
IF (PRESENT(p_hybrid_tol)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/hybrid_tol", &
  & VALUE=p_hybrid_tol)
!
! p_hybrid_omega
!
IF (PRESENT(p_hybrid_omega)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/hybrid_omega", &
  & VALUE=p_hybrid_omega)
!
! p_hybrid_ell
!
IF (PRESENT(p_hybrid_ell)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/hybrid_ell", &
  & VALUE=p_hybrid_ell)
!
! p_hybrid_restart
!
IF (PRESENT(p_hybrid_restart)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/hybrid_restart", &
  & VALUE=p_hybrid_restart)
!
! p_is_alpha
!
IF (PRESENT(p_is_alpha)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/is_alpha", &
  & VALUE=p_is_alpha)
!
! p_is_m
!
IF (PRESENT(p_is_m)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/is_m", &
  & VALUE=p_is_m)
!
! p_sainv_drop
!
IF (PRESENT(p_sainv_drop)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/sainv_drop", &
  & VALUE=p_sainv_drop)
!
! p_saamg_unsym
!
IF (PRESENT(p_saamg_unsym)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/saamg_unsym", &
  & VALUE=p_saamg_unsym)
!
! p_saamg_theta
!
IF (PRESENT(p_saamg_theta)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/saamg_theta", &
  & VALUE=p_saamg_theta)
!
! p_iluc_drop
!
IF (PRESENT(p_iluc_drop)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/iluc_drop", &
  & VALUE=p_iluc_drop)
!
! p_iluc_rate
!
IF (PRESENT(p_iluc_rate)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/iluc_rate", &
  & VALUE=p_iluc_rate)
!
! p_adds
!
IF (PRESENT(p_adds)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/adds", &
  & VALUE=p_adds)
!
! p_adds_iter
!
IF (PRESENT(p_adds_iter)) &
  & ierr = param%get( &
  & key=TRIM(prefix)//"/Precond/adds_iter", &
  & VALUE=p_adds_iter)

END PROCEDURE getAbstractLinSolverParam

END SUBMODULE GetMethods
