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

SUBMODULE(AbstractLinSolver_Class) ConstructorMethods
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   SetAbstractLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractLinSolverParam
CHARACTER(*), PARAMETER :: myName = "SetLinSolverParam()"
INTEGER(I4B) :: p_name0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

IF (.NOT. PRESENT(solverName)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: solverName should be present')
END IF

IF (solverName .EQ. LIS_SOR) THEN
  IF (.NOT. PRESENT(sor_omega)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: For solverName LIS_SOR sor_omega should be present')
  END IF
END IF

IF (solverName .EQ. LIS_BICGSTABL) THEN
  IF (.NOT. PRESENT(bicgstab_ell)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: For solverName LIS_BICGSTABL bicgstab_ell '//  &
      & 'should be present')
  END IF
END IF

IF (.NOT. PRESENT(preconditionOption)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: preconditionOption should be present')
END IF

p_name0 = INPUT(option=p_name, default=PRECOND_NONE)

IF (preconditionOption .NE. PRECOND_NONE) THEN
  IF (.NOT. PRESENT(p_name)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: preconditionOption is active, therefore, '// &
      & 'precondition name (p_name) should be present')
  END IF
END IF

SELECT CASE (p_name0)
CASE (PRECOND_NONE)
  !! Do nothing
CASE (PRECOND_ILUT)
  IF (.NOT. PRESENT(p_ilu_droptol) .OR. &
    & .NOT. PRESENT(p_ilu_lfil)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_ILUT (sparsekit) '//   &
    & 'p_ilu_droptol and p_ilu_lfil should be present!')
  END IF
CASE (PRECOND_ILUTP)
  IF (.NOT. PRESENT(p_ilu_droptol) .OR. &
    & .NOT. PRESENT(p_ilu_lfil) .OR. &
    & .NOT. PRESENT(p_ilu_permtol) .OR. &
    & .NOT. PRESENT(p_ilu_mbloc)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_ILUTP (sparsekit) '// &
    & 'p_ilu_droptol, p_ilu_lfil, p_ilu_permtol, '// &
    & 'p_ilu_mbloc should be present!')
  END IF
CASE (PRECOND_ILUD)
  IF (.NOT. PRESENT(p_ilu_droptol) .OR. &
    & .NOT. PRESENT(p_ilu_alpha)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_ILUTP (sparsekit) '//  &
    & 'p_ilu_droptol and p_ilu_alpha should be present!')
  END IF
CASE (PRECOND_ILUDP)
  IF (.NOT. PRESENT(p_ilu_droptol) .OR. &
    & .NOT. PRESENT(p_ilu_alpha) .OR. &
    & .NOT. PRESENT(p_ilu_permtol) .OR. &
    & .NOT. PRESENT(p_ilu_mbloc)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_ILUDP  (sparsekit) p_ilu_droptol, '//  &
    & 'p_ilu_alpha, p_ilu_permtol, '// &
    & 'p_ilu_mbloc should be present!!!')
  END IF
CASE (PRECOND_ILUK)
  IF (.NOT. PRESENT(p_ilu_lfil)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_ILUK  (sparsekit) p_ilu_lfil '//  &
    & 'should be present!')
  END IF

! LIS LIB
CASE (PRECOND_ILU)
  IF (.NOT. PRESENT(p_ilu_fill)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[CONFIG ERROR] :: for PRECOND_ILU (LIS) p_ilu_fill should be present!')
  END IF
CASE (PRECOND_SSOR)
  IF (.NOT. PRESENT(p_ssor_omega)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_SSOR (LIS) p_ssor_omega '//  &
    & 'should be present!')
  END IF
CASE (PRECOND_HYBRID)

  IF (.NOT. PRESENT(p_hybrid_i) .OR. &
    & .NOT. PRESENT(p_hybrid_tol) .OR. &
    & .NOT. PRESENT(p_hybrid_maxiter)  &
    &  ) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_HYBRID (LIS) p_hybrid_i '//  &
    & 'p_hybrid_maxiter p_hybrid_tol '//  &
    & ' should be present!!!')
  END IF

  IF (p_hybrid_i .EQ. LIS_SOR) THEN
    IF (.NOT. PRESENT(p_hybrid_omega)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: for PRECOND_HYBRID (LIS) and '//  &
      & 'p_hybrid_i=LIS_SOR,  p_hybrid_omega '//  &
      & ' should be present!!!')
    END IF
  END IF

  IF (p_hybrid_i .EQ. LIS_BICGSTABL) THEN
    IF (.NOT. PRESENT(p_hybrid_ell)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: for PRECOND_HYBRID (LIS) and '//  &
      & 'p_hybrid_i=LIS_BICGSTABL, '//  &
      & ' p_hybrid_ell should be present!!!')
    END IF
  END IF

  IF (ANY(p_hybrid_i .EQ. [LIS_GMRES, LIS_ORTHOMIN, LIS_FGMRES])) THEN
    IF (.NOT. PRESENT(p_hybrid_restart)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: for PRECOND_HYBRID (LIS), '// &
      & 'and p_hybrid_i=LIS_GMRES LIS_ORTHOMIN LIS_FGMRES, '// &
      & 'p_hybrid_restart should be present!!!')
    END IF
  END IF

CASE (PRECOND_IS)
  IF (.NOT. PRESENT(p_is_alpha) .OR. &
    & .NOT. PRESENT(p_is_m)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_IS (LIS) p_is_alpha, '//  &
    & 'p_is_m should be present!')
  END IF

CASE (PRECOND_SAINV)
  IF (.NOT. PRESENT(p_sainv_drop)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_SAINV (LIS) '//  &
    & 'p_sainv_drop should be present!')
  END IF

CASE (PRECOND_SAAMG)

  IF ( &
    & .NOT. PRESENT(p_saamg_theta) .OR. &
    & .NOT. PRESENT(p_saamg_unsym)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_SAAMG (LIS) '//  &
    & 'p_saamg_theta, p_saamg_unsym '//  &
    & 'should be present!!!')
  END IF

CASE (PRECOND_ILUC)

  IF ( &
    & .NOT. PRESENT(p_iluc_drop) .OR. &
    & .NOT. PRESENT(p_iluc_rate)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_ILUC (LIS) p_iluc_drop, p_iluc_rate'// &
    & 'should be present!!!')
  END IF

CASE (PRECOND_ADDS)

  IF ( &
    & .NOT. PRESENT(p_adds) .OR. &
    & .NOT. PRESENT(p_adds_iter)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: for PRECOND_ADDS (LIS) p_adds, p_adds_iter'// &
    & 'should be present!!!')
  END IF

CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[CONFIG ERROR] :: No case found for given precondition name, '//  &
  &  tostring(p_name0))
END SELECT

! engine
CALL Set(obj=param, prefix=prefix, key="engine", datatype="char",  &
& VALUE=engine)

! solverName
CALL Set(obj=param, prefix=prefix, key="solverName", datatype=1_I4B,  &
& VALUE=solverName)

! preconditionOption
CALL Set(obj=param, prefix=prefix, key="preconditionOption", datatype=1_I4B,  &
& VALUE=preconditionOption)

! maxIter
CALL Set(obj=param, prefix=prefix, key="maxIter", datatype=1_I4B,  &
& VALUE=maxIter)

! rtol
CALL Set(obj=param, prefix=prefix, key="rtol", datatype=1.0_DFP,  &
& VALUE=rtol)

! atol
CALL Set(obj=param, prefix=prefix, key="atol", datatype=1.0_DFP,  &
& VALUE=atol)

! convergenceIn
CALL Set(obj=param, prefix=prefix, key="convergenceIn", datatype=1_I4B,  &
& VALUE=INPUT(option=convergenceIn, default=default_convergenceIn))

! convergenceType
CALL Set(obj=param, prefix=prefix, key="convergenceType", datatype=1_I4B,  &
& VALUE=INPUT(option=convergenceType, default=default_convergenceType))

! relativeToRHS
CALL Set(obj=param, prefix=prefix, key="relativeToRHS", datatype=.TRUE.,  &
& VALUE=INPUT(option=relativeToRHS, default=default_relativeToRHS))

! KrylovSubspaceSize
CALL Set(obj=param, prefix=prefix, key="KrylovSubspaceSize",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=KrylovSubspaceSize, default=default_KrylovSubspaceSize))

! scale
CALL Set(obj=param, prefix=prefix, key="scale",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=scale, default=default_scale))

! initx_zeros
CALL Set(obj=param, prefix=prefix, key="initx_zeros",  &
& datatype=.TRUE.,  &
& VALUE=INPUT(option=initx_zeros, default=default_initx_zeros))

! bicgstab_ell
CALL Set(obj=param, prefix=prefix, key="bicgstab_ell",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=bicgstab_ell, default=default_bicgstab_ell))

! sor_omega
CALL Set(obj=param, prefix=prefix, key="sor_omega",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=sor_omega, default=default_sor_omega))

! p_name
CALL Set(obj=param, prefix=prefix, key="/Precond/name",  &
& datatype=1_I4B,  &
& VALUE=p_name0)

! p_ilu_lfil
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_lfil",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_ilu_lfil, default=default_ilu_lfil))

! p_ilu_mbloc
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_mbloc",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_ilu_mbloc, default=default_ilu_mbloc))

! p_ilu_droptol
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_droptol",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_ilu_droptol, default=default_ilu_droptol))

! p_ilu_permtol
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_permtol",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_ilu_permtol, default=default_ilu_permtol))

! p_ilu_alpha
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_alpha", &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_ilu_alpha, default=default_ilu_alpha))

! p_ilu_fill
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_fill",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_ilu_fill, default=default_ilu_fill))

! p_ssor_omega
CALL Set(obj=param, prefix=prefix, key="/Precond/ssor_omega",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_ssor_omega, default=default_ssor_omega))

! p_hybrid_i
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_i",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_hybrid_i, default=default_hybrid_i))

! p_hybrid_maxiter
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_maxiter",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_hybrid_maxiter, default=default_hybrid_maxiter))

! p_hybrid_tol
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_tol",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_hybrid_tol, default=default_hybrid_tol))

! p_hybrid_omega
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_omega",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_hybrid_omega, default=default_hybrid_omega))

! p_hybrid_ell
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_ell",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_hybrid_ell, default=default_hybrid_ell))

! p_hybrid_restart
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_restart",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_hybrid_restart, default=default_hybrid_restart))

! p_is_alpha
CALL Set(obj=param, prefix=prefix, key="/Precond/is_alpha",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_is_alpha, default=default_is_alpha))

!
! p_is_m
CALL Set(obj=param, prefix=prefix, key="/Precond/is_m",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_is_m, default=default_is_m))

! p_sainv_drop
CALL Set(obj=param, prefix=prefix, key="/Precond/sainv_drop",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_sainv_drop, default=default_sainv_drop))

! p_saamg_unsym
CALL Set(obj=param, prefix=prefix, key="/Precond/saamg_unsym",  &
& datatype=.TRUE.,  &
& VALUE=INPUT(option=p_saamg_unsym, default=default_saamg_unsym))

! p_saamg_theta
CALL Set(obj=param, prefix=prefix, key="/Precond/saamg_theta",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_saamg_theta, default=default_saamg_theta))

! p_iluc_drop
CALL Set(obj=param, prefix=prefix, key="/Precond/iluc_drop",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_iluc_drop, default=default_iluc_drop))

! p_iluc_rate
CALL Set(obj=param, prefix=prefix, key="/Precond/iluc_rate",  &
& datatype=1.0_DFP,  &
& VALUE=INPUT(option=p_iluc_rate, default=default_iluc_rate))

! p_adds
CALL Set(obj=param, prefix=prefix, key="/Precond/adds",  &
& datatype=.TRUE.,  &
& VALUE=INPUT(option=p_adds, default=default_adds))

! p_adds_iter
CALL Set(obj=param, prefix=prefix, key="/Precond/adds_iter",  &
& datatype=1_I4B,  &
& VALUE=INPUT(option=p_adds_iter, default=default_adds_iter))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE SetAbstractLinSolverParam

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE als_Deallocate
obj%engine = ''
obj%isInitiated = .FALSE.
obj%ierr = 0
obj%iter = 0
obj%solverName = 0
obj%preconditionOption = 0
obj%convergenceIn = 0
obj%convergenceType = 0
obj%maxIter = 0
obj%relativeToRHS = .FALSE.
obj%KrylovSubspaceSize = 15
obj%atol = 1.0E-8
obj%rtol = 1.0E-8
obj%globalNumColumn = 0
obj%globalNumRow = 0
obj%localNumColumn = 0
obj%localNumRow = 0
obj%comm = 0
obj%myRank = 0
obj%numProcs = 1
IF (ALLOCATED(obj%RES)) DEALLOCATE (obj%RES)
NULLIFY (obj%Amat)
END PROCEDURE als_Deallocate

END SUBMODULE ConstructorMethods
