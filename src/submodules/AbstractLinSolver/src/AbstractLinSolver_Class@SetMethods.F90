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

USE BaseType, ONLY: TypePrecondOpt, TypeConvergenceOpt, &
                    TypeSolverNameOpt

USE FPL_Method, ONLY: Set

USE Display_Method, ONLY: ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             SerParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
IF (PRESENT(isInitiated)) obj%isInit = isInitiated
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
IF (PRESENT(res)) obj%res = res
IF (PRESENT(amat)) obj%amat => amat
IF (PRESENT(scale)) obj%scale = scale
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setTolerance
IF (PRESENT(atol)) obj%atol = atol
IF (PRESENT(rtol)) obj%rtol = rtol
END PROCEDURE obj_setTolerance

!----------------------------------------------------------------------------
!                                                  setAbstractLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractLinSolverParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetAbstractLinSolverParam()"
#endif

INTEGER(I4B) :: p_name0, tempint
LOGICAL(LGT) :: isok, tempbool
REAL(DFP) :: tempreal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(solverName)
CALL AssertError1(isok, myname, &
                  'solverName should be present')
#endif

#ifdef DEBUG_VER
IF (solverName .EQ. TypeSolverNameOpt%SOR) THEN
  isok = PRESENT(sor_omega)
  CALL AssertError1(isok, myname, &
                    'For solverName LIS_SOR sor_omega should be present')
END IF
#endif

#ifdef DEBUG_VER
IF (solverName .EQ. TypeSolverNameOpt%BICGSTABL) THEN
  isok = PRESENT(bicgstab_ell)
  CALL AssertError1(isok, myname, &
                'For solverName LIS_BICGSTABL bicgstab_ell should be present')
END IF
#endif

#ifdef DEBUG_VER
isok = PRESENT(preconditionOption)
CALL AssertError1(isok, myname, &
                  'preconditionOption should be present')
#endif

p_name0 = INPUT(option=p_name, default=TypePrecondOpt%NONE)

#ifdef DEBUG_VER
IF (preconditionOption .NE. TypePrecondOpt%NONE) THEN
  isok = PRESENT(p_name)

  CALL AssertError1(isok, myname, &
                    'preconditionOption is active, therefore, '// &
                    'precondition name (p_name) should be present')

END IF
#endif

#ifdef DEBUG_VER
SELECT CASE (p_name0)
CASE (TypePrecondOpt%NONE)
  !! Do nothing

CASE (TypePrecondOpt%ILUT)

  isok = PRESENT(p_ilu_droptol) .AND. PRESENT(p_ilu_fill)
  CALL AssertError1(isok, myname, &
                    'For PRECOND_ILUT (LIS) p_ilu_droptol and '// &
                    'p_ilu_fill should be present')

CASE (TypePrecondOpt%ILUTP)
  isok = PRESENT(p_ilu_droptol) .AND. &
         PRESENT(p_ilu_lfil) .AND. &
         PRESENT(p_ilu_permtol) .AND. &
         PRESENT(p_ilu_mbloc)

  CALL AssertError1(isok, myname, &
                    'for PRECOND_ILUTP (sparsekit) '// &
                    'p_ilu_droptol, p_ilu_lfil, p_ilu_permtol, '// &
                    'p_ilu_mbloc should be present!')

CASE (TypePrecondOpt%ILUD)

  isok = PRESENT(p_ilu_droptol) .AND. &
         PRESENT(p_ilu_alpha)

  CALL AssertError1(isok, myname, &
                    'for PRECOND_ILUD (sparsekit) '// &
                    'p_ilu_droptol and p_ilu_alpha should be present!')

CASE (TypePrecondOpt%ILUDP)

  isok = PRESENT(p_ilu_droptol) .AND. &
         PRESENT(p_ilu_alpha) .AND. &
         PRESENT(p_ilu_permtol) .AND. &
         PRESENT(p_ilu_mbloc)

  CALL AssertError1(isok, myname, &
                    'for PRECOND_ILUDP  (sparsekit) p_ilu_droptol, '// &
                    'p_ilu_alpha, p_ilu_permtol, '// &
                    'p_ilu_mbloc should be present!!!')

CASE (TypePrecondOpt%ILUK)

  isok = PRESENT(p_ilu_lfil)
  CALL AssertError1(isok, myname, &
                'for PRECOND_ILUK  (sparsekit) p_ilu_lfil should be present!')

! LIS LIB
CASE (TypePrecondOpt%ILU)

  isok = PRESENT(p_ilu_fill)
  CALL AssertError1(isok, myname, &
                    'for PRECOND_ILU (LIS) p_ilu_fill should be present!')

CASE (TypePrecondOpt%SSOR)

  isok = PRESENT(p_ssor_omega)
  CALL AssertError1(isok, myname, &
                    'for PRECOND_SSOR (LIS) p_ssor_omega should be present!')

CASE (TypePrecondOpt%HYBRID)

  isok = PRESENT(p_hybrid_i) .AND. &
         PRESENT(p_hybrid_tol) .AND. &
         PRESENT(p_hybrid_maxiter)

  CALL AssertError1(isok, myname, &
                    'for PRECOND_HYBRID (LIS) p_hybrid_i '// &
                    'p_hybrid_maxiter p_hybrid_tol '// &
                    ' should be present!!!')

  SELECT CASE (p_hybrid_i)

  CASE (TypeSolverNameOpt%SOR)
    isok = PRESENT(p_hybrid_omega)
    CALL AssertError1(isok, myname, &
                      'for PRECOND_HYBRID (LIS) and '// &
                      'p_hybrid_i=LIS_SOR,  p_hybrid_omega '// &
                      ' should be present!!!')
  CASE (TypeSolverNameOpt%BICGSTABL)
    isok = PRESENT(p_hybrid_ell)
    CALL AssertError1(isok, myname, &
                      'for PRECOND_HYBRID (LIS) and '// &
                      'p_hybrid_i=LIS_BICGSTABL, '// &
                      ' p_hybrid_ell should be present!!!')

  CASE (TypeSolverNameOpt%gmres, TypeSolverNameOpt%orthomin, &
        TypeSolverNameOpt%fgmres)
    isok = PRESENT(p_hybrid_restart)
    CALL AssertError1(isok, myname, &
                      'for PRECOND_HYBRID (LIS), '// &
                      'and p_hybrid_i=LIS_GMRES LIS_ORTHOMIN LIS_FGMRES, '// &
                      'p_hybrid_restart should be present!!!')

  END SELECT

CASE (TypePrecondOpt%IS)

  isok = PRESENT(p_is_alpha) .AND. PRESENT(p_is_m)
  CALL AssertError1(isok, myname, &
                    'for PRECOND_IS (LIS) p_is_alpha, '// &
                    'p_is_m should be present!')

CASE (TypePrecondOpt%SAINV)

  isok = PRESENT(p_sainv_drop)

  CALL AssertError1(isok, myname, &
                    'for PRECOND_SAINV (LIS) '// &
                    'p_sainv_drop should be present!')

CASE (TypePrecondOpt%SAAMG)

  isok = PRESENT(p_saamg_theta) .AND. PRESENT(p_saamg_unsym)
  CALL AssertError1(isok, myname, &
                    'for PRECOND_SAAMG (LIS) '// &
                    'p_saamg_theta, p_saamg_unsym should be present!!!')

CASE (TypePrecondOpt%ILUC)

  isok = PRESENT(p_iluc_drop) .AND. PRESENT(p_iluc_rate)
  CALL AssertError1(isok, myname, &
                    'for PRECOND_ILUC (LIS) '// &
                    'p_iluc_drop, p_iluc_rate should be present!!!')

CASE (TypePrecondOpt%ADDS)

  isok = PRESENT(p_adds) .AND. PRESENT(p_adds_iter)
  CALL AssertError1(isok, myname, &
                    'for PRECOND_ADDS (LIS) '// &
                    'p_adds, p_adds_iter should be present!!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: No case found for given precondition name')
END SELECT
#endif

! engine
CALL Set(obj=param, prefix=prefix, key="engine", datatype="char", &
         VALUE=engine)

! solverName
CALL Set(obj=param, prefix=prefix, key="solverName", datatype=1_I4B, &
         VALUE=solverName)

! preconditionOption
CALL Set(obj=param, prefix=prefix, key="preconditionOption", datatype=1_I4B, &
         VALUE=preconditionOption)

! maxIter
tempint = INPUT(option=maxIter, default=TypeLinSolverOpt%maxIter)
CALL Set(obj=param, prefix=prefix, key="maxIter", datatype=1_I4B, &
         VALUE=tempint)

! rtol
tempreal = INPUT(option=rtol, default=TypeLinSolverOpt%rtol)
CALL Set(obj=param, prefix=prefix, key="rtol", datatype=1.0_DFP, &
         VALUE=tempreal)

! atol
tempreal = INPUT(option=atol, default=TypeLinSolverOpt%atol)
CALL Set(obj=param, prefix=prefix, key="atol", datatype=1.0_DFP, &
         VALUE=tempreal)

! convergenceIn
tempint = INPUT(option=convergenceIn, default=TypeLinSolverOpt%convergenceIn)
CALL Set(obj=param, prefix=prefix, key="convergenceIn", datatype=1_I4B, &
         VALUE=tempint)

! convergenceType
tempint = INPUT(option=convergenceType, &
                default=TypeLinSolverOpt%convergenceType)
CALL Set(obj=param, prefix=prefix, key="convergenceType", datatype=1_I4B, &
         VALUE=tempint)

! relativeToRHS
tempbool = INPUT(option=relativeToRHS, &
                 default=TypeLinSolverOpt%relativeToRHS)
CALL Set(obj=param, prefix=prefix, key="relativeToRHS", datatype=.TRUE., &
         VALUE=tempbool)

! KrylovSubspaceSize
tempint = INPUT(option=KrylovSubspaceSize, &
                default=TypeLinSolverOpt%KrylovSubspaceSize)
CALL Set(obj=param, prefix=prefix, key="KrylovSubspaceSize", &
         datatype=1_I4B, VALUE=tempint)

! scale
tempint = INPUT(option=scale, default=TypeLinSolverOpt%scale)
CALL Set(obj=param, prefix=prefix, key="scale", &
         datatype=1_I4B, VALUE=tempint)

! initx_zeros
tempbool = INPUT(option=initx_zeros, &
                 default=TypeLinSolverOpt%initx_zeros)
CALL Set(obj=param, prefix=prefix, key="initx_zeros", &
         datatype=.TRUE., VALUE=tempbool)

! bicgstab_ell
tempint = INPUT(option=bicgstab_ell, &
                default=TypeLinSolverOpt%bicgstab_ell)
CALL Set(obj=param, prefix=prefix, key="bicgstab_ell", &
         datatype=1_I4B, VALUE=tempint)

! sor_omega
tempreal = INPUT(option=sor_omega, &
                 default=TypeLinSolverOpt%sor_omega)
CALL Set(obj=param, prefix=prefix, key="sor_omega", &
         datatype=1.0_DFP, VALUE=tempreal)

! p_name
CALL Set(obj=param, prefix=prefix, key="/Precond/name", &
         datatype=1_I4B, VALUE=p_name0)

CALL SetPrecondIluParam(param=param, prefix=prefix, &
                        p_ilu_lfil=p_ilu_lfil, &
                        p_ilu_mbloc=p_ilu_mbloc, &
                        p_ilu_droptol=p_ilu_droptol, &
                        p_ilu_permtol=p_ilu_permtol, &
                        p_ilu_alpha=p_ilu_alpha, &
                        p_ilu_fill=p_ilu_fill)

CALL SetPrecondSsorParam(param=param, prefix=prefix, p_ssor_omega=p_ssor_omega)

CALL SetPrecondHybridParam(param=param, prefix=prefix, p_hybrid_i=p_hybrid_i,&
                           p_hybrid_maxiter=p_hybrid_maxiter, &
                           p_hybrid_tol=p_hybrid_tol, &
                           p_hybrid_omega=p_hybrid_omega, &
                           p_hybrid_ell=p_hybrid_ell, &
                           p_hybrid_restart=p_hybrid_restart)

CALL SetPrecondIsParam(param=param, prefix=prefix, p_is_m=p_is_m, &
                       p_is_alpha=p_is_alpha)

CALL SetPrecondSainvParam(param=param, prefix=prefix, &
                          p_sainv_drop=p_sainv_drop)

CALL SetPrecondSaamgParam(param=param, prefix=prefix, &
                          p_saamg_theta=p_saamg_theta, &
                          p_saamg_unsym=p_saamg_unsym)

CALL SetPrecondIlucParam(param=param, prefix=prefix, &
                         p_iluc_rate=p_iluc_rate, p_iluc_drop=p_iluc_drop)

CALL SetPrecondAddsParam(param=param, prefix=prefix, &
                         p_adds_iter=p_adds_iter, p_adds=p_adds)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetAbstractLinSolverParam

!----------------------------------------------------------------------------
!                                                          SetPrecondIluParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPrecondIluParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetPrecondIluParam()"
#endif

INTEGER(I4B) :: tempint
REAL(DFP) :: tempreal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! p_ilu_lfil
tempint = INPUT(option=p_ilu_lfil, default=TypeLinSolverOpt%ilu_lfil)
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_lfil", &
         datatype=1_I4B, VALUE=tempint)

! p_ilu_mbloc
tempint = INPUT(option=p_ilu_mbloc, default=TypeLinSolverOpt%ilu_mbloc)
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_mbloc", &
         datatype=1_I4B, VALUE=tempint)

! p_ilu_droptol
tempreal = INPUT(option=p_ilu_droptol, default=TypeLinSolverOpt%ilu_droptol)
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_droptol", &
         datatype=1.0_DFP, VALUE=tempreal)

! p_ilu_permtol
tempreal = INPUT(option=p_ilu_permtol, default=TypeLinSolverOpt%ilu_permtol)
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_permtol", &
         datatype=1.0_DFP, VALUE=tempreal)

! p_ilu_alpha
tempreal = INPUT(option=p_ilu_alpha, default=TypeLinSolverOpt%ilu_alpha)
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_alpha", &
         datatype=1.0_DFP, VALUE=tempreal)

! p_ilu_fill
tempint = INPUT(option=p_ilu_fill, default=TypeLinSolverOpt%ilu_fill)
CALL Set(obj=param, prefix=prefix, key="/Precond/ilu_fill", &
         datatype=1_I4B, VALUE=tempint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetPrecondIluParam

!----------------------------------------------------------------------------
!                                                      SetPrecondHybridParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPrecondHybridParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetPrecondHybridParam()"
#endif

INTEGER(I4B) :: tempint
REAL(DFP) :: tempreal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! p_hybrid_i
tempint = INPUT(option=p_hybrid_i, default=TypeLinSolverOpt%hybrid_i)
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_i", &
         datatype=1_I4B, VALUE=tempint)

! p_hybrid_maxiter
tempint = INPUT(option=p_hybrid_maxiter, &
                default=TypeLinSolverOpt%hybrid_maxiter)
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_maxiter", &
         datatype=1_I4B, VALUE=tempint)

! p_hybrid_tol
tempreal = INPUT(option=p_hybrid_tol, default=TypeLinSolverOpt%hybrid_tol)
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_tol", &
         datatype=1.0_DFP, VALUE=tempreal)

! p_hybrid_omega
tempreal = INPUT(option=p_hybrid_omega, default=TypeLinSolverOpt%hybrid_omega)
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_omega", &
         datatype=1.0_DFP, VALUE=tempreal)

! p_hybrid_ell
tempint = INPUT(option=p_hybrid_ell, default=TypeLinSolverOpt%hybrid_ell)
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_ell", &
         datatype=1_I4B, VALUE=tempint)

! p_hybrid_restart
tempint = INPUT(option=p_hybrid_restart, &
                default=TypeLinSolverOpt%hybrid_restart)
CALL Set(obj=param, prefix=prefix, key="/Precond/hybrid_restart", &
         datatype=1_I4B, VALUE=tempint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetPrecondHybridParam

!----------------------------------------------------------------------------
!                                                      SetPrecondIsParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPrecondIsParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetPrecondIsParam()"
#endif

REAL(DFP) :: tempreal
INTEGER(I4B) :: tempint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! p_is_alpha
tempreal = INPUT(option=p_is_alpha, default=TypeLinSolverOpt%is_alpha)
CALL Set(obj=param, prefix=prefix, key="/Precond/is_alpha", &
         datatype=1.0_DFP, VALUE=tempreal)

! p_is_m
tempint = INPUT(option=p_is_m, default=TypeLinSolverOpt%is_m)
CALL Set(obj=param, prefix=prefix, key="/Precond/is_m", &
         datatype=1_I4B, VALUE=tempint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetPrecondIsParam

!----------------------------------------------------------------------------
!                                                      SetPrecondAddsParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPrecondAddsParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetPrecondAddsParam()"
#endif

LOGICAL(LGT) :: tempbool
INTEGER(I4B) :: tempint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! p_adds
tempbool = INPUT(option=p_adds, default=TypeLinSolverOpt%adds)
CALL Set(obj=param, prefix=prefix, key="/Precond/adds", &
         datatype=.TRUE., VALUE=tempbool)

! p_adds_iter
tempint = INPUT(option=p_adds_iter, default=TypeLinSolverOpt%adds_iter)
CALL Set(obj=param, prefix=prefix, key="/Precond/adds_iter", &
         datatype=1_I4B, VALUE=tempint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetPrecondAddsParam

!----------------------------------------------------------------------------
!                                                      SetPrecondSsorParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPrecondSsorParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetPrecondSsorParam()"
#endif

REAL(DFP) :: tempreal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! p_ssor_omega
tempreal = INPUT(option=p_ssor_omega, default=TypeLinSolverOpt%ssor_omega)
CALL Set(obj=param, prefix=prefix, key="/Precond/ssor_omega", &
         datatype=1.0_DFP, VALUE=tempreal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetPrecondSsorParam

!----------------------------------------------------------------------------
!                                                      SetPrecondSainvParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPrecondSainvParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetPrecondSainvParam()"
#endif

REAL(DFP) :: tempreal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! p_sainv_drop
tempreal = INPUT(option=p_sainv_drop, default=TypeLinSolverOpt%sainv_drop)
CALL Set(obj=param, prefix=prefix, key="/Precond/sainv_drop", &
         datatype=1.0_DFP, VALUE=tempreal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetPrecondSainvParam

!----------------------------------------------------------------------------
!                                                      SetPrecondSaamgParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPrecondSaamgParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetPrecondSaamgParam()"
#endif

REAL(DFP) :: tempreal
LOGICAL(LGT) :: tempbool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! p_saamg_unsym
tempbool = INPUT(option=p_saamg_unsym, default=TypeLinSolverOpt%saamg_unsym)
CALL Set(obj=param, prefix=prefix, key="/Precond/saamg_unsym", &
         datatype=.TRUE., VALUE=tempbool)

! p_saamg_theta
tempreal = INPUT(option=p_saamg_theta, default=TypeLinSolverOpt%saamg_theta)
CALL Set(obj=param, prefix=prefix, key="/Precond/saamg_theta", &
         datatype=1.0_DFP, VALUE=tempreal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetPrecondSaamgParam

!----------------------------------------------------------------------------
!                                                      SetPrecondIlucParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPrecondIlucParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetPrecondIlucParam()"
#endif

REAL(DFP) :: tempreal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! p_iluc_drop
tempreal = INPUT(option=p_iluc_drop, default=TypeLinSolverOpt%iluc_drop)
CALL Set(obj=param, prefix=prefix, key="/Precond/iluc_drop", &
         datatype=1.0_DFP, VALUE=tempreal)

! p_iluc_rate
tempreal = INPUT(option=p_iluc_rate, default=TypeLinSolverOpt%iluc_rate)
CALL Set(obj=param, prefix=prefix, key="/Precond/iluc_rate", &
         datatype=1.0_DFP, VALUE=tempreal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetPrecondIlucParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
