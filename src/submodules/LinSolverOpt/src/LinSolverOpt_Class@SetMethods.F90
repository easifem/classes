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

SUBMODULE(LinSolverOpt_Class) SetMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                SetTolerance
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTolerance
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTolerance()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(atol)) obj%atol = atol
IF (PRESENT(rtol)) obj%rtol = rtol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTolerance

!----------------------------------------------------------------------------
!                                                                   SetParam
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
IF (PRESENT(engine)) obj%engine = TRIM(engine)
IF (PRESENT(solverName)) obj%solverName = solverName
IF (PRESENT(preconditionOption)) obj%preconditionOption = preconditionOption
IF (PRESENT(maxIter)) obj%maxIter = maxIter
IF (PRESENT(atol)) obj%atol = atol
IF (PRESENT(rtol)) obj%rtol = rtol
IF (PRESENT(convergenceIn)) obj%convergenceIn = convergenceIn
IF (PRESENT(convergenceType)) obj%convergenceType = convergenceType
IF (PRESENT(relativeToRHS)) obj%relativeToRHS = relativeToRHS
IF (PRESENT(krylovSubspaceSize)) obj%krylovSubspaceSize = krylovSubspaceSize
IF (PRESENT(globalNumRow)) obj%globalNumRow = globalNumRow
IF (PRESENT(globalNumColumn)) obj%globalNumColumn = globalNumColumn
IF (PRESENT(localNumRow)) obj%localNumRow = localNumRow
IF (PRESENT(localNumColumn)) obj%localNumColumn = localNumColumn
IF (PRESENT(scale)) obj%scale = scale
IF (PRESENT(initx_zeros)) obj%initx_zeros = initx_zeros
IF (PRESENT(bicgstab_ell)) obj%bicgstab_ell = bicgstab_ell
IF (PRESENT(sor_omega)) obj%sor_omega = sor_omega
IF (PRESENT(p_name)) obj%p_name = p_name

IF (PRESENT(p_ilu_lfil)) obj%ilu_lfil = p_ilu_lfil
IF (PRESENT(p_ilu_mbloc)) obj%ilu_mbloc = p_ilu_mbloc
IF (PRESENT(p_ilu_droptol)) obj%ilu_droptol = p_ilu_droptol
IF (PRESENT(p_ilu_permtol)) obj%ilu_permtol = p_ilu_permtol
IF (PRESENT(p_ilu_alpha)) obj%ilu_alpha = p_ilu_alpha
IF (PRESENT(p_ilu_fill)) obj%ilu_fill = p_ilu_fill

IF (PRESENT(p_ssor_omega)) obj%ssor_omega = p_ssor_omega

IF (PRESENT(p_hybrid_i)) obj%hybrid_i = p_hybrid_i
IF (PRESENT(p_hybrid_maxiter)) obj%hybrid_maxiter = p_hybrid_maxiter
IF (PRESENT(p_hybrid_tol)) obj%hybrid_tol = p_hybrid_tol
IF (PRESENT(p_hybrid_omega)) obj%hybrid_omega = p_hybrid_omega
IF (PRESENT(p_hybrid_ell)) obj%hybrid_ell = p_hybrid_ell
IF (PRESENT(p_hybrid_restart)) obj%hybrid_restart = p_hybrid_restart

IF (PRESENT(p_is_alpha)) obj%is_alpha = p_is_alpha
IF (PRESENT(p_is_m)) obj%is_m = p_is_m

IF (PRESENT(p_sainv_drop)) obj%sainv_drop = p_sainv_drop
IF (PRESENT(p_saamg_unsym)) obj%saamg_unsym = p_saamg_unsym
IF (PRESENT(p_saamg_theta)) obj%saamg_theta = p_saamg_theta

IF (PRESENT(p_iluc_drop)) obj%iluc_drop = p_iluc_drop
IF (PRESENT(p_iluc_rate)) obj%iluc_rate = p_iluc_rate

IF (PRESENT(p_adds)) obj%adds = p_adds
IF (PRESENT(p_adds_iter)) obj%adds_iter = p_adds_iter

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                               SetPrecondILU
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPrecondILU
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondILU()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(p_ilu_lfil)
IF (isok) obj%ilu_lfil = p_ilu_lfil

isok = PRESENT(p_ilu_mbloc)
IF (isok) obj%ilu_mbloc = p_ilu_mbloc

isok = PRESENT(p_ilu_droptol)
IF (isok) obj%ilu_droptol = p_ilu_droptol

isok = PRESENT(p_ilu_permtol)
IF (isok) obj%ilu_permtol = p_ilu_permtol

isok = PRESENT(p_ilu_alpha)
IF (isok) obj%ilu_alpha = p_ilu_alpha

isok = PRESENT(p_ilu_fill)
IF (isok) obj%ilu_fill = p_ilu_fill

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPrecondILU

!----------------------------------------------------------------------------
!                                                            SetPrecondHybrid
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPrecondHybrid
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondHybrid()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(p_hybrid_i)
IF (isok) obj%hybrid_i = p_hybrid_i

isok = PRESENT(p_hybrid_maxiter)
IF (isok) obj%hybrid_maxiter = p_hybrid_maxiter

isok = PRESENT(p_hybrid_tol)
IF (isok) obj%hybrid_tol = p_hybrid_tol

isok = PRESENT(p_hybrid_omega)
IF (isok) obj%hybrid_omega = p_hybrid_omega

isok = PRESENT(p_hybrid_ell)
IF (isok) obj%hybrid_ell = p_hybrid_ell

isok = PRESENT(p_hybrid_restart)
IF (isok) obj%hybrid_restart = p_hybrid_restart

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPrecondHybrid

!----------------------------------------------------------------------------
!                                                                SetPrecondIS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPrecondIS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondIS()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(p_is_alpha)
IF (isok) obj%is_alpha = p_is_alpha

isok = PRESENT(p_is_m)
IF (isok) obj%is_m = p_is_m

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPrecondIS

!----------------------------------------------------------------------------
!                                                             SetPrecondADDS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPrecondADDS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondADDS()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(p_adds)
IF (isok) obj%adds = p_adds

isok = PRESENT(p_adds_iter)
IF (isok) obj%adds_iter = p_adds_iter

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPrecondADDS

!----------------------------------------------------------------------------
!                                                              SetPrecondSSOR
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPrecondSSOR
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondSSOR()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(p_ssor_omega)
IF (isok) obj%ssor_omega = p_ssor_omega

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPrecondSSOR

!----------------------------------------------------------------------------
!                                                             SetPrecondSAINV
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPrecondSAINV
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondSAINV()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(p_sainv_drop)
IF (isok) obj%sainv_drop = p_sainv_drop

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPrecondSAINV

!----------------------------------------------------------------------------
!                                                             SetPrecondSAAMG
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPrecondSAAMG
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondSAAMG()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(p_saamg_unsym)
IF (isok) obj%saamg_unsym = p_saamg_unsym

isok = PRESENT(p_saamg_theta)
IF (isok) obj%saamg_theta = p_saamg_theta

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPrecondSAAMG

!----------------------------------------------------------------------------
!                                                              SetPrecondILUC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPrecondILUC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondILUC()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(p_iluc_drop)
IF (isok) obj%iluc_drop = p_iluc_drop

isok = PRESENT(p_iluc_rate)
IF (isok) obj%iluc_rate = p_iluc_rate

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPrecondILUC

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
