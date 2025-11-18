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

SUBMODULE(LinSolverOpt_Class) GetMethods
USE StringUtility, ONLY: UpperCase

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                         SolverNameToInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SolverNameToInteger
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SolverNameToInteger()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
! ans = obj%GetLinSolverCodeFromName(name)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SolverNameToInteger

!----------------------------------------------------------------------------
!                                             preconditionOption_ToInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PrecondOptToInteger
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_PrecondOptToInteger()"
#endif

CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = UpperCase(name)

SELECT CASE (astr)
CASE ("LEFT", "LEFT_PRECONDITION")
  ans = TypePrecondOpt%left

CASE ("RIGHT", "RIGHT_PRECONDITION")
  ans = TypePrecondOpt%right

CASE ("LEFTRIGHT", "RIGHTLEFT", "LEFTRIGHT_PRECONDITION", &
      "RIGHTLEFT_PRECONDITION")
  ans = TypePrecondOpt%both

CASE DEFAULT
  ans = TypePrecondOpt%NONE
END SELECT

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_PrecondOptToInteger

!----------------------------------------------------------------------------
!                                                  convergenceIn_ToInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ConvergenceInToInteger
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ConvergenceInToInteger()"
#endif

CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = UpperCase(name)

SELECT CASE (astr)
CASE ("RESIDUAL")
  ans = TypeConvergenceOpt%res
CASE ("SOL")
  ans = TypeConvergenceOpt%sol
CASE ("RESIDUALSOL", "SOLRESIDUAL")
  ans = TypeConvergenceOpt%both
CASE DEFAULT
  ans = TypeConvergenceOpt%res
END SELECT

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ConvergenceInToInteger

!----------------------------------------------------------------------------
!                                                  convergenceType_ToInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ConvergenceTypeToInteger
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ConvergenceTypeToInteger()"
#endif

CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = UpperCase(name)

SELECT CASE (astr)
CASE ("RELATIVE")
  ans = TypeConvergenceOpt%relative

CASE ("ABSOLUTE")
  ans = TypeConvergenceOpt%absolute

CASE default
  ans = TypeConvergenceOpt%relative

END SELECT

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ConvergenceTypeToInteger

!----------------------------------------------------------------------------
!                                                  scale_ToInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ScaleToInteger
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ScaleToInteger()"
#endif

CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = UpperCase(name)

SELECT CASE (astr)
CASE ("JACOBI")
  ans = obj%scale_jacobi

CASE ("SYMDIAG")
  ans = obj%scale_symm_diag

CASE default
  ans = obj%scale_none

END SELECT

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ScaleToInteger

!----------------------------------------------------------------------------
!                                                 preconditionName_ToInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PrecondNameToInteger
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_PrecondNameToInteger()"
#endif

CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = UpperCase(name)

SELECT CASE (astr)

CASE ("NONE")
  ans = TypePrecondOpt%NONE

CASE ("JACOBI")
  ans = TypePrecondOpt%jacobi

CASE ("ILU")
  ans = TypePrecondOpt%ilu

CASE ("SSOR")
  ans = TypePrecondOpt%ssor

CASE ("HYBRID")
  ans = TypePrecondOpt%hybrid

CASE ("IS")
  ans = TypePrecondOpt%IS

CASE ("SAINV")
  ans = TypePrecondOpt%SAINV

CASE ("SAAMG")
  ans = TypePrecondOpt%SAAMG

CASE ("ILUC")
  ans = TypePrecondOpt%ILUC

CASE ("ADDS")
  ans = TypePrecondOpt%ADDS

CASE ("ILUTP")
  ans = TypePrecondOpt%ILUTP

CASE ("ILUD")
  ans = TypePrecondOpt%ILUD

CASE ("ILUDP")
  ans = TypePrecondOpt%ILUDP

CASE ("ILU0")
  ans = TypePrecondOpt%ILU0

CASE ("ILUK")
  ans = TypePrecondOpt%ILUK

CASE ("ILUT")
  ans = TypePrecondOpt%ILUT

CASE DEFAULT
  ans = TypePrecondOpt%NONE
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_PrecondNameToInteger

!----------------------------------------------------------------------------
!                                                                IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                       GetPreconditionOption
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPreconditionOption
ans = obj%preconditionOption
END PROCEDURE obj_GetPreconditionOption

!----------------------------------------------------------------------------
!                                                                   GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(isInitiated)) isInitiated = obj%isInit
IF (PRESENT(engine)) engine = TRIM(obj%engine)
IF (PRESENT(solverName)) solverName = obj%solverName
IF (PRESENT(preconditionOption)) preconditionOption = obj%preconditionOption
IF (PRESENT(maxIter)) maxIter = obj%maxIter
IF (PRESENT(atol)) atol = obj%atol
IF (PRESENT(rtol)) rtol = obj%rtol
IF (PRESENT(convergenceIn)) convergenceIn = obj%convergenceIn
IF (PRESENT(convergenceType)) convergenceType = obj%convergenceType
IF (PRESENT(relativeToRHS)) relativeToRHS = obj%relativeToRHS
IF (PRESENT(krylovSubspaceSize)) krylovSubspaceSize = obj%krylovSubspaceSize
IF (PRESENT(globalNumRow)) globalNumRow = obj%globalNumRow
IF (PRESENT(globalNumColumn)) globalNumColumn = obj%globalNumColumn
IF (PRESENT(localNumRow)) localNumRow = obj%localNumRow
IF (PRESENT(localNumColumn)) localNumColumn = obj%localNumColumn

IF (PRESENT(scale)) scale = obj%scale
IF (PRESENT(initx_zeros)) initx_zeros = obj%initx_zeros
IF (PRESENT(bicgstab_ell)) bicgstab_ell = obj%bicgstab_ell
IF (PRESENT(sor_omega)) sor_omega = obj%sor_omega
IF (PRESENT(p_name)) p_name = obj%p_name

IF (PRESENT(p_ilu_lfil)) p_ilu_lfil = obj%ilu_lfil
IF (PRESENT(p_ilu_mbloc)) p_ilu_mbloc = obj%ilu_mbloc
IF (PRESENT(p_ilu_droptol)) p_ilu_droptol = obj%ilu_droptol
IF (PRESENT(p_ilu_permtol)) p_ilu_permtol = obj%ilu_permtol
IF (PRESENT(p_ilu_alpha)) p_ilu_alpha = obj%ilu_alpha
IF (PRESENT(p_ilu_fill)) p_ilu_fill = obj%ilu_fill

IF (PRESENT(p_ssor_omega)) p_ssor_omega = obj%ssor_omega

IF (PRESENT(p_hybrid_i)) p_hybrid_i = obj%hybrid_i
IF (PRESENT(p_hybrid_maxiter)) p_hybrid_maxiter = obj%hybrid_maxiter
IF (PRESENT(p_hybrid_tol)) p_hybrid_tol = obj%hybrid_tol
IF (PRESENT(p_hybrid_omega)) p_hybrid_omega = obj%hybrid_omega
IF (PRESENT(p_hybrid_ell)) p_hybrid_ell = obj%hybrid_ell
IF (PRESENT(p_hybrid_restart)) p_hybrid_restart = obj%hybrid_restart

IF (PRESENT(p_is_alpha)) p_is_alpha = obj%is_alpha
IF (PRESENT(p_is_m)) p_is_m = obj%is_m

IF (PRESENT(p_sainv_drop)) p_sainv_drop = obj%sainv_drop
IF (PRESENT(p_saamg_unsym)) p_saamg_unsym = obj%saamg_unsym
IF (PRESENT(p_saamg_theta)) p_saamg_theta = obj%saamg_theta

IF (PRESENT(p_iluc_drop)) p_iluc_drop = obj%iluc_drop
IF (PRESENT(p_iluc_rate)) p_iluc_rate = obj%iluc_rate

IF (PRESENT(p_adds)) p_adds = obj%adds
IF (PRESENT(p_adds_iter)) p_adds_iter = obj%adds_iter

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
