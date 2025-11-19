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
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = UpperCase(name)

SELECT CASE (astr)
CASE ("CG") !1
  ans = TypeSolverNameOpt%CG

CASE ("BICG", "BCG") !2
  ans = TypeSolverNameOpt%BICG

CASE ("CGS") !3
  ans = TypeSolverNameOpt%CGS

CASE ("BICGSTAB", "BCGSTAB") !4
  ans = TypeSolverNameOpt%BICGSTAB

CASE ("BICGSTABL", "BCGSTABL") !5
  ans = TypeSolverNameOpt%BICGSTABL

CASE ("GPBICG") !6
  ans = TypeSolverNameOpt%GPBICG

CASE ("TFQMR") !7
  ans = TypeSolverNameOpt%TFQMR

CASE ("OMN", "FOM", "ORTHOMIN") !8
  ans = TypeSolverNameOpt%OMN

CASE ("GMRES", "GMR") !9
  ans = TypeSolverNameOpt%GMRES

CASE ("JACOBI") !10
  ans = TypeSolverNameOpt%JACOBI

CASE ("GS") !11
  ans = TypeSolverNameOpt%GS

CASE ("SOR") !12
  ans = TypeSolverNameOpt%SOR

CASE ("BICGSAFE") !13
  ans = TypeSolverNameOpt%BICGSAFE

CASE ("CR") !14
  ans = TypeSolverNameOpt%CR

CASE ("BICR") !15
  ans = TypeSolverNameOpt%BICR

CASE ("CRS") !16
  ans = TypeSolverNameOpt%CRS

CASE ("BICRSTAB") !17
  ans = TypeSolverNameOpt%BICRSTAB

CASE ("GPBICR") !18
  ans = TypeSolverNameOpt%GPBICR

CASE ("BICRSAFE") !19
  ans = TypeSolverNameOpt%BICRSAFE

CASE ("FGMRES") !20
  ans = TypeSolverNameOpt%FGMRES

CASE ("IDRS") !21
  ans = TypeSolverNameOpt%IDRS

CASE ("IDR1") !22
  ans = TypeSolverNameOpt%IDR1

CASE ("MINRES") !23
  ans = TypeSolverNameOpt%MINRES

CASE ("COCG") !24
  ans = TypeSolverNameOpt%COCG

CASE ("COCR") !25
  ans = TypeSolverNameOpt%COCR

CASE ("CGNR", "CGN") !26
  ans = TypeSolverNameOpt%CGNR

CASE ("DBICG", "DBCG") !27
  ans = TypeSolverNameOpt%DBICG

CASE ("DQGMRES") !28
  ans = TypeSolverNameOpt%DQGMRES

CASE ("SUPERLU") !29
  ans = TypeSolverNameOpt%SUPERLU

#ifdef DEBUG_VER
CASE Default
  CALL AssertError1(.FALSE., myName, &
                    'Unknown linear solver name: '//astr)
#endif

END SELECT

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SolverNameToInteger

!----------------------------------------------------------------------------
!                                                     obj_SolverIntegerToName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SolverIntegerToName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SolveIntegerToName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (name)
CASE (TypeSolverNameOpt%SUPERLU)
  ans = "SUPERLU" !1

CASE (TypeSolverNameOpt%CG)
  ans = "CG" !1

CASE (TypeSolverNameOpt%BICG)
  ans = "BICG" !2

CASE (TypeSolverNameOpt%CGS)
  ans = "CGS" !3

CASE (TypeSolverNameOpt%BICGSTAB)
  ans = "BICGSTAB" !4

CASE (TypeSolverNameOpt%BICGSTABL)
  ans = "BICGSTABL" !5

CASE (TypeSolverNameOpt%GPBICG)
  ans = "GPBICG" !6

CASE (TypeSolverNameOpt%TFQMR)
  ans = "TFQMR" !7

CASE (TypeSolverNameOpt%OMN)
  ans = "ORTHOMIN" !8

CASE (TypeSolverNameOpt%GMRES)
  ans = "GMRES" !9

CASE (TypeSolverNameOpt%JACOBI)
  ans = "JACOBI" !10

CASE (TypeSolverNameOpt%GS)
  ans = "GS" !11

CASE (TypeSolverNameOpt%SOR)
  ans = "SOR" !12

CASE (TypeSolverNameOpt%BICGSAFE)
  ans = "BICGSAFE" !13

CASE (TypeSolverNameOpt%CR)
  ans = "CR" !14

CASE (TypeSolverNameOpt%BICR)
  ans = "BICR" !15

CASE (TypeSolverNameOpt%CRS)
  ans = "CRS" !16

CASE (TypeSolverNameOpt%BICRSTAB)
  ans = "BICRSTAB" !17

CASE (TypeSolverNameOpt%GPBICR)
  ans = "GPBICR" !18

CASE (TypeSolverNameOpt%BICRSAFE)
  ans = "BICRSAFE" !19

CASE (TypeSolverNameOpt%FGMRES)
  ans = "FGMRES" !20

CASE (TypeSolverNameOpt%IDRS)
  ans = "IDRS" !21

CASE (TypeSolverNameOpt%IDR1)
  ans = "IDR1" !22

CASE (TypeSolverNameOpt%MINRES)
  ans = "MINRES" !23

CASE (TypeSolverNameOpt%COCG)
  ans = "COCG" !24

CASE (TypeSolverNameOpt%COCR)
  ans = "COCR" !25

CASE (TypeSolverNameOpt%CGNR)
  ans = "CGNR" !26

CASE (TypeSolverNameOpt%DBICG)
  ans = "DBICG" !27

CASE (TypeSolverNameOpt%DQGMRES)
  ans = "DQGMRES" !28

#ifdef DEBUG_VER
CASE Default
  CALL AssertError1(.FALSE., myName, &
                    'Unknown linear solver name')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SolverIntegerToName

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
!                                                       GetPreconditionOption
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPreconditionOption
ans = obj%preconditionOption
END PROCEDURE obj_GetPreconditionOption

!----------------------------------------------------------------------------
!                                                         GetPreconditionName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPreconditionName
ans = obj%p_name
END PROCEDURE obj_GetPreconditionName

!----------------------------------------------------------------------------
!                                                               GetSolverName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSolverName
ans = obj%solverName
END PROCEDURE obj_GetSolverName

!----------------------------------------------------------------------------
!                                                             GetBicgstabEll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBicgstabEll
ans = obj%bicgstab_ell
END PROCEDURE obj_GetBicgstabEll

!----------------------------------------------------------------------------
!                                                      GetKrylovSubspaceSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetKrylovSubspaceSize
ans = obj%krylovSubspaceSize
END PROCEDURE obj_GetKrylovSubspaceSize

!----------------------------------------------------------------------------
!                                                                 GetSorOmega
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSorOmega
ans = obj%sor_omega
END PROCEDURE obj_GetSorOmega

!----------------------------------------------------------------------------
!                                                                GetSsorOmega
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSsorOmega
ans = obj%ssor_omega
END PROCEDURE obj_GetSsorOmega

!----------------------------------------------------------------------------
!                                                                 GetMaxIter
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxIter
ans = obj%maxIter
END PROCEDURE obj_GetMaxIter

!----------------------------------------------------------------------------
!                                                                    GetScale
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetScale
ans = obj%scale
END PROCEDURE obj_GetScale

!----------------------------------------------------------------------------
!                                                       GetRelativeTolerance
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRelativeTolerance
ans = obj%rtol
END PROCEDURE obj_GetRelativeTolerance

!----------------------------------------------------------------------------
!                                                               GetInitxZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInitxZeros
ans = obj%initx_zeros
END PROCEDURE obj_GetInitxZeros

!----------------------------------------------------------------------------
!                                                            GetRelativeToRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRelativeToRHS
ans = obj%relativeToRHS
END PROCEDURE obj_GetRelativeToRHS

!----------------------------------------------------------------------------
!                                                                  GetIluFill
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIluFill
ans = obj%ilu_fill
END PROCEDURE obj_GetIluFill

!----------------------------------------------------------------------------
!                                                                 GetHybridI
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetHybridI
ans = obj%hybrid_i
END PROCEDURE obj_GetHybridI

!----------------------------------------------------------------------------
!                                                           GetHybridMaxIter
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetHybridMaxIter
ans = obj%hybrid_maxiter
END PROCEDURE obj_GetHybridMaxIter

!----------------------------------------------------------------------------
!                                                               GetHybridEll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetHybridEll
ans = obj%hybrid_ell
END PROCEDURE obj_GetHybridEll

!----------------------------------------------------------------------------
!                                                            GetHybridRestart
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetHybridRestart
ans = obj%hybrid_restart
END PROCEDURE obj_GetHybridRestart

!----------------------------------------------------------------------------
!                                                                GetHybridTol
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetHybridTol
ans = obj%hybrid_tol
END PROCEDURE obj_GetHybridTol

!----------------------------------------------------------------------------
!                                                             GetHybridOmega
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetHybridOmega
ans = obj%hybrid_omega
END PROCEDURE obj_GetHybridOmega

!----------------------------------------------------------------------------
!                                                                      GetIsM
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIsM
ans = obj%is_m
END PROCEDURE obj_GetIsM

!----------------------------------------------------------------------------
!                                                                  GetIsAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIsAlpha
ans = obj%is_alpha
END PROCEDURE obj_GetIsAlpha

!----------------------------------------------------------------------------
!                                                               GetSainvDrop
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSainvDrop
ans = obj%sainv_drop
END PROCEDURE obj_GetSainvDrop

!----------------------------------------------------------------------------
!                                                               GetSaamgUnsym
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSaamgUnsym
ans = obj%saamg_unsym
END PROCEDURE obj_GetSaamgUnsym

!----------------------------------------------------------------------------
!                                                               GetSaamgTheta
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSaamgTheta
ans = obj%saamg_theta
END PROCEDURE obj_GetSaamgTheta

!----------------------------------------------------------------------------
!                                                                 GetIlucDrop
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIlucDrop
ans = obj%iluc_drop
END PROCEDURE obj_GetIlucDrop

!----------------------------------------------------------------------------
!                                                                 GetIlucRate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIlucRate
ans = obj%iluc_rate
END PROCEDURE obj_GetIlucRate

!----------------------------------------------------------------------------
!                                                                GetAddsIter
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAddsIter
ans = obj%adds_iter
END PROCEDURE obj_GetAddsIter

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
