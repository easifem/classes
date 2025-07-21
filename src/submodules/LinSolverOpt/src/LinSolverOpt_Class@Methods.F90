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

SUBMODULE(LinSolverOpt_Class) Methods
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

END SUBMODULE Methods
