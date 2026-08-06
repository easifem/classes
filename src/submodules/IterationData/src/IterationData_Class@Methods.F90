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

SUBMODULE(IterationData_Class) Methods
USE Display_Method, ONLY: Display
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "obj_Initiate()"
#endif

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

obj%isInit = math%yes

IF (PRESENT(maxIter)) obj%maxIter = maxIter
IF (PRESENT(iterationNumber)) obj%iterationNumber = iterationNumber
IF (PRESENT(residualError0)) obj%residualError0 = residualError0
IF (PRESENT(residualError)) obj%residualError = residualError
IF (PRESENT(residualRelTolerance)) &
  obj%residualRelTolerance = residualRelTolerance
IF (PRESENT(residualAbsTolerance)) &
  obj%residualAbsTolerance = residualAbsTolerance

IF (PRESENT(solutionRelTolerance)) &
  obj%solutionRelTolerance = solutionRelTolerance
IF (PRESENT(solutionAbsTolerance)) &
  obj%solutionAbsTolerance = solutionAbsTolerance
IF (PRESENT(solutionError0)) obj%solutionError0 = solutionError0
IF (PRESENT(solutionError)) obj%solutionError = solutionError

IF (PRESENT(convergenceType)) obj%convergenceType = convergenceType
IF (PRESENT(convergenceIn)) obj%convergenceIn = convergenceIn
IF (PRESENT(normType)) obj%normType = normType
IF (PRESENT(converged)) obj%converged = converged
IF (PRESENT(timeAtStart)) obj%timeAtStart = timeAtStart
IF (PRESENT(timeAtEnd)) obj%timeAtEnd = timeAtEnd

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

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = math%no
obj%maxIter = 0
obj%iterationNumber = 0
obj%residualError = 0.0
obj%residualError0 = 0.0
obj%solutionError = 0.0
obj%solutionError0 = 0.0
obj%solutionRelTolerance = 1.0E-5_DFP
obj%solutionAbsTolerance = 1.0E-5_DFP
obj%residualRelTolerance = 1.0E-5_DFP
obj%residualAbsTolerance = 1.0E-5_DFP
obj%convergenceType = 0
obj%convergenceIn = 0
obj%normType = 0
obj%converged = math%no
obj%timeAtStart = 0.0
obj%timeAtEnd = 0.0

isok = ALLOCATED(obj%residualHistory)
IF (isok) DEALLOCATE (obj%residualHistory)

isok = ALLOCATED(obj%solutionHistory)
IF (isok) DEALLOCATE (obj%solutionHistory)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)

CALL Display(obj%isInit, 'isInit: ', unitNo=unitNo)
CALL Display(obj%converged, 'converged: ', unitNo=unitNo)
CALL Display(obj%storeHistory, 'storeHistory: ', unitNo=unitNo)

CALL Display(obj%maxIter, 'maxIter: ', unitNo=unitNo)
CALL Display(obj%iterationNumber, 'iterationNumber: ', unitNo=unitNo)
CALL Display(obj%convergenceType, 'convergenceType: ', unitNo=unitNo)
CALL Display(obj%convergenceIn, 'convergenceIn: ', unitNo=unitNo)
CALL Display(obj%normType, 'normType: ', unitNo=unitNo)

CALL Display(obj%residualError0, 'residualError0: ', unitNo=unitNo)
CALL Display(obj%residualError, 'residualError: ', unitNo=unitNo)
CALL Display(obj%residualRelTolerance, 'residualRelTolerance: ', &
             unitNo=unitNo)
CALL Display(obj%residualAbsTolerance, 'residualAbsTolerance: ', &
             unitNo=unitNo)

CALL Display(obj%solutionError0, 'solutionError0: ', unitNo=unitNo)
CALL Display(obj%solutionError, 'solutionError: ', unitNo=unitNo)
CALL Display(obj%solutionRelTolerance, 'solutionRelTolerance: ', &
             unitNo=unitNo)
CALL Display(obj%solutionAbsTolerance, 'solutionAbsTolerance: ', &
             unitNo=unitNo)

CALL Display(obj%timeAtStart, 'timeAtStart: ', unitNo=unitNo)
CALL Display(obj%timeAtEnd, 'timeAtEnd: ', unitNo=unitNo)

isok = ALLOCATED(obj%residualHistory)
CALL Display(isok, "residualHistory ALLOCATED:", unitNo=unitNo)
IF (isok) THEN
  CALL Display(obj%residualHistory, "residualHistory: ", unitNo=unitNo)
END IF

isok = ALLOCATED(obj%solutionHistory)
CALL Display(isok, "solutionHistory ALLOCATED:", unitNo=unitNo)
IF (isok) THEN
  CALL Display(obj%solutionHistory, "solutionHistory: ", unitNo=unitNo)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                IsConverged
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsConverged
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsConverged()"
#endif
LOGICAL(LGT) :: ans1, ans2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%convergenceIn)

  ! Convergence in residual
CASE (TypeConvergenceOpt%res)

  SELECT CASE (obj%convergenceType)
  CASE (TypeConvergenceOpt%relative)
    ans = obj%residualError .LE. &
          (obj%residualRelTolerance * obj%residualError0)

  CASE (TypeConvergenceOpt%absolute)
    ans = obj%residualError .LE. obj%residualAbsTolerance

  CASE (TypeConvergenceOpt%both)
    ans = obj%residualError .LE. &
          (obj%residualRelTolerance * obj%residualError0 &
           + obj%residualAbsTolerance)

  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "No case found for obj%convergenceType (1)")
#endif
  END SELECT

  ! Convergence in sol
CASE (TypeConvergenceOpt%sol)

  SELECT CASE (obj%convergenceType)
  CASE (TypeConvergenceOpt%relative)
    ans = obj%solutionError .LE. &
          (obj%solutionRelTolerance * obj%solutionError0)

  CASE (TypeConvergenceOpt%absolute)
    ans = obj%solutionError .LE. obj%solutionAbsTolerance

  CASE (TypeConvergenceOpt%both)
    ans = obj%solutionError .LE. &
          (obj%solutionRelTolerance * obj%solutionError0 &
           + obj%solutionAbsTolerance)

  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "No case found for obj%convergenceType (2)")
#endif
  END SELECT

  ! Convergence in both solution and residual
CASE (TypeConvergenceOpt%both)

  SELECT CASE (obj%convergenceType)
  CASE (TypeConvergenceOpt%relative)
    ans1 = obj%solutionError .LE. &
           (obj%solutionRelTolerance * obj%solutionError0)

    ans2 = obj%residualError .LE. &
           (obj%residualRelTolerance * obj%residualError0)
    ans = ans1 .AND. ans2

  CASE (TypeConvergenceOpt%absolute)
    ans1 = obj%solutionError .LE. obj%solutionAbsTolerance
    ans2 = obj%residualError .LE. obj%residualAbsTolerance
    ans = ans1 .AND. ans2

  CASE (TypeConvergenceOpt%both)
    ans1 = obj%solutionError .LE. &
           (obj%solutionRelTolerance * obj%solutionError0 &
            + obj%solutionAbsTolerance)

    ans2 = obj%residualError .LE. &
           (obj%residualRelTolerance * obj%residualError0 &
            + obj%residualAbsTolerance)

    ans = ans1 .AND. ans2

  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "No case found for obj%convergenceType (3)")
#endif
  END SELECT

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for obj%convergenceIn")
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsConverged

!----------------------------------------------------------------------------
!                                                                 GetMaxIter
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxIter
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxIter()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%maxIter

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxIter

!----------------------------------------------------------------------------
!                                                                GetNormType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNormType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNormType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%normType

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNormType

!----------------------------------------------------------------------------
!                                                          GetIterationNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIterationNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetIterationNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%iterationNumber

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetIterationNumber

!----------------------------------------------------------------------------
!                                                          SetResidualError0
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetResidualError0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetResidualError0()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%residualError0 = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetResidualError0

!----------------------------------------------------------------------------
!                                                           SetResidualError
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetResidualError
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetResidualError()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%residualError = VALUE
IF (obj%storeHistory) obj%residualHistory(obj%iterationNumber) = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetResidualError

!----------------------------------------------------------------------------
!                                                          SetSolutionError0
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSolutionError0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSolutionError0()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%SolutionError0 = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSolutionError0

!----------------------------------------------------------------------------
!                                                           SetSolutionError
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSolutionError
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSolutionError()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%SolutionError = VALUE
IF (obj%storeHistory) obj%solutionHistory(obj%iterationNumber) = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSolutionError

!----------------------------------------------------------------------------
!                                                         SetIterationNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIterationNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIterationNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%iterationNumber = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIterationNumber

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
