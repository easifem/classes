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
obj%solutionRelTolerance = 1.0E-5
obj%solutionAbsTolerance = 1.0E-5
obj%residualRelTolerance = 1.0E-5
obj%residualAbsTolerance = 1.0E-5
obj%convergenceType = 0
obj%convergenceIn = 0
obj%normType = 0
obj%converged = .FALSE.
obj%timeAtStart = 0.0
obj%timeAtEnd = 0.0

isok = ALLOCATED(obj%convergenceData)
IF (isok) DEALLOCATE (obj%convergenceData)

isok = ALLOCATED(obj%header)
IF (isok) DEALLOCATE (obj%header)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%maxIter, 'maxIter: ', unitNo=unitNo)
CALL Display(obj%iterationNumber, 'iterationNumber: ', unitNo=unitNo)
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

CALL Display(obj%convergenceType, 'convergenceType: ', unitNo=unitNo)
CALL Display(obj%convergenceIn, 'convergenceIn: ', unitNo=unitNo)
CALL Display(obj%normType, 'normType: ', unitNo=unitNo)
CALL Display(obj%converged, 'converged: ', unitNo=unitNo)
CALL Display(obj%timeAtStart, 'timeAtStart: ', unitNo=unitNo)
CALL Display(obj%timeAtEnd, 'timeAtEnd: ', unitNo=unitNo)

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
CHARACTER(*), PARAMETER :: myName = "obj_IsConverged"
#endif
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%convergenceIn)

  ! Convergence in residual
CASE (TypeConvergenceOpt%res)

  abool = obj%convergenceType .EQ. TypeConvergenceOpt%relative

  IF (abool) THEN

    ans = CheckConvergence(errorAtStart=obj%residualError0, &
                           errorAtEnd=obj%residualError, &
                           tolerance=obj%residualRelTolerance)

  ELSE

    ans = CheckConvergence(errorAtStart=math%one, &
                           errorAtEnd=obj%residualError, &
                           tolerance=obj%residualAbsTolerance)

  END IF

  ! Convergence in sol
CASE (TypeConvergenceOpt%sol)

  abool = obj%convergenceType .EQ. TypeConvergenceOpt%relative

  IF (abool) THEN

    ans = CheckConvergence(errorAtStart=obj%solutionError0, &
                           errorAtEnd=obj%solutionError, &
                           tolerance=obj%solutionRelTolerance)

  ELSE

    ans = CheckConvergence(errorAtStart=math%one, &
                           errorAtEnd=obj%solutionError, &
                           tolerance=obj%solutionAbsTolerance)

  END IF

  ! Convergence in both solution and residual
CASE (TypeConvergenceOpt%both)

  abool = obj%convergenceType .EQ. TypeConvergenceOpt%relative

  IF (abool) THEN

    ans = CheckConvergence(errorAtStart=obj%residualError0, &
                           errorAtEnd=obj%residualError, &
                           tolerance=obj%residualRelTolerance) &
          .AND. &
          CheckConvergence(errorAtStart=obj%solutionError0, &
                           errorAtEnd=obj%solutionError, &
                           tolerance=obj%solutionRelTolerance)

  ELSE

    ans = CheckConvergence(errorAtStart=math%one, &
                           errorAtEnd=obj%residualError, &
                           tolerance=obj%residualAbsTolerance) .AND. &
          CheckConvergence(errorAtStart=math%one, &
                           errorAtEnd=obj%solutionError, &
                           tolerance=obj%solutionAbsTolerance)

  END IF

CASE DEFAULT

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
!
!----------------------------------------------------------------------------

PURE FUNCTION CheckConvergence(errorAtStart, errorAtEnd, tolerance) &
  RESULT(ans)
  REAL(DFP), INTENT(IN) :: errorAtStart
  REAL(DFP), INTENT(IN) :: errorAtEnd
  REAL(DFP), INTENT(IN) :: tolerance
  LOGICAL(LGT) :: ans
  ans = (errorAtEnd) .LE. (tolerance * errorAtStart)
END FUNCTION CheckConvergence

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
