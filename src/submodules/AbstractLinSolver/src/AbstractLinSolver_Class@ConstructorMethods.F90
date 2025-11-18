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
USE InputUtility, ONLY: Input

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%engine = TypeLinSolverOpt%engine
obj%solverName = TypeLinSolverOpt%solverName
obj%ierr = 0
obj%preconditionOption = TypeLinSolverOpt%preconditionOption
obj%iter = 0
obj%maxIter = TypeLinSolverOpt%maxIter
obj%convergenceIn = TypeLinSolverOpt%convergenceIn
obj%convergenceType = TypeLinSolverOpt%convergenceType
obj%krylovSubspaceSize = TypeLinSolverOpt%krylovSubspaceSize
obj%globalNumColumn = 0
obj%globalNumRow = 0
obj%localNumColumn = 0
obj%localNumRow = 0
obj%comm = 0
obj%myRank = 0
obj%numProcs = 1
obj%scale = TypeLinSolverOpt%scale
obj%atol = TypeLinSolverOpt%atol
obj%rtol = TypeLinSolverOpt%rtol
obj%tol = 0.0
obj%normRes = 0.0
obj%error0 = 0.0
obj%error = 0.0
obj%relativeToRHS = TypeLinSolverOpt%relativeToRHS
IF (ALLOCATED(obj%res)) DEALLOCATE (obj%res)
NULLIFY (obj%amat)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Initiate
!-------------------------------------------------------------------- --------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'This method is deprecated. Use obj_Initiate2() instead.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%isInit
CALL AssertError1(isok, myName, &
                  "Object is already initialized. Deallocate it first")
#endif

! The deallocate method should be called from the calling routine
! CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%engine = TRIM(engine)
IF (PRESENT(solverName)) obj%solverName = solverName
IF (PRESENT(preconditionOption)) obj%preconditionOption = preconditionOption
IF (PRESENT(maxIter)) obj%maxIter = maxIter
IF (PRESENT(atol)) obj%atol = atol
IF (PRESENT(rtol)) obj%rtol = rtol
IF (PRESENT(convergenceIn)) obj%convergenceIn = convergenceIn
IF (PRESENT(convergenceType)) obj%convergenceType = convergenceType
IF (PRESENT(relativeToRHS)) obj%relativeToRHS = relativeToRHS
IF (PRESENT(KrylovSubspaceSize)) obj%krylovSubspaceSize = krylovSubspaceSize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
