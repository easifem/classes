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
USE FPL_Method, ONLY: FPL_CheckEssentialParam => CheckEssentialParam

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%isInit = .FALSE.
obj%engine = TypeLinSolverOpt%engine
obj%solverName = TypeLinSolverOpt%solverName
obj%ierr = 0
obj%preconditionOption = TypeLinSolverOpt%preconditionOption
obj%iter = 0
obj%maxIter = TypeLinSolverOpt%maxIter
obj%atol = TypeLinSolverOpt%atol
obj%rtol = TypeLinSolverOpt%rtol
obj%tol = 0.0
obj%convergenceIn = TypeLinSolverOpt%convergenceIn
obj%convergenceType = TypeLinSolverOpt%convergenceType
obj%relativeToRHS = TypeLinSolverOpt%relativeToRHS
obj%krylovSubspaceSize = TypeLinSolverOpt%krylovSubspaceSize
obj%globalNumColumn = 0
obj%globalNumRow = 0
obj%localNumColumn = 0
obj%localNumRow = 0
obj%comm = 0
obj%myRank = 0
obj%numProcs = 1
IF (ALLOCATED(obj%res)) DEALLOCATE (obj%res)
NULLIFY (obj%amat)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                       AbstractLinSolverCheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "ls_checkEssentialParam"
CHARACTER(:), ALLOCATABLE :: keys, prefix

prefix = obj%GetPrefix()
keys = "solverName/preconditionOption/convergenceIn/convergenceType/maxIter/" // &
       "relativeToRHS/KrylovSubspaceSize/rtol/atol"
CALL FPL_CheckEssentialParam(obj=param, keys=keys, &
                             prefix=prefix, myName=myName, modName=modName)
prefix = ""
keys = ""
END PROCEDURE obj_CheckEssentialParam

END SUBMODULE ConstructorMethods
