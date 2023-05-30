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
IMPLICIT NONE
CONTAINS

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
