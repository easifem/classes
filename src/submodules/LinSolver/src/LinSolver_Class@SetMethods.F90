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

SUBMODULE(LinSolver_Class) SetMethods
USE BaseType, ONLY: TypeSolverNameOpt
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This subroutine allocates the workspace required for the linear solver
!
!# Introduction
!
! This routine allocates the workspace required for the linear solver

SUBROUTINE AllocateWorkSpace(W, IPAR, solverName, n)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: W(:)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: solverName
  INTEGER(I4B), INTENT(IN) :: n

  INTEGER(I4B) :: i, m

  SELECT CASE (solverName)

  CASE (TypeSolverNameOpt%CG, TypeSolverNameOpt%CGNR)
    i = 5 * n

  CASE (TypeSolverNameOpt%BICG)
    i = 7 * n

  CASE (TypeSolverNameOpt%DBICG)
    i = 11 * n

  CASE (TypeSolverNameOpt%BICGSTAB)
    i = 8 * n

  CASE (TypeSolverNameOpt%TFQMR)
    i = 11 * n

  CASE (TypeSolverNameOpt%ORTHOMIN, TypeSolverNameOpt%GMRES)
    m = INPUT(default=15, option=IPAR(5))
    i = (n + 3) * (m + 2) + (m + 1) * m / 2

  CASE (TypeSolverNameOpt%FGMRES)
    m = INPUT(default=15, option=IPAR(5))
    i = 2 * n * (m + 1) + (m + 1) * m / 2 + 3 * m + 2

  CASE (TypeSolverNameOpt%DQGMRES)
    m = INPUT(default=15, option=IPAR(5)) + 1
    i = n + m * (2 * n + 4)

  END SELECT

  IPAR(4) = i

  CALL Reallocate(W, i)
END SUBROUTINE AllocateWorkSpace

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
INTEGER(I4B) :: s(2)
INTEGER(I4B) :: solverName

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

s = amat%SHAPE()
CALL obj%SetParam(amat=amat, localNumColumn=s(2), localNumRow=s(1), &
                  globalNumRow=s(1), globalNumColumn=s(2))

CALL obj%GetParam(solverName=solverName)

CALL AllocateWorkSpace(w=obj%w, n=s(1), solverName=solverName, ipar=obj%ipar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
