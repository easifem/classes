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

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-15
! summary: This module defines LIS library based linear solver

MODULE LinSolverLis_Class
USE LinSolver_Class, ONLY: LinSolver_

IMPLICIT NONE

PRIVATE

PUBLIC :: LinSolverLis_
PUBLIC :: TypeLinSolverLis
PUBLIC :: LinSolverLisPointer_

CHARACTER(*), PARAMETER :: modName = "LinSolverLis_Class"
CHARACTER(*), PARAMETER :: myPrefix = "LinSolver"
CHARACTER(*), PARAMETER :: myengine = "LIS_OMP"

!----------------------------------------------------------------------------
!                                                               LinSolver_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-15
! summary: Lis library based linear solver

TYPE, EXTENDS(LinSolver_) :: LinSolverLis_
END TYPE LinSolverLis_

!----------------------------------------------------------------------------
!                                                              TypeLinSolver
!----------------------------------------------------------------------------

TYPE(LinSolverLis_), PARAMETER :: TypeLinSolverLis = LinSolverLis_()

!----------------------------------------------------------------------------
!                                                       LinSolverLisPointer_
!----------------------------------------------------------------------------

TYPE :: LinSolverLisPointer_
  CLASS(LinSolverLis_), POINTER :: ptr => NULL()
END TYPE LinSolverLisPointer_

END MODULE LinSolverLis_Class
