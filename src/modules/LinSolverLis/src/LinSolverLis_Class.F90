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
USE GlobalData, ONLY: I4B, DFP, LGT, INT64
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE AbstractLinSolver_Class, ONLY: AbstractLinSolver_
USE LinSolver_Class, ONLY: LinSolver_

IMPLICIT NONE

PRIVATE

PUBLIC :: LinSolverLis_
PUBLIC :: TypeLinSolverLis
PUBLIC :: LinSolverLisPointer_

CHARACTER(*), PARAMETER :: modName = "LinSolverLis_Class"
CHARACTER(*), PARAMETER :: myengine = "LIS_OMP"

!----------------------------------------------------------------------------
!                                                               LinSolver_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-15
! summary: Lis library based linear solver

TYPE, EXTENDS(LinSolver_) :: LinSolverLis_
  PRIVATE
  INTEGER(INT64) :: lis_precon = 0
  INTEGER(INT64) :: lis_solver = 0

CONTAINS

  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate object
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
    !! Deallocate Data
  FINAL :: obj_final

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
    !! Set the matrix and preconditioning matrix

  ! SET:
  ! @SolveMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Solve => obj_solve
    !! Solve the system of linear equation
END TYPE LinSolverLis_

!----------------------------------------------------------------------------
!                                                              TypeLinSolver
!----------------------------------------------------------------------------

TYPE(LinSolverLis_), PARAMETER :: TypeLinSolverLis = LinSolverLis_()

!----------------------------------------------------------------------------
!                                                              TypeLinSolver
!----------------------------------------------------------------------------

TYPE :: LinSolverLisPointer_
  CLASS(LinSolverLis_), POINTER :: Ptr => NULL()
END TYPE LinSolverLisPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Initiate the linear solver

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_final(obj)
    TYPE(LinSolverLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Set(obj, Amat)
    CLASS(LinSolverLis_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), TARGET, INTENT(INOUT) :: Amat
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Solve@SolveMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary:         This routine solves the system of linear equation
!
!# Introduction
! This routine solves the system of linear equation
! On entry `sol` can contain the initial guess

INTERFACE
  MODULE SUBROUTINE obj_Solve(obj, sol, rhs)
    CLASS(LinSolverLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
  END SUBROUTINE obj_Solve
END INTERFACE

END MODULE LinSolverLis_Class
