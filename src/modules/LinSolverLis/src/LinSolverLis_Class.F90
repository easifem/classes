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
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE Field
USE AbstractLinSolver_Class
USE LinSolver_Class
USE HDF5File_Class
#include "lisf.h"

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "LinsolverLis_Class"
CHARACTER(*), PARAMETER :: myPrefix = "Linsolver"
CHARACTER(*), PARAMETER :: myengine = "LIS_OMP"

!----------------------------------------------------------------------------
!                                                               LinSolver_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-15
! summary: Lis library based linear solver

TYPE, EXTENDS(LinSolver_) :: LinSolverLis_
  INTEGER(INT64) :: lis_precon = 0
  INTEGER(INT64) :: lis_solver = 0
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => ls_Initiate
    !! Initiate object
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => ls_Deallocate
    !! Deallocate Data
  FINAL :: ls_final
  PROCEDURE, PUBLIC, PASS(obj) :: Set => ls_Set
    !! Set the matrix and preconditioning matrix
  PROCEDURE, PUBLIC, PASS(obj) :: Solve => ls_solve
    !! Solve the system of linear equation
  PROCEDURE, PUBLIC, PASS(obj) :: Display => ls_Display
END TYPE LinSolverLis_

PUBLIC :: LinSolverLis_

!----------------------------------------------------------------------------
!                                                             TypeLinSolver
!----------------------------------------------------------------------------

TYPE(LinSolverLis_), PUBLIC, PARAMETER :: &
  & TypeLinSolverLis = LinSolverLis_()

TYPE :: LinSolverLisPointer_
  CLASS(LinSolverLis_), POINTER :: Ptr => NULL()
END TYPE LinSolverLisPointer_

PUBLIC :: LinSolverLisPointer_

!-----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!-----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This subroutine initiate the [[LinSolver_]] object
!
!# Introduction
!
! This subroutine initiate the [[LinSolver_]] object
!
! - It sets the name of the solver
! - It sets the parameters related to the solver
!
! If name of the solver is `lis_gmres`, `lis_fgmres`, `lis_dqgmres`,
! or `lis_om` then `ipar(1)` denotes the number of restarts required in
! these algorithms. Default value is set to 20.

INTERFACE
  MODULE SUBROUTINE ls_Initiate(obj, param)
    CLASS(LinSolverLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE ls_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ls_Set(obj, Amat)
    CLASS(LinSolverLis_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), TARGET, INTENT(INOUT) :: Amat
  END SUBROUTINE ls_Set
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
  MODULE SUBROUTINE ls_Solve(obj, sol, rhs)
    CLASS(LinSolverLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
  END SUBROUTINE ls_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ls_Display(obj, msg, unitno)
    CLASS(LinSolverLis_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE ls_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ls_Deallocate(obj)
    CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  END SUBROUTINE ls_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ls_final(obj)
    TYPE(LinSolverLis_), INTENT(INOUT) :: obj
  END SUBROUTINE ls_final
END INTERFACE

END MODULE LinSolverLis_Class
