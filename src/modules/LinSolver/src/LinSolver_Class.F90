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
! date: 15 July 2021
! summary: Native linear solver

MODULE LinSolver_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE AbstractLinSolver_Class, ONLY: AbstractLinSolver_

IMPLICIT NONE

PRIVATE

PUBLIC :: LinSolverInitiate
PUBLIC :: LinSolverDeallocate
PUBLIC :: LinSolverPointer_
PUBLIC :: LinSolver_
PUBLIC :: TypeLinSolver

CHARACTER(*), PARAMETER :: modName = "LinSolver_Class"
CHARACTER(*), PARAMETER :: myprefix = "LinSolver"
CHARACTER(*), PARAMETER :: myengine = "NATIVE_SERIAL"
INTEGER(I4B), PARAMETER :: IPAR_LENGTH = 14
INTEGER(I4B), PARAMETER :: FPAR_LENGTH = 14

!----------------------------------------------------------------------------
!                                                                LinSolver_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: Native linear solver
!
!# Introduction
!
! This data type defines a native linear solver, It uses LinSolver library.
! [[LinSolver_]] data type is a container around Yusef Saad's LinSolver
! lib. It is used for solvign the linear system with sparse matrices
!
! - Reference : https://www-users.cs.umn.edu/~saad/software/SPARSKIT/
! - This class interface LinSolver and `EASIFEM`

TYPE, EXTENDS(AbstractLinSolver_) :: LinSolver_
  PRIVATE
  INTEGER(I4B) :: ipar(IPAR_LENGTH) = 0
  !! Integer parameters
  REAL(DFP) :: fpar(FPAR_LENGTH) = 0.0_DFP
  !! Real parameters
  REAL(DFP), ALLOCATABLE :: W(:)
  !! Workspace

CONTAINS

  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
    !! Initiate object

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate Data

  FINAL :: obj_final
  !! Final

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! Set the matrix and preconditioning matrix

  PROCEDURE, PUBLIC, PASS(obj) :: Solve => obj_solve
  !! Solve the system of linear equation

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix

  PROCEDURE, PUBLIC, NOPASS :: GetLinSolverCodeFromName => &
    obj_GetLinSolverCodeFromName

  PROCEDURE, PUBLIC, NOPASS :: GetLinSolverNameFromCode => &
    obj_GetLinSolverNameFromCode

END TYPE LinSolver_

!----------------------------------------------------------------------------
!                                                              TypeLinSolver
!----------------------------------------------------------------------------

TYPE(LinSolver_), PARAMETER :: TypeLinSolver = LinSolver_()

!----------------------------------------------------------------------------
!                                                         LinSolverPointer_
!----------------------------------------------------------------------------

TYPE :: LinSolverPointer_
  CLASS(LinSolver_), POINTER :: Ptr => NULL()
END TYPE LinSolverPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This subroutine initiate the [[LinSolver_]] object
!
!# Introduction
!
! This subroutine initiate the [[LinSolver_]] object
!
! - It Sets the name of the solver
! - It Sets the parameters related to the solver
!
! If name of the solver is `lis_gmres`, `lis_fgmres`, `lis_dqgmres`,
! or `lis_om` then `ipar(1)` denotes the number of restarts required in
! these algorithms. Default value is Set to 20.

INTERFACE LinSolverInitiate
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE LinSolverInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE LinSolverDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE LinSolverDeallocate

!----------------------------------------------------------------------------
!                                                 Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_final(obj)
    TYPE(LinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_final
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetLinSolverCodeFromName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get linear solver integer code from name

INTERFACE
  MODULE FUNCTION obj_GetLinSolverCodeFromName(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetLinSolverCodeFromName
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetLinSolverNameFromCode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get the linear solver name from integer code

INTERFACE
  MODULE FUNCTION obj_GetLinSolverNameFromCode(name) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(15) :: ans
  END FUNCTION obj_GetLinSolverNameFromCode
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  Get the prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(LinSolver_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Set(obj, amat)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), TARGET, INTENT(INOUT) :: amat
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
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
  END SUBROUTINE obj_Solve
END INTERFACE

END MODULE LinSolver_Class
