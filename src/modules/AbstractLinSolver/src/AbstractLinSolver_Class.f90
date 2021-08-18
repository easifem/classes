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
! date: 16 July 2021
! summary: This module defines an abstract class for solving system of linear equation.

MODULE AbstractLinSolver_Class
USE GlobalData
USE BaseType
USE FPL, ONLY: ParameterList_
USE AbstractNodeField_Class
USE AbstractMatrixField_Class
IMPLICIT NONE
PRIVATE
!----------------------------------------------------------------------------
!                                                        AbstractLinSolver_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: It is an abstract class
!
!### Introduction
!
! [[AbstractLinSolver_]] is an abstract class for solving system of linear equation
!
! @note
! It is important to node that [[AbstractLinSolver_]] is created to build an
! interface between `EASIFEM` library and other existing open-source
! and powerful linear solver libraries.
! @endnote

TYPE, ABSTRACT :: AbstractLinSolver_
  LOGICAL( LGT ) :: isInitiated = .FALSE.
    !! is object initiated?
  INTEGER( I4B ) :: solverName = 0
    !! Solver name
  INTEGER( I4B ) :: ierr = 0
    !! error code returned by the solver
  INTEGER( I4B ) :: preconditionOption = 0
    !! Name of preconditioner
  INTEGER( I4B ) ::  iter=0
    !! Current iteration number
  INTEGER( I4B ) ::  maxIter=0
    !! Maximum iteration number
  REAL( DFP ) :: atol=1.0E-8, rtol=1.0E-8
    !! Tolerance for testing convergence
  INTEGER( I4B ) :: convergenceIn = convergenceInRes
    !! convergence in residual or solution
  INTEGER( I4B ) :: convergenceType = relativeConvergence
    !! relative/ absolute convergence
  LOGICAL( LGT ) :: relativeToRHS = .FALSE.
    !! In case of relative convergence, is convergence is relative to
    !! right hand side
  INTEGER( I4B ) :: KrylovSubspaceSize = 15
    !! Useful for GMRES type algorithm
  INTEGER( I4B ) :: globalNumRow = 0, globalNumColumn = 0
    !! Size of the global problem;
  INTEGER( I4B ) :: localNumRow =0, localNumColumn = 0
    !! Size of the problem on a single process
  REAL( DFP ), ALLOCATABLE :: RES( : )
    !! Residual in each iteration
  CLASS( AbstractMatrixField_ ), POINTER :: Amat => NULL()
  CONTAINS
  PROCEDURE( als_initiate ), PUBLIC, DEFERRED, PASS( obj ) :: Initiate
    !! Initiate the object
  PROCEDURE( als_set ), PUBLIC, DEFERRED, PASS( obj ) :: set
    !! Set the matrix and preconditioning matrix
  PROCEDURE( als_solve ), PUBLIC, DEFERRED, PASS( obj ) :: Solve
    !! Solve system of linear equation
  PROCEDURE( als_display ), PUBLIC, DEFERRED, PASS( obj ) :: Display
    !! Display the content
  PROCEDURE( als_deallocateData ), PUBLIC, DEFERRED, PASS( obj ) :: DeallocateData
    !! Deallocate Data
END TYPE AbstractLinSolver_

PUBLIC :: AbstractLinSolver_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractLinSolverPointer_
  CLASS( AbstractLinSolver_ ), POINTER :: ptr => NULL()
END TYPE

PUBLIC :: AbstractLinSolverPointer_

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE als_Initiate( obj, param )
  IMPORT :: AbstractLinSolver_, ParameterList_
  CLASS( AbstractLinSolver_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE als_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE als_Set( obj, Amat )
  IMPORT :: AbstractLinSolver_, AbstractMatrixField_
  CLASS( AbstractLinSolver_ ), INTENT( INOUT) :: obj
  CLASS( AbstractMatrixField_ ), TARGET, INTENT( INOUT ) :: Amat
END SUBROUTINE als_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Solve
!----------------------------------------------------------------------------

! sol contains the initial guess
ABSTRACT INTERFACE
SUBROUTINE als_Solve( obj, sol, rhs )
  IMPORT :: AbstractLinSolver_, AbstractNodeField_
  CLASS( AbstractLinSolver_ ), INTENT( INOUT) :: obj
  CLASS( AbstractNodeField_ ), TARGET, INTENT( INOUT) :: sol
  CLASS( AbstractNodeField_ ), TARGET, INTENT( INOUT) :: rhs
END SUBROUTINE als_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE als_Display( obj, msg, unitno )
  IMPORT :: AbstractLinSolver_, I4B
  CLASS( AbstractLinSolver_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Unitno
END SUBROUTINE als_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE als_DeallocateData( obj )
  IMPORT :: AbstractLinSolver_
  CLASS( AbstractLinSolver_ ), INTENT( INOUT) :: obj
END SUBROUTINE als_DeallocateData
END INTERFACE

END MODULE AbstractLinSolver_Class