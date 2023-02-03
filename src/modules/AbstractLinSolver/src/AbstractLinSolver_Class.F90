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
! summary: An abstract class for solving system of linear equation.

MODULE AbstractLinSolver_Class
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE DirichletBC_Class
USE Field
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                        AbstractLinSolver_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: It is an abstract class
!
!# Introduction
!
! [[AbstractLinSolver_]] is an abstract class for solving system of linear
! equation
!
! @note
! It is important to node that [[AbstractLinSolver_]] is created to build an
! interface between `EASIFEM` library and other existing open-source
! and powerful linear solver libraries.
! @endnote

TYPE, ABSTRACT :: AbstractLinSolver_
  LOGICAL(LGT) :: isInitiated = .FALSE.
    !! is object initiated?
  TYPE(String) :: engine
    !! Name of the engine
    !! NATIVE-SERIAL
    !! NATIVE-OMP
    !! NATIVE-ACC
    !! NATIVE-MPI
    !! PETSC
    !! LIS-OMP
    !! LIS-MPI
  INTEGER(I4B) :: solverName = 0
    !! Solver name
  INTEGER(I4B) :: ierr = 0
    !! error code returned by the solver
  INTEGER(I4B) :: preconditionOption = 0
    !! Name of preconditioner;
    !! - NO_PRECONDITION
    !! - LEFT_PRECONDITION
    !! - RIGHT_PRECONDITION
    !! - LEFT_RIGHT_PRECONDITON
  INTEGER(I4B) :: iter = 0
    !! Current iteration number
  INTEGER(I4B) :: maxIter = 0
    !! Maximum iteration number
  REAL(DFP) :: atol = 0.0_DFP
  REAL(DFP) :: rtol = 1.0E-8
  REAL(DFP) :: tol = 0.0_DFP
    !! Tolerance for testing convergence
  REAL(DFP) :: normRes = 0.0_DFP
  REAL(DFP) :: error0 = 0.0_DFP
  !! initial error res or sol
  REAL(DFP) :: error = 0.0_DFP
  !! final error in res of sol
  INTEGER(I4B) :: convergenceIn = convergenceInRes
    !! convergence in residual or solution
  INTEGER(I4B) :: convergenceType = relativeConvergence
    !! relative/ absolute convergence
  LOGICAL(LGT) :: relativeToRHS = .FALSE.
    !! In case of relative convergence, is convergence is relative to
    !! right hand side
  INTEGER(I4B) :: KrylovSubspaceSize = 15
    !! Useful for GMRES type algorithm
  INTEGER(I4B) :: globalNumRow = 0, globalNumColumn = 0
    !! Size of the global problem;
  INTEGER(I4B) :: localNumRow = 0, localNumColumn = 0
    !! Size of the problem on a single process
  INTEGER(I4B), ALLOCATABLE :: dbcIndx(:)
    !! Indices where Dirichlet boundary conditions is prescribed
  REAL(DFP), ALLOCATABLE :: RES(:)
    !! Residual in each iteration
  CLASS(AbstractMatrixField_), POINTER :: Amat => NULL()
    !! Pointer to child of [[AbstractMatrixField_]]
CONTAINS
  PROCEDURE(als_checkEssentialParam), PUBLIC, DEFERRED, PASS(obj) :: &
    & checkEssentialParam
  PROCEDURE(als_initiate), PUBLIC, DEFERRED, PASS(obj) :: Initiate
    !! Initiate the object
  PROCEDURE(als_set), PUBLIC, DEFERRED, PASS(obj) :: Set
    !! Set the matrix and preconditioning matrix
  PROCEDURE(als_solve), PUBLIC, DEFERRED, PASS(obj) :: Solve
    !! Solve system of linear equation
  PROCEDURE(als_display), PUBLIC, DEFERRED, PASS(obj) :: Display
    !! Display the content
  PROCEDURE(als_Deallocate), PUBLIC, DEFERRED, PASS(obj) :: DEALLOCATE
    !! Deallocate Data
  PROCEDURE(als_Import), PUBLIC, DEFERRED, PASS(obj) :: IMPORT
    !! importing linsolver from external file
  PROCEDURE(als_Export), PUBLIC, DEFERRED, PASS(obj) :: Export
    !! exporting linsolver from external file
  PROCEDURE, PUBLIC, PASS(obj) :: GetPreconditionOption => &
    & als_getPreconditionOption
  PROCEDURE, PUBLIC, PASS(obj) :: setTolerance => &
    & als_setTolerance
  PROCEDURE, PUBLIC, PASS(obj) :: setDirichletBCIndices => &
    & als_setDirichletBCIndices
  !!
END TYPE AbstractLinSolver_

PUBLIC :: AbstractLinSolver_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractLinSolverPointer_
  CLASS(AbstractLinSolver_), POINTER :: ptr => NULL()
END TYPE

PUBLIC :: AbstractLinSolverPointer_

!----------------------------------------------------------------------------
!                                                        checkEssentialParam
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine checks the essential parameters

ABSTRACT INTERFACE
  SUBROUTINE als_checkEssentialParam(obj, param)
    IMPORT :: AbstractLinSolver_, ParameterList_
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE als_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                            setLinSolverParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Jan 2022
! summary: Set linear solver parameters

INTERFACE
  MODULE SUBROUTINE setAbstractLinSolverParam(param, prefix, &
    & engine, solverName, preconditionOption, &
    & maxIter, atol, rtol, convergenceIn, convergenceType, &
    & relativeToRHS, KrylovSubspaceSize)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: solverName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: preconditionOption
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceIn
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativeToRHS
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: KrylovSubspaceSize
  END SUBROUTINE setAbstractLinSolverParam
END INTERFACE

PUBLIC :: setAbstractLinSolverParam

!----------------------------------------------------------------------------
!                                              getLinSolverParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Jan 2022
! summary: Set linear solver parameters

INTERFACE
  MODULE SUBROUTINE getAbstractLinSolverParam(param, prefix, &
    & engine, solverName, preconditionOption, &
    & maxIter, atol, rtol, convergenceIn, convergenceType, &
    & relativeToRHS, KrylovSubspaceSize)
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: solverName
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: preconditionOption
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: maxIter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: atol
    REAL(DFP), OPTIONAL, INTENT(OUT) :: rtol
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: convergenceIn
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: convergenceType
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: relativeToRHS
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: KrylovSubspaceSize
  END SUBROUTINE getAbstractLinSolverParam
END INTERFACE

PUBLIC :: getAbstractLinSolverParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE als_Initiate(obj, param)
    IMPORT :: AbstractLinSolver_, ParameterList_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE als_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE als_Deallocate(obj)
    IMPORT :: AbstractLinSolver_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE als_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE als_Set(obj, Amat)
    IMPORT :: AbstractLinSolver_, AbstractMatrixField_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), TARGET, INTENT(INOUT) :: Amat
  END SUBROUTINE als_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Solve
!----------------------------------------------------------------------------

! sol contains the initial guess
ABSTRACT INTERFACE
  SUBROUTINE als_Solve(obj, sol, rhs)
    IMPORT :: AbstractLinSolver_, AbstractNodeField_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
  END SUBROUTINE als_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE als_Display(obj, msg, unitno)
    IMPORT :: AbstractLinSolver_, I4B
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Unitno
  END SUBROUTINE als_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine intiates the linear solver from import

ABSTRACT INTERFACE
  SUBROUTINE als_Import(obj, hdf5, group)
    IMPORT :: AbstractLinSolver_, HDF5File_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE als_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine exports the linear solver to external file

ABSTRACT INTERFACE
  SUBROUTINE als_Export(obj, hdf5, group)
    IMPORT :: AbstractLinSolver_, HDF5File_
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE als_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                            getPreconditionOption@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 Sept 2021
! summary: Returns the preconditionOption

INTERFACE
  MODULE PURE FUNCTION als_getPreconditionOption(obj) RESULT(Ans)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION als_getPreconditionOption
END INTERFACE

!----------------------------------------------------------------------------
!                                            getPreconditionOption@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 Sept 2021
! summary: Returns the preconditionOption

INTERFACE
  MODULE PURE SUBROUTINE als_setTolerance(obj, atol, rtol)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
  END SUBROUTINE als_setTolerance
END INTERFACE

!----------------------------------------------------------------------------
!                                            getPreconditionOption@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 Sept 2021
! summary: Returns the preconditionOption

INTERFACE
  MODULE PURE SUBROUTINE als_setDirichletBCIndices(obj, indx)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
  END SUBROUTINE als_setDirichletBCIndices
END INTERFACE

END MODULE AbstractLinSolver_Class
