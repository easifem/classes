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
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE Field
USE AbstractLinSolver_Class
USE HDF5File_Class
IMPLICIT NONE
PRIVATE

CHARACTER(LEN=*), PARAMETER :: modName = "Linsolver_Class"
TYPE(ExceptionHandler_) :: e
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
! This data type defines a native linear solver, It uses LinSolvert library.
! [[LinSolver_]] data type is a container around Yusef Saad's LinSolver
! lib. It is used to solve the linear system with sparse matrices
!
! - Reference : https://www-users.cs.umn.edu/~saad/software/SPARSKIT/
! - This class interface LinSolver and `EASIFEM`

TYPE, EXTENDS(AbstractLinSolver_) :: LinSolver_
  PRIVATE
  INTEGER(I4B) :: ipar(IPAR_LENGTH) = 0
  REAL(DFP) :: fpar(FPAR_LENGTH) = 0.0_DFP
  REAL(DFP), ALLOCATABLE :: W(:)
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: addSurrogate => ls_addSurrogate
    !! Add surrogate to the module exception handler
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & ls_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => ls_Initiate
    !! Initiate object
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => ls_Deallocate
    !! Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Set => ls_Set
    !! Set the matrix and preconditioning matrix
  PROCEDURE, PUBLIC, PASS(obj) :: Solve => ls_solve
    !! Solve the system of linear equation
  PROCEDURE, PUBLIC, PASS(obj) :: Display => ls_display
    !! Display the contents
  PROCEDURE, PUBLIC, PASS(obj) :: Import => ls_Import
    !! Importing linsolver from external file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => ls_Export
    !! Exporting linsolver from external file
  PROCEDURE, PUBLIC, PASS( obj ) :: setTolerance => ls_setTolerance
END TYPE LinSolver_

PUBLIC :: LinSolver_

!----------------------------------------------------------------------------
!                                                              TypeLinSolver
!----------------------------------------------------------------------------

TYPE(LinSolver_), PUBLIC, PARAMETER :: TypeLinSolver = &
    & LinSolver_(RES=NULL(), W=NULL())

!----------------------------------------------------------------------------
!                                                         LinSolverPointer_
!----------------------------------------------------------------------------

TYPE :: LinSolverPointer_
  CLASS(LinSolver_), POINTER :: Ptr => NULL()
END TYPE LinSolverPointer_

PUBLIC :: LinSolverPointer_

!----------------------------------------------------------------------------
!                                      getLinSolverCodeFromName@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION getLinSolverCodeFromName(name) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION getLinSolverCodeFromName
END INTERFACE

PUBLIC :: getLinSolverCodeFromName

!----------------------------------------------------------------------------
!                                      getLinSolverNameFromCode@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION getLinSolverNameFromCode(name) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(LEN=15) :: ans
  END FUNCTION getLinSolverNameFromCode
END INTERFACE

PUBLIC :: getLinSolverNameFromCode

!----------------------------------------------------------------------------
!                                                 addSurrogate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary:         Add surrogate to the module [[ExceptionHandler_]]

INTERFACE
  MODULE SUBROUTINE ls_addSurrogate(obj, UserObj)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    TYPE(ExceptionHandler_), INTENT(IN) :: UserObj
  END SUBROUTINE ls_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                            setLinSolverParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: Set linear solver parameters

INTERFACE
  MODULE SUBROUTINE setLinSolverParam(param, solverName, preconditionOption, &
    & convergenceIn, convergenceType, maxIter, relativeToRHS, &
    & KrylovSubspaceSize, rtol, atol)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    INTEGER(I4B), INTENT(IN) :: solverName
    INTEGER(I4B), INTENT(IN) :: preconditionOption
    INTEGER(I4B), INTENT(IN) :: convergenceIn
    INTEGER(I4B), INTENT(IN) :: convergenceType
    INTEGER(I4B), INTENT(IN) :: maxIter
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativeToRHS
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: KrylovSubspaceSize
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
  END SUBROUTINE setLinSolverParam
END INTERFACE

PUBLIC :: setLinSolverParam

!----------------------------------------------------------------------------
!                                              getLinSolverParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: Returns the linear solver parameters

INTERFACE
  MODULE SUBROUTINE getLinSolverParam(param, solverName, &
    & preconditionOption, convergenceIn, convergenceType, &
    & maxIter, relativeToRHS, KrylovSubspaceSize, rtol, atol)
    TYPE(ParameterList_), INTENT(IN) :: param
    INTEGER(I4B), INTENT(OUT) :: solverName
    INTEGER(I4B), INTENT(OUT) :: preconditionOption
    INTEGER(I4B), INTENT(OUT) :: convergenceIn
    INTEGER(I4B), INTENT(OUT) :: convergenceType
    INTEGER(I4B), INTENT(OUT) :: maxIter
    LOGICAL(LGT), INTENT(OUT) :: relativeToRHS
    INTEGER(I4B), INTENT(OUT) :: KrylovSubspaceSize
    REAL(DFP), INTENT(OUT) :: rtol
    REAL(DFP), INTENT(OUT) :: atol
  END SUBROUTINE getLinSolverParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine checks the essential parameters

INTERFACE
  MODULE SUBROUTINE ls_checkEssentialParam(obj, param)
    CLASS(LinSolver_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE ls_checkEssentialParam
END INTERFACE

PUBLIC :: ls_checkEssentialParam

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
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE ls_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ls_Deallocate(obj)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE ls_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ls_Set(obj, Amat)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), TARGET, INTENT(INOUT) :: Amat
  END SUBROUTINE ls_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetTolerance@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE ls_setTolerance(obj, atol, rtol)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: atol
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: rtol
  END SUBROUTINE ls_setTolerance
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
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
  END SUBROUTINE ls_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine displays the content of linear solver

INTERFACE
  MODULE SUBROUTINE ls_Display(obj, msg, unitno)
    CLASS(LinSolver_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Unitno
  END SUBROUTINE ls_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE ls_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine intiates the linear solver from import

INTERFACE
  MODULE SUBROUTINE ls_Import(obj, hdf5, group)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
  END SUBROUTINE ls_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine exports the linear solver to external file

INTERFACE
  MODULE SUBROUTINE ls_Export(obj, hdf5, group)
    CLASS(LinSolver_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
  END SUBROUTINE ls_Export
END INTERFACE

END MODULE LinSolver_Class
