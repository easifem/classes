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
! summary: This module defines a native linear solver, It uses LinSolvert library

MODULE LinSolver_Class
USE GlobalData
USE BaseType
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE AbstractNodeField_Class
USE AbstractMatrixField_Class
USE MatrixField_Class
USE AbstractLinSolver_Class
IMPLICIT NONE
PRIVATE

CHARACTER( LEN = * ), PARAMETER :: modName="LINSOLVER_CLASS"
TYPE( ExceptionHandler_ ) :: e
INTEGER( I4B ), PARAMETER :: eUnitNo = 1011
CHARACTER( LEN = * ), PARAMETER :: eLogFile = "LINSOLVER_CLASS_EXCEPTION.txt"
INTEGER( I4B ), PARAMETER :: IPAR_LENGTH = 14
INTEGER( I4B ), PARAMETER :: FPAR_LENGTH = 14

!----------------------------------------------------------------------------
!                                                                LinSolver_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: Native linear solver
!
!### Introduction
!
! This data type defines a native linear solver, It uses LinSolvert library.
! [[LinSolver_]] data type is a container around Yusef Saad's LinSolver
! lib. It is used to solve the linear system with sparse matrices
!
! - Reference : https://www-users.cs.umn.edu/~saad/software/SPARSKIT/
! - This class interface LinSolver and `EASIFEM`
!
!
!### Usage
!
! ```fortran
!	CALL obj % Initiate( obj, SolverName, MaxIter, SolverName &
!    & <, diagScale, ipar, fpar> )
! CALL obj % setPrecondition( obj, precondtype <,ipar, fpar> )
! CALL obj % setSparsity( From )
! CALL obj % setDirichletBCNodes( Nptrs, dofs )
! CALL obj % setMatrix( From )
! CALL obj % solve( sol, rhs )
! CALL obj % Display( msg <,unitno > )
! CALL obj % writeResidueHistory( path, prefix, fmt, iter )
! CALL obj % DeallocateData( )
! ```
!
!### Solver name
!
!
!### Precondition Name
!
!
!### Todo
!
!@todo
! - Implement `ilutp` ans `iludp` preconditioners
!@endtodo

TYPE, EXTENDS( AbstractLinSolver_ ) :: LinSolver_
  PRIVATE
  INTEGER( I4B ) :: ipar( IPAR_LENGTH ) = 0
  REAL( DFP ) :: fpar( FPAR_LENGTH ) = 0.0_DFP
  REAL( DFP ), ALLOCATABLE :: W( : )
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => ls_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => ls_Initiate
    !! Initiate object
  PROCEDURE, PUBLIC, PASS( obj ) :: Set => ls_Set
    !! Set the matrix and preconditioning matrix
  PROCEDURE, PUBLIC, PASS( obj ) :: Solve => ls_solve
    !! Solve the system of linear equation
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => ls_display
    !! Display the contents
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => ls_deallocatedata
    !! DeallocateData
END TYPE LinSolver_

PUBLIC :: LinSolver_

TYPE( LinSolver_ ), PUBLIC, PARAMETER :: TypeLinSolver = LinSolver_( RES=NULL(), W=NULL() )

TYPE :: LinSolverPointer_
  CLASS( LinSolver_ ), POINTER :: Ptr => NULL( )
END TYPE LinSolverPointer_

PUBLIC :: LinSolverPointer_

!----------------------------------------------------------------------------
!                                                         setLinSolverParam
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setLinSolverParam( param, solverName, preconditionOption, &
  & convergenceIn, convergenceType, maxIter, relativeToRHS, &
  & KrylovSubspaceSize, rtol, atol )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  INTEGER( I4B ), INTENT( IN ) :: solverName
  INTEGER( I4B ), INTENT( IN ) :: preconditionOption
  INTEGER( I4B ), INTENT( IN ) :: convergenceIn
  INTEGER( I4B ), INTENT( IN ) :: convergenceType
  INTEGER( I4B ), INTENT( IN ) :: maxIter
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: relativeToRHS
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: KrylovSubspaceSize
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: rtol
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: atol
END SUBROUTINE setLinSolverParam
END INTERFACE

PUBLIC :: setLinSolverParam

!----------------------------------------------------------------------------
!                                                         getLinSolverParam
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE getLinSolverParam( param, solverName, preconditionOption, &
  & convergenceIn, convergenceType, maxIter, relativeToRHS, &
  & KrylovSubspaceSize, rtol, atol )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  INTEGER( I4B ), INTENT( OUT ) :: solverName
  INTEGER( I4B ), INTENT( OUT ) :: preconditionOption
  INTEGER( I4B ), INTENT( OUT ) :: convergenceIn
  INTEGER( I4B ), INTENT( OUT ) :: convergenceType
  INTEGER( I4B ), INTENT( OUT ) :: maxIter
  LOGICAL( LGT ), INTENT( OUT ) :: relativeToRHS
  INTEGER( I4B ), INTENT( OUT ) :: KrylovSubspaceSize
  REAL( DFP ), INTENT( OUT ) :: rtol
  REAL( DFP ), INTENT( OUT ) :: atol
END SUBROUTINE getLinSolverParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                        checkEssentialParam
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE ls_checkEssentialParam( obj, param )
  CLASS( LinSolver_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
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
!### Introduction
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
MODULE SUBROUTINE ls_Initiate( obj, param )
  CLASS( LinSolver_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE ls_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE ls_DeallocateData( obj )
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
END SUBROUTINE ls_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE ls_Set( obj, Amat )
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
  CLASS( AbstractMatrixField_ ), TARGET, INTENT( INOUT ) :: Amat
END SUBROUTINE ls_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Solve@Solve
!----------------------------------------------------------------------------

! sol contains the initial guess

INTERFACE
MODULE SUBROUTINE ls_Solve( obj, sol, rhs )
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
  CLASS( AbstractNodeField_ ), TARGET, INTENT( INOUT) :: sol
  CLASS( AbstractNodeField_ ), TARGET, INTENT( INOUT) :: rhs
END SUBROUTINE ls_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE ls_Display( obj, msg, unitno )
  CLASS( LinSolver_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Unitno
END SUBROUTINE ls_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE ls_Display
END INTERFACE Display

PUBLIC :: Display

END MODULE LinSolver_Class