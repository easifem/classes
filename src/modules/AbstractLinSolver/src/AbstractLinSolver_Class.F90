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
USE AbstractLinSolverParam
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractLinSolver_Class"

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
  !! NATIVE_SERIAL
  !! NATIVE_OMP
  !! NATIVE_ACC
  !! NATIVE_MPI
  !! PETSC
  !! LIS_OMP
  !! LIS_MPI
  INTEGER(I4B) :: solverName = 0
  !! Solver name
  INTEGER(I4B) :: ierr = 0
  !! Error code returned by the solver
  INTEGER(I4B) :: preconditionOption = 0
  !! Name of preconditioner;
  !! NO_PRECONDITION
  !! LEFT_PRECONDITION
  !! RIGHT_PRECONDITION
  !! LEFT_RIGHT_PRECONDITON
  INTEGER(I4B) :: iter = 0
  !! Current iteration number
  INTEGER(I4B) :: maxIter = 0
  !! Maximum iteration number
  REAL(DFP) :: atol = 0.0_DFP
  !! absolute tolerance
  REAL(DFP) :: rtol = 1.0E-8
  !! relative tolerance
  REAL(DFP) :: tol = 0.0_DFP
  !! Tolerance for testing convergence
  REAL(DFP) :: normRes = 0.0_DFP
  !! norm Residual
  REAL(DFP) :: error0 = 0.0_DFP
  !! initial error res or sol
  REAL(DFP) :: error = 0.0_DFP
  !! final error in res of sol
  INTEGER(I4B) :: convergenceIn = convergenceInRes
  !! convergence in residual or solution
  INTEGER(I4B) :: convergenceType = relativeConvergence
  !! relative/ absolute convergence
  LOGICAL(LGT) :: relativeToRHS = .FALSE.
  !! In case of relative convergence
  !! is convergence
  !! is relative to
  !! right hand side
  INTEGER(I4B) :: KrylovSubspaceSize = 15
  !! Useful for GMRES type algorithm
  INTEGER(I4B) :: globalNumRow = 0, globalNumColumn = 0
  !! Size of the global problem;
  INTEGER(I4B) :: localNumRow = 0, localNumColumn = 0
  !! Size of the problem on a single process
  INTEGER(I4B) :: comm = 0_I4B
  !! This is not needed
  INTEGER(I4B) :: myRank = 0_I4B
  !! This is not needed
  INTEGER(I4B) :: numProcs = 1_I4B
  !! this is not needed
  REAL(DFP), ALLOCATABLE :: RES(:)
  !! Residual in each iteration
  CLASS(AbstractMatrixField_), POINTER :: Amat => NULL()
  !! Pointer to child of [[AbstractMatrixField_]]
CONTAINS
  PROCEDURE(als_checkEssentialParam), PUBLIC, DEFERRED, PASS(obj) :: &
    & checkEssentialParam
  PROCEDURE(als_initiate), PUBLIC, DEFERRED, PASS(obj) :: Initiate
  !! Initiate the object
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => als_Deallocate
  !! Deallocate Data
  PROCEDURE(als_set), PUBLIC, DEFERRED, PASS(obj) :: Set
  !! Set the matrix and preconditioning matrix
  PROCEDURE(als_solve), PUBLIC, DEFERRED, PASS(obj) :: Solve
  !! Solve system of linear equation
  PROCEDURE(als_getLinSolverCodeFromName), DEFERRED, PUBLIC, NOPASS :: &
   & getLinSolverCodeFromName
  PROCEDURE(als_getLinSolverNameFromCode), DEFERRED, PUBLIC, NOPASS :: &
   & getLinSolverNameFromCode
  PROCEDURE, PUBLIC, PASS(obj) :: Display => als_display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => als_Import
  !! Importing linsolver from external file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => als_Export
  !! Exporting linsolver from external file
  PROCEDURE, PUBLIC, PASS(obj) :: GetPreconditionOption => &
    & als_getPreconditionOption
  !! Get precondition options
  PROCEDURE, PUBLIC, PASS(obj) :: setTolerance => &
    & als_setTolerance
  !! Set tolerance
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => als_GetParam
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => als_SetParam
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
  MODULE SUBROUTINE setAbstractLinSolverParam( &
    & param, &
    & prefix, &
    & engine, &
    & solverName, &
    & preconditionOption, &
    & maxIter, &
    & atol, &
    & rtol, &
    & convergenceIn, &
    & convergenceType, &
    & relativeToRHS, &
    & KrylovSubspaceSize, &
    & scale, &
    & initx_zeros, &
    & bicgstab_ell, &
    & sor_omega, &
    & p_name, &
    & p_ilu_lfil, &
    & p_ilu_mbloc, &
    & p_ilu_droptol, &
    & p_ilu_permtol, &
    & p_ilu_alpha, &
    & p_ilu_fill, &
    & p_ssor_omega, &
    & p_hybrid_i, &
    & p_hybrid_maxiter, &
    & p_hybrid_tol, &
    & p_hybrid_omega, &
    & p_hybrid_ell, &
    & p_hybrid_restart, &
    & p_is_alpha, &
    & p_is_m, &
    & p_sainv_drop, &
    & p_saamg_unsym, &
    & p_saamg_theta, &
    & p_iluc_drop, &
    & p_iluc_rate, &
    & p_adds, &
    & p_adds_iter &
    & )
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: solverName
    !! name of linear solver, it should be present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: preconditionOption
    !! precondition option
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_name
    !! if preconditionOption .ne. NO_PRECONDITION
    !! then p_name should be present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceIn
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativeToRHS
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: KrylovSubspaceSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: scale
    !! LIS, Solver digonal scaling
    !! scale_none: No scaling
    !! scale_jacobi: jacobi scaling inv(D)Ax = inv(D)b
    !! scale_symm_diag: sqrt(inv(D)) A sqrt(inv(D)) x = sqrt(inv(D))b
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: initx_zeros
    !! if True, then we set sol=0.0 as initial guess.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: bicgstab_ell
    !! Needed for solver BiCGSTABL
    REAL(DFP), OPTIONAL, INTENT(IN) :: sor_omega
    !! The relaxation coefficient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_lfil
    !! Sparsekit, ilu
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_mbloc
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_droptol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_permtol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_alpha
    !! Sparsekit, ilu, alpha
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_fill
    !! ILU, fill-in
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ssor_omega
    !! The relaxation coefficient omega in (0.0, 2.0)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_i
    !! Hybrid, the linear solver, for example, SSOR, GMRES,
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_maxiter
    !! Hybrid, maximum number of iterations
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_hybrid_tol
    !! Hybrid, convergence tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_hybrid_omega
    !! Hybrid, The relaxation coefficient omega of the SOR
    !! omega should be in (0.0, 2.0)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_ell
    !!Hybrid, The degree l of the BiCGSTAB(l)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_restart
    !! Hybrid, The restart value of GMRES and Orthomin
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_is_alpha
    !! I+S, The parameter alpha of $I + \alpha {S}^{m}$
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_is_m
    !! I+S, The parameter m of $I + \alpha {S}^{m}$
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_sainv_drop
    !! SA-AMG, The drop criteria
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_saamg_unsym
    !! SA-AMG, Select the unsymmetric version
    !! The matrix structure must be symmetric
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_saamg_theta
    !! SA-AMG, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_drop
    !! Crout ILU, default is 0.05, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_rate
    !! Crout ILU, The ratio of the maximum fill-in
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_adds
    !! ilut Additive Schwarz, default is true
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_adds_iter
    !! default value is 1
    !! ILUT Additive Schwarz number of iteration
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
  MODULE SUBROUTINE getAbstractLinSolverParam( &
    & param, &
    & prefix, &
    & engine, &
    & solverName, &
    & preconditionOption, &
    & maxIter, &
    & atol, &
    & rtol, &
    & convergenceIn, &
    & convergenceType, &
    & relativeToRHS, &
    & KrylovSubspaceSize, &
    & scale, &
    & initx_zeros, &
    & bicgstab_ell, &
    & sor_omega, &
    & p_name, &
    & p_ilu_lfil, &
    & p_ilu_mbloc, &
    & p_ilu_droptol, &
    & p_ilu_permtol, &
    & p_ilu_alpha, &
    & p_ilu_fill, &
    & p_ssor_omega, &
    & p_hybrid_i, &
    & p_hybrid_maxiter, &
    & p_hybrid_tol, &
    & p_hybrid_omega, &
    & p_hybrid_ell, &
    & p_hybrid_restart, &
    & p_is_alpha, &
    & p_is_m, &
    & p_sainv_drop, &
    & p_saamg_unsym, &
    & p_saamg_theta, &
    & p_iluc_drop, &
    & p_iluc_rate, &
    & p_adds, &
    & p_adds_iter &
    & )
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
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: scale
    !! LIS, Solver digonal scaling
    !! scale_none: No scaling
    !! scale_jacobi: jacobi scaling inv(D)Ax = inv(D)b
    !! scale_symm_diag: sqrt(inv(D)) A sqrt(inv(D)) x = sqrt(inv(D))b
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: initx_zeros
    !! if True, then we set sol=0.0 as initial guess.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: bicgstab_ell
    !! Needed for solver BiCGSTABL
    REAL(DFP), OPTIONAL, INTENT(OUT) :: sor_omega
    !! The relaxation coefficient
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_name
    !! Name of preconditioner
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_ilu_lfil
    !! Sparsekit, ilu
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_ilu_mbloc
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_ilu_droptol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_ilu_permtol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_ilu_alpha
    !! Sparsekit, ilu, alpha
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_ilu_fill
    !! ILU, fill-in
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_ssor_omega
    !! The relaxation coefficient omega in (0.0, 2.0)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_hybrid_i
    !! Hybrid, the linear solver, for example, SSOR, GMRES,
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_hybrid_maxiter
    !! Hybrid, maximum number of iterations
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_hybrid_tol
    !! Hybrid, convergence tolerance
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_hybrid_omega
    !! Hybrid, The relaxation coefficient omega of the SOR
    !! omega should be in (0.0, 2.0)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_hybrid_ell
    !!Hybrid, The degree l of the BiCGSTAB(l)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_hybrid_restart
    !! Hybrid, The restart value of GMRES and Orthomin
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_is_alpha
    !! I+S, The parameter alpha of $I + \alpha {S}^{m}$
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_is_m
    !! I+S, The parameter m of $I + \alpha {S}^{m}$
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_sainv_drop
    !! SA-AMG, The drop criteria
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: p_saamg_unsym
    !! SA-AMG, Select the unsymmetric version
    !! The matrix structure must be symmetric
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_saamg_theta
    !! SA-AMG, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_iluc_drop
    !! Crout ILU, default is 0.05, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(OUT) :: p_iluc_rate
    !! Crout ILU, The ratio of the maximum fill-in
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: p_adds
    !! ilut Additive Schwarz, default is true
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: p_adds_iter
    !! default value is 1
    !! ILUT Additive Schwarz number of iteration
  END SUBROUTINE getAbstractLinSolverParam
END INTERFACE

PUBLIC :: getAbstractLinSolverParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Initiate the linear solver

ABSTRACT INTERFACE
  SUBROUTINE als_Initiate(obj, param)
    IMPORT :: AbstractLinSolver_, ParameterList_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE als_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary:  Set the linear solver

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

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Solve the system of linear equations

ABSTRACT INTERFACE
  SUBROUTINE als_Solve(obj, sol, rhs)
    IMPORT :: AbstractLinSolver_, AbstractNodeField_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
  END SUBROUTINE als_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getLinSolverCodeFromName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get linear solver integer code from name

ABSTRACT INTERFACE
  FUNCTION als_getLinSolverCodeFromName(name) RESULT(Ans)
    IMPORT :: I4B
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION als_getLinSolverCodeFromName
END INTERFACE

!----------------------------------------------------------------------------
!                                      getLinSolverNameFromCode@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get the linear solver name from integer code

ABSTRACT INTERFACE
  FUNCTION als_getLinSolverNameFromCode(name) RESULT(Ans)
    IMPORT :: I4B
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(15) :: ans
  END FUNCTION als_getLinSolverNameFromCode
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Display the linear solver object

INTERFACE
  MODULE SUBROUTINE als_Display(obj, msg, unitno)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Unitno
  END SUBROUTINE als_Display
END INTERFACE

INTERFACE AbstractLinSolverDisplay
  MODULE PROCEDURE als_Display
END INTERFACE AbstractLinSolverDisplay

PUBLIC :: AbstractLinSolverDisplay

!----------------------------------------------------------------------------
!                                                             Import@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine intiates the linear solver from import

INTERFACE
  MODULE SUBROUTINE als_Import(obj, hdf5, group)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE als_Import
END INTERFACE

INTERFACE AbstractLinSolverImport
  MODULE PROCEDURE als_Import
END INTERFACE AbstractLinSolverImport

PUBLIC :: AbstractLinSolverImport

!----------------------------------------------------------------------------
!                                                             Export@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine exports the linear solver to external file

INTERFACE
  MODULE SUBROUTINE als_Export(obj, hdf5, group)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE als_Export
END INTERFACE

INTERFACE ExportAbstractLinSolver
  MODULE PROCEDURE als_Export
END INTERFACE ExportAbstractLinSolver

PUBLIC :: ExportAbstractLinSolver

!----------------------------------------------------------------------------
!                                                       Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Deallocate the linear solver

INTERFACE
  MODULE SUBROUTINE als_Deallocate(obj)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE als_Deallocate
END INTERFACE

INTERFACE AbstractLinSolverDeallocate
  MODULE PROCEDURE als_Deallocate
END INTERFACE AbstractLinSolverDeallocate

PUBLIC :: AbstractLinSolverDeallocate

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
!                                                          SetParam@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE als_SetParam(obj, isInitiated, engine, solverName, &
      & ierr, preconditionOption, iter, maxIter, atol, rtol, tol, normRes, &
      & error0, error, convergenceIn, convergenceType, relativeToRHS, &
      & KrylovSubspaceSize, globalNumRow, globalNumColumn, &
      & localNumRow, localNumColumn, RES, Amat)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isInitiated
    !! is object initiated?
    CHARACTER(*), OPTIONAL, INTENT(IN) :: engine
    !! Name of the engine
    !! NATIVE-SERIAL
    !! NATIVE-OMP
    !! NATIVE-ACC
    !! NATIVE-MPI
    !! PETSC
    !! LIS-OMP
    !! LIS-MPI
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: solverName
    !! Solver name
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ierr
    !! Error code returned by the solver
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: preconditionOption
    !! Name of preconditioner;
    !! NO_PRECONDITION
    !! LEFT_PRECONDITION
    !! RIGHT_PRECONDITION
    !! LEFT_RIGHT_PRECONDITON
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iter
    !! Current iteration number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! Maximum iteration number
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    !! absolute tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
    !! relative tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! Tolerance for testing convergence
    REAL(DFP), OPTIONAL, INTENT(IN) :: normRes
    !! norm Residual
    REAL(DFP), OPTIONAL, INTENT(IN) :: error0
    !! initial error res or sol
    REAL(DFP), OPTIONAL, INTENT(IN) :: error
    !! final error in res of sol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceIn
    !! convergence in residual or solution
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceType
    !! relative/ absolute convergence
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativeToRHS
    !! In case of relative convergence
    !! is convergence
    !! is relative to
    !! right hand side
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: KrylovSubspaceSize
    !! Useful for GMRES type algorithm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNumRow, globalNumColumn
    !! Size of the global problem;
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: localNumRow, localNumColumn
    !! Size of the problem on a single process
    REAL(DFP), OPTIONAL, INTENT(IN) :: RES(:)
    !! Residual in each iteration
    CLASS(AbstractMatrixField_), OPTIONAL, TARGET, INTENT(IN) :: Amat
    !! Pointer to child of [[AbstractMatrixField_]]
  END SUBROUTINE als_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetParam@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE als_GetParam(obj, isInitiated, engine, solverName, &
      & ierr, preconditionOption, iter, maxIter, atol, rtol, tol, normRes, &
      & error0, error, convergenceIn, convergenceType, relativeToRHS, &
      & KrylovSubspaceSize, globalNumRow, globalNumColumn, &
      & localNumRow, localNumColumn, RES, Amat)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isInitiated
    !! is object initiated?
    CHARACTER(*), OPTIONAL, INTENT(INOUT) :: engine
    !! Name of the engine
    !! NATIVE-SERIAL
    !! NATIVE-OMP
    !! NATIVE-ACC
    !! NATIVE-MPI
    !! PETSC
    !! LIS-OMP
    !! LIS-MPI
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: solverName
    !! Solver name
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: ierr
    !! Error code returned by the solver
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: preconditionOption
    !! Name of preconditioner;
    !! NO_PRECONDITION
    !! LEFT_PRECONDITION
    !! RIGHT_PRECONDITION
    !! LEFT_RIGHT_PRECONDITON
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: iter
    !! Current iteration number
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: maxIter
    !! Maximum iteration number
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: atol
    !! absolute tolerance
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: rtol
    !! relative tolerance
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: tol
    !! Tolerance for testing convergence
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: normRes
    !! norm Residual
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: error0
    !! initial error res or sol
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: error
    !! final error in res of sol
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: convergenceIn
    !! convergence in residual or solution
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: convergenceType
    !! relative/ absolute convergence
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: relativeToRHS
    !! In case of relative convergence
    !! is convergence
    !! is relative to
    !! right hand side
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: KrylovSubspaceSize
    !! Useful for GMRES type algorithm
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: globalNumRow, globalNumColumn
    !! Size of the global problem;
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localNumRow, localNumColumn
    !! Size of the problem on a single process
    REAL(DFP), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: RES(:)
    !! Residual in each iteration
    CLASS(AbstractMatrixField_), OPTIONAL, POINTER, INTENT(INOUT) :: Amat
    !! Pointer to child of [[AbstractMatrixField_]]
  END SUBROUTINE als_GetParam
END INTERFACE

END MODULE AbstractLinSolver_Class
