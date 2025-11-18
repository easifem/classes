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
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE LinSolverOpt_Class, ONLY: TypeLinSolverOpt, LinSolverOpt_

IMPLICIT NONE

PRIVATE

PUBLIC :: AbstractLinSolverImport
PUBLIC :: AbstractLinSolverDisplay
PUBLIC :: AbstractLinSolverPointer_
PUBLIC :: AbstractLinSolver_
PUBLIC :: AbstractLinSolverExport
PUBLIC :: AbstractLinSolverDeallocate
PUBLIC :: AbstractLinSolverInitiate
PUBLIC :: AbstractLinSolverImportFromToml

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
  PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  !! is object initiated?
  INTEGER(I4B) :: ierr = 0
  !! Error code returned by the solver
  INTEGER(I4B) :: iter = 0
  !! Current iteration number
  REAL(DFP) :: tol = 0.0_DFP
  !! Tolerance for testing convergence
  REAL(DFP) :: normRes = 0.0_DFP
  !! norm Residual
  REAL(DFP) :: error0 = 0.0_DFP
  !! initial error res or sol
  REAL(DFP) :: error = 0.0_DFP
  !! final error in res of sol
  TYPE(LinSolverOpt_) :: opt
  !! Linear solver option
  REAL(DFP), ALLOCATABLE :: res(:)
  !! Residual in each iteration
  CLASS(AbstractMatrixField_), POINTER :: amat => NULL()
  !! Pointer to child of [[AbstractMatrixField_]]

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate the object with arguments
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate Data

  ! SET:
  ! @SetMethods
  PROCEDURE(obj_Set), PUBLIC, DEFERRED, PASS(obj) :: Set
  !! Set the matrix and preconditioning matrix
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set param

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: IsInitiated => &
    obj_IsInitiated
  !! returns isInit
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: GetMatrixPointer => &
    obj_GetMatrixPointer
  !! Get pointer to amat
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get paramereters from abstractlin solver
  PROCEDURE(obj_solve), PUBLIC, DEFERRED, PASS(obj) :: Solve
  !! Solve system of linear equation

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_display
  !! Display the content

  ! IO:
  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Importing linsolver from external file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Exporting linsolver from external file

  ! IO:
  ! @TomlMethods
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Import abstract kernel from toml
END TYPE AbstractLinSolver_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractLinSolverPointer_
  CLASS(AbstractLinSolver_), POINTER :: ptr => NULL()
END TYPE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Initiate the linear solver

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, engine, solverName, preconditionOption, maxIter, atol, rtol, &
    convergenceIn, convergenceType, relativeToRHS, krylovSubspaceSize, &
    scale, initx_zeros, bicgstab_ell, sor_omega, p_name, p_ilu_lfil, &
    p_ilu_mbloc, p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, p_ilu_fill, &
    p_ssor_omega, p_hybrid_i, p_hybrid_maxiter, p_hybrid_tol, &
    p_hybrid_omega, p_hybrid_ell, p_hybrid_restart, p_is_alpha, p_is_m, &
    p_sainv_drop, p_saamg_unsym, p_saamg_theta, p_iluc_drop, p_iluc_rate, &
    p_adds, p_adds_iter)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: solverName
    !! name of linear solver, it should be present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: preconditionOption
    !! precondition option
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_name
    !! if preconditionOption .ne. NO_PRECONDITION
    !! then p_name should be present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceIn
    !! convergence in residual or solution
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceType
    !! relative or absolute convergence
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: scale
    !! LIS, Solver digonal scaling
    !! scale_none: No scaling
    !! scale_jacobi: jacobi scaling inv(D)Ax = inv(D)b
    !! scale_symm_diag: sqrt(inv(D)) A sqrt(inv(D)) x = sqrt(inv(D))b
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! maximum iteration allowed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: krylovSubspaceSize
    !! Size of KrylovSubspaceSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: bicgstab_ell
    !! Needed for solver BiCGSTABL
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_lfil
    !! Sparsekit, ilu
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_mbloc
    !! Sparsekit, ilu
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_fill
    !! ILU, fill-in
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_i
    !! Hybrid, the linear solver, for example, SSOR, GMRES,
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_maxiter
    !! Hybrid, maximum number of iterations
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_ell
    !!Hybrid, The degree l of the BiCGSTAB(l)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_restart
    !! Hybrid, The restart value of GMRES and Orthomin
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_is_m
    !! I+S, The parameter m of $I + \alpha {S}^{m}$
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_adds_iter
    !! default value is 1
    !! ILUT Additive Schwarz number of iteration
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    !! absolute tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
    !! relative tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: sor_omega
    !! The relaxation coefficient
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_is_alpha
    !! I+S, The parameter alpha of $I + \alpha {S}^{m}$
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_droptol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_permtol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_alpha
    !! Sparsekit, ilu, alpha
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ssor_omega
    !! The relaxation coefficient omega in (0.0, 2.0)
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_hybrid_tol
    !! Hybrid, convergence tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_hybrid_omega
    !! Hybrid, The relaxation coefficient omega of the SOR
    !! omega should be in (0.0, 2.0)
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_sainv_drop
    !! SA-AMG, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_saamg_theta
    !! SA-AMG, The drop criteria
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_saamg_unsym
    !! SA-AMG, Select the unsymmetric version
    !! The matrix structure must be symmetric
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_drop
    !! Crout ILU, default is 0.05, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_rate
    !! Crout ILU, The ratio of the maximum fill-in
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativeToRHS
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: initx_zeros
    !! if True, then we Set sol=0.0 as initial guess.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_adds
    !! ilut Additive Schwarz, default is true
  END SUBROUTINE obj_Initiate
END INTERFACE

INTERFACE AbstractLinSolverInitiate
  MODULE PROCEDURE obj_Initiate
END INTERFACE AbstractLinSolverInitiate

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary:  Set the linear solver

ABSTRACT INTERFACE
  SUBROUTINE obj_Set(obj, amat)
    IMPORT :: AbstractLinSolver_, AbstractMatrixField_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), TARGET, INTENT(INOUT) :: amat
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Solve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Solve the system of linear equations

ABSTRACT INTERFACE
  SUBROUTINE obj_Solve(obj, sol, rhs)
    IMPORT :: AbstractLinSolver_, AbstractNodeField_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
  END SUBROUTINE obj_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Display the linear solver object

INTERFACE AbstractLinSolverDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Unitno
  END SUBROUTINE obj_Display
END INTERFACE AbstractLinSolverDisplay

!----------------------------------------------------------------------------
!                                                   Import@ImportHDFMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine intiates the linear solver from import

INTERFACE AbstractLinSolverImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE AbstractLinSolverImport

!----------------------------------------------------------------------------
!                                         ImportFromToml@ImportTomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE AbstractLinSolverImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE AbstractLinSolverImportFromToml

!----------------------------------------------------------------------------
!                                           ImportFromToml@ImportTomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE AbstractLinSolverImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE AbstractLinSolverImportFromToml

!----------------------------------------------------------------------------
!                                                   Export@ExportHDFMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine exports the linear solver to external file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

INTERFACE AbstractLinSolverExport
  MODULE PROCEDURE obj_Export
END INTERFACE AbstractLinSolverExport

!----------------------------------------------------------------------------
!                                                       Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Deallocate the linear solver

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE AbstractLinSolverDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE AbstractLinSolverDeallocate

!----------------------------------------------------------------------------
!                                                         SetParam@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, isInitiated, engine, solverName, ierr, preconditionOption, iter, &
    maxIter, atol, rtol, tol, normRes, error0, error, convergenceIn, &
    convergenceType, relativeToRHS, krylovSubspaceSize, globalNumRow, &
    globalNumColumn, localNumRow, localNumColumn, scale, initx_zeros, &
    bicgstab_ell, sor_omega, p_name, p_ilu_lfil, p_ilu_mbloc, p_ilu_droptol, &
    p_ilu_permtol, p_ilu_alpha, p_ilu_fill, p_ssor_omega, p_hybrid_i, &
    p_hybrid_maxiter, p_hybrid_tol, p_hybrid_omega, p_hybrid_ell, &
    p_hybrid_restart, p_is_alpha, p_is_m, p_sainv_drop, p_saamg_unsym, &
    p_saamg_theta, p_iluc_drop, p_iluc_rate, p_adds, p_adds_iter, &
    res, amat)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isInitiated
    !! is object initiated?
    CHARACTER(*), OPTIONAL, INTENT(IN) :: engine
    !! Name of the engine
    !! NATIVE-SERIAL ! NATIVE-OMP ! NATIVE-ACC ! NATIVE-MPI ! PETSC
    !! LIS-OMP ! LIS-MPI
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: solverName
    !! Solver name
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ierr
    !! Error code returned by the solver
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: preconditionOption
    !! Name of preconditioner;
    !! NO_PRECONDITION ! LEFT_PRECONDITION ! RIGHT_PRECONDITION
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
    !! In case of relative convergence isConvergence is relative to
    !! right hand side
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: krylovSubspaceSize
    !! Useful for GMRES type algorithm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNumRow, globalNumColumn
    !! Size of the global problem;
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: localNumRow, localNumColumn
    !! Size of the problem on a single process
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: scale
    !! LIS, Solver digonal scaling
    !! scale_none: No scaling
    !! scale_jacobi: jacobi scaling inv(D)Ax = inv(D)b
    !! scale_symm_diag: sqrt(inv(D)) A sqrt(inv(D)) x = sqrt(inv(D))b
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: bicgstab_ell
    !! Needed for solver BiCGSTABL
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_lfil
    !! Sparsekit, ilu
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_mbloc
    !! Sparsekit, ilu
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_ilu_fill
    !! ILU, fill-in
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_i
    !! Hybrid, the linear solver, for example, SSOR, GMRES,
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_maxiter
    !! Hybrid, maximum number of iterations
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_ell
    !!Hybrid, The degree l of the BiCGSTAB(l)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_hybrid_restart
    !! Hybrid, The restart value of GMRES and Orthomin
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_is_m
    !! I+S, The parameter m of $I + \alpha {S}^{m}$
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_adds_iter
    !! default value is 1
    !! ILUT Additive Schwarz number of iteration
    REAL(DFP), OPTIONAL, INTENT(IN) :: sor_omega
    !! The relaxation coefficient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_name
    !! if preconditionOption .ne. NO_PRECONDITION
    !! then p_name should be present
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_is_alpha
    !! I+S, The parameter alpha of $I + \alpha {S}^{m}$
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_droptol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_permtol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ilu_alpha
    !! Sparsekit, ilu, alpha
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ssor_omega
    !! The relaxation coefficient omega in (0.0, 2.0)
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_hybrid_tol
    !! Hybrid, convergence tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_hybrid_omega
    !! Hybrid, The relaxation coefficient omega of the SOR
    !! omega should be in (0.0, 2.0)
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_sainv_drop
    !! SA-AMG, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_saamg_theta
    !! SA-AMG, The drop criteria
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_saamg_unsym
    !! SA-AMG, Select the unsymmetric version
    !! The matrix structure must be symmetric
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_drop
    !! Crout ILU, default is 0.05, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_rate
    !! Crout ILU, The ratio of the maximum fill-in
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: initx_zeros
    !! if True, then we Set sol=0.0 as initial guess.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_adds
    !! ilut Additive Schwarz, default is true
    REAL(DFP), OPTIONAL, INTENT(IN) :: res(:)
    !! Residual in each iteration
    CLASS(AbstractMatrixField_), OPTIONAL, TARGET, INTENT(IN) :: amat
    !! Pointer to child of [[AbstractMatrixField_]]
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-19
! summary: Returns isInit

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(AbstractLinSolver_), INTENT(in) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMatrixPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-21
! summary: Get pointer to amat

INTERFACE
  MODULE FUNCTION obj_GetMatrixPointer(obj) RESULT(ans)
    CLASS(AbstractLinSolver_), INTENT(in) :: obj
    CLASS(AbstractMatrixField_), POINTER :: ans
  END FUNCTION obj_GetMatrixPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary:  Get the parameters of the linear solver

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, isInitiated, engine, solverName, ierr, preconditionOption, iter, &
    maxIter, atol, rtol, tol, normRes, error0, error, convergenceIn, &
    convergenceType, relativeToRHS, krylovSubspaceSize, globalNumRow, &
    globalNumColumn, localNumRow, localNumColumn, scale, initx_zeros, &
    bicgstab_ell, sor_omega, p_name, p_ilu_lfil, p_ilu_mbloc, p_ilu_droptol, &
    p_ilu_permtol, p_ilu_alpha, p_ilu_fill, p_ssor_omega, p_hybrid_i, &
    p_hybrid_maxiter, p_hybrid_tol, p_hybrid_omega, p_hybrid_ell, &
    p_hybrid_restart, p_is_alpha, p_is_m, p_sainv_drop, p_saamg_unsym, &
    p_saamg_theta, p_iluc_drop, p_iluc_rate, p_adds, p_adds_iter, &
    res, amat)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isInitiated
    !! is object initiated?
    CHARACTER(*), OPTIONAL, INTENT(INOUT) :: engine
    !! Name of the engine
    !! NATIVE-SERIAL ! NATIVE-OMP ! NATIVE-ACC ! NATIVE-MPI ! PETSC
    !! LIS-OMP ! LIS-MPI
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: solverName
    !! Solver name
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: ierr
    !! Error code returned by the solver
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: preconditionOption
    !! Name of preconditioner;
    !! NO_PRECONDITION ! LEFT_PRECONDITION ! RIGHT_PRECONDITION
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
    !! In case of relative convergence isConvergence is relative to
    !! right hand side
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: krylovSubspaceSize
    !! Useful for GMRES type algorithm
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: globalNumRow, globalNumColumn
    !! Size of the global problem;
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localNumRow, localNumColumn
    !! Size of the problem on a single process
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: scale
    !! LIS, Solver digonal scaling
    !! scale_none: No scaling
    !! scale_jacobi: jacobi scaling inv(D)Ax = inv(D)b
    !! scale_symm_diag: sqrt(inv(D)) A sqrt(inv(D)) x = sqrt(inv(D))b
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: bicgstab_ell
    !! Needed for solver BiCGSTABL
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_ilu_lfil
    !! Sparsekit, ilu
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_ilu_mbloc
    !! Sparsekit, ilu
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_ilu_fill
    !! ILU, fill-in
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_hybrid_i
    !! Hybrid, the linear solver, for example, SSOR, GMRES,
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_hybrid_maxiter
    !! Hybrid, maximum number of iterations
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_hybrid_ell
    !!Hybrid, The degree l of the BiCGSTAB(l)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_hybrid_restart
    !! Hybrid, The restart value of GMRES and Orthomin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_is_m
    !! I+S, The parameter m of $I + \alpha {S}^{m}$
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_adds_iter
    !! default value is 1
    !! ILUT Additive Schwarz number of iteration
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: sor_omega
    !! The relaxation coefficient
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: p_name
    !! if preconditionOption .ne. NO_PRECONDITION
    !! then p_name should be present
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_is_alpha
    !! I+S, The parameter alpha of $I + \alpha {S}^{m}$
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_ilu_droptol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_ilu_permtol
    !! Sparsekit, ilu
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_ilu_alpha
    !! Sparsekit, ilu, alpha
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_ssor_omega
    !! The relaxation coefficient omega in (0.0, 2.0)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_hybrid_tol
    !! Hybrid, convergence tolerance
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_hybrid_omega
    !! Hybrid, The relaxation coefficient omega of the SOR
    !! omega should be in (0.0, 2.0)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_sainv_drop
    !! SA-AMG, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_saamg_theta
    !! SA-AMG, The drop criteria
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: p_saamg_unsym
    !! SA-AMG, Select the unsymmetric version
    !! The matrix structure must be symmetric
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_iluc_drop
    !! Crout ILU, default is 0.05, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: p_iluc_rate
    !! Crout ILU, The ratio of the maximum fill-in
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: initx_zeros
    !! if True, then we Set sol=0.0 as initial guess.
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: p_adds
    !! ilut Additive Schwarz, default is true
    REAL(DFP), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: res(:)
    !! Residual in each iteration
    CLASS(AbstractMatrixField_), OPTIONAL, POINTER, INTENT(INOUT) :: amat
    !! Pointer to child of [[AbstractMatrixField_]]
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractLinSolver_Class
