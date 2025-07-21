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
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE LinSolverOpt_Class, ONLY: TypeLinSolverOpt

IMPLICIT NONE

PRIVATE

PUBLIC :: AbstractLinSolverImport
PUBLIC :: AbstractLinSolverDisplay
PUBLIC :: GetAbstractLinSolverParam
PUBLIC :: AbstractLinSolverPointer_
PUBLIC :: AbstractLinSolver_
PUBLIC :: ExportAbstractLinSolver
PUBLIC :: AbstractLinSolverExport
PUBLIC :: AbstractLinSolverDeallocate
PUBLIC :: AbstractLinSolverImportParamFromToml
PUBLIC :: AbstractLinSolverImportFromToml
PUBLIC :: SetAbstractLinSolverParam
PUBLIC :: SetPrecondIluParam
PUBLIC :: SetPrecondHybridParam
PUBLIC :: SetPrecondIsParam
PUBLIC :: SetPrecondAddsParam
PUBLIC :: SetPrecondSsorParam
PUBLIC :: SetPrecondSainvParam
PUBLIC :: SetPrecondSaamgParam
PUBLIC :: SetPrecondIlucParam

CHARACTER(*), PARAMETER :: modName = "AbstractLinSolver_Class"
CHARACTER(*), PARAMETER :: myprefix = "AbstractLinSolver"

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
  TYPE(String) :: engine
  !! Name of the engine
  !! NATIVE_SERIAL ! NATIVE_OMP ! NATIVE_ACC ! NATIVE_MPI ! PETSC ! LIS_OMP
  !! LIS_MPI
  INTEGER(I4B) :: solverName = TypeLinSolverOpt%solverName
  !! Solver name
  INTEGER(I4B) :: ierr = 0
  !! Error code returned by the solver
  INTEGER(I4B) :: preconditionOption = TypeLinSolverOpt%preconditionOption
  !! Name of preconditioner;
  !! NO_PRECONDITION
  !! LEFT_PRECONDITION
  !! RIGHT_PRECONDITION
  !! LEFT_RIGHT_PRECONDITON
  INTEGER(I4B) :: iter = 0
  !! Current iteration number
  INTEGER(I4B) :: maxIter = TypeLinSolverOpt%maxIter
  !! Maximum iteration number
  REAL(DFP) :: atol = TypeLinSolverOpt%atol
  !! absolute tolerance
  REAL(DFP) :: rtol = TypeLinSolverOpt%rtol
  !! relative tolerance
  REAL(DFP) :: tol = 0.0_DFP
  !! Tolerance for testing convergence
  REAL(DFP) :: normRes = 0.0_DFP
  !! norm Residual
  REAL(DFP) :: error0 = 0.0_DFP
  !! initial error res or sol
  REAL(DFP) :: error = 0.0_DFP
  !! final error in res of sol
  INTEGER(I4B) :: convergenceIn = TypeLinSolverOpt%convergenceIn
  !! convergence in residual or solution
  INTEGER(I4B) :: convergenceType = TypeLinSolverOpt%convergenceType
  !! relative/ absolute convergence
  LOGICAL(LGT) :: relativeToRHS = TypeLinSolverOpt%relativeToRHS
  !! In case of relative convergence
  !! is convergence
  !! is relative to
  !! right hand side
  INTEGER(I4B) :: KrylovSubspaceSize = TypeLinSolverOpt%KrylovSubspaceSize
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
  REAL(DFP), ALLOCATABLE :: res(:)
  !! Residual in each iteration
  CLASS(AbstractMatrixField_), POINTER :: amat => NULL()
  !! Pointer to child of [[AbstractMatrixField_]]

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! Check essential parameters

  PROCEDURE(obj_initiate), PUBLIC, DEFERRED, PASS(obj) :: Initiate
  !! Initiate the object

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate Data

  ! SET:
  ! @SetMethods

  PROCEDURE(obj_Set), PUBLIC, DEFERRED, PASS(obj) :: Set
  !! Set the matrix and preconditioning matrix

  PROCEDURE, PUBLIC, PASS(obj) :: SetTolerance => obj_SetTolerance
  !! Set the tolerance

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

  PROCEDURE(obj_GetLinSolverCodeFromName), DEFERRED, PUBLIC, NOPASS :: &
    GetLinSolverCodeFromName

  PROCEDURE(obj_GetLinSolverNameFromCode), DEFERRED, PUBLIC, NOPASS :: &
    GetLinSolverNameFromCode

  PROCEDURE, PUBLIC, PASS(obj) :: GetPreconditionOption => &
    obj_GetPreconditionOption
  !! Get precondition options

  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get paramereters from abstractlin solver

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Get the prefix

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: solverName_ToInteger
  !! Convert solver name to integer code

  PROCEDURE(obj_solve), PUBLIC, DEFERRED, PASS(obj) :: Solve
  !! Solve system of linear equation

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_display
  !! Display the content

  ! IO:
  ! @ImportHDFMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Importing linsolver from external file

  ! IO:
  ! @ImportTomlMethods

  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2

  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Import abstract kernel from toml

  PROCEDURE, PUBLIC, PASS(obj) :: ImportParamFromToml => &
    obj_ImportParamFromToml
  !! Import parameters for TOML file

  ! IO:
  ! @ExportMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Exporting linsolver from external file
END TYPE AbstractLinSolver_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractLinSolverPointer_
  CLASS(AbstractLinSolver_), POINTER :: ptr => NULL()
END TYPE

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine Checks the essential parameters

INTERFACE
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetLinSolverParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Jan 2022
! summary: Set linear solver parameters

INTERFACE
  MODULE SUBROUTINE SetAbstractLinSolverParam(param, prefix, engine, &
         solverName, preconditionOption, maxIter, atol, rtol, convergenceIn, &
     convergenceType, relativeToRHS, KrylovSubspaceSize, scale, initx_zeros, &
    bicgstab_ell, sor_omega, p_name, p_ilu_lfil, p_ilu_mbloc, p_ilu_droptol, &
           p_ilu_permtol, p_ilu_alpha, p_ilu_fill, p_ssor_omega, p_hybrid_i, &
               p_hybrid_maxiter, p_hybrid_tol, p_hybrid_omega, p_hybrid_ell, &
          p_hybrid_restart, p_is_alpha, p_is_m, p_sainv_drop, p_saamg_unsym, &
                 p_saamg_theta, p_iluc_drop, p_iluc_rate, p_adds, p_adds_iter)

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
  END SUBROUTINE SetAbstractLinSolverParam
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLinSolverParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Jan 2022
! summary: Set linear solver parameters

INTERFACE
  MODULE SUBROUTINE GetAbstractLinSolverParam(param, prefix, engine, &
         solverName, preconditionOption, maxIter, atol, rtol, convergenceIn, &
     convergenceType, relativeToRHS, KrylovSubspaceSize, scale, initx_zeros, &
                   bicgstab_ell, sor_omega, p_name, p_ilu_lfil, p_ilu_mbloc, &
        p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, p_ilu_fill, p_ssor_omega, &
                 p_hybrid_i, p_hybrid_maxiter, p_hybrid_tol, p_hybrid_omega, &
           p_hybrid_ell, p_hybrid_restart, p_is_alpha, p_is_m, p_sainv_drop, &
             p_saamg_unsym, p_saamg_theta, p_iluc_drop, p_iluc_rate, p_adds, &
                                              p_adds_iter)

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
    !! if True, then we Set sol=0.0 as initial guess.
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
  END SUBROUTINE GetAbstractLinSolverParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetPrecondIluParam
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-27
! summary:  Set param for ilu preconditioner
!
! note : param should be allocated

INTERFACE
  MODULE SUBROUTINE SetPrecondIluParam(param, prefix, p_ilu_lfil, &
           p_ilu_mbloc, p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, p_ilu_fill)

    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
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
  END SUBROUTINE SetPrecondIluParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetPrecondHybridParam
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-27
! summary:  Set param for hybrid preconditioner
!
! note : param should be allocated

INTERFACE
  MODULE SUBROUTINE SetPrecondHybridParam(param, prefix, p_hybrid_i, &
               p_hybrid_maxiter, p_hybrid_tol, p_hybrid_omega, p_hybrid_ell, &
                                          p_hybrid_restart)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
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
  END SUBROUTINE SetPrecondHybridParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetPrecondIsParam
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-27
! summary:  Set param for Is preconditioner
!
! note : param should be allocated

INTERFACE
  MODULE SUBROUTINE SetPrecondIsParam(param, prefix, p_is_m, p_is_alpha)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_is_alpha
    !! I+S, The parameter alpha of $I + \alpha {S}^{m}$
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_is_m
    !! I+S, The parameter m of $I + \alpha {S}^{m}$
  END SUBROUTINE SetPrecondIsParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetPrecondAddsParam
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-27
! summary:  Set param for Adds preconditioner
!
! note : param should be allocated

INTERFACE
  MODULE SUBROUTINE SetPrecondAddsParam(param, prefix, p_adds_iter, p_adds)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_adds_iter
    !! default value is 1
    !! ILUT Additive Schwarz number of iteration
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_adds
    !! ilut Additive Schwarz, default is true
  END SUBROUTINE SetPrecondAddsParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetPrecondSsorParam
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-27
! summary:  Set param for SSOR preconditioner
!
! note : param should be allocated

INTERFACE
  MODULE SUBROUTINE SetPrecondSsorParam(param, prefix, p_ssor_omega)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ssor_omega
    !! The relaxation coefficient omega in (0.0, 2.0)
  END SUBROUTINE SetPrecondSsorParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetPrecondSainvParam
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-27
! summary:  Set param for Sainv preconditioner
!
! note : param should be allocated

INTERFACE
  MODULE SUBROUTINE SetPrecondSainvParam(param, prefix, p_sainv_drop)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_sainv_drop
    !! SA-AMG, The drop criteria
  END SUBROUTINE SetPrecondSainvParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetPrecondSaamgParam
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-27
! summary:  Set param for Saamg preconditioner
!
! note : param should be allocated

INTERFACE
  MODULE SUBROUTINE SetPrecondSaamgParam(param, prefix, p_saamg_theta, &
                                         p_saamg_unsym)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_saamg_theta
    !! SA-AMG, The drop criteria
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_saamg_unsym
    !! SA-AMG, Select the unsymmetric version
    !! The matrix structure must be symmetric
  END SUBROUTINE SetPrecondSaamgParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetPrecondIlucParam
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-27
! summary:  Set param for Iluc preconditioner
!
! note : param should be allocated

INTERFACE
  MODULE SUBROUTINE SetPrecondIlucParam(param, prefix, p_iluc_drop, &
                                        p_iluc_rate)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_drop
    !! Crout ILU, default is 0.05, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_rate
    !! Crout ILU, The ratio of the maximum fill-in
  END SUBROUTINE SetPrecondIlucParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Initiate the linear solver

ABSTRACT INTERFACE
  SUBROUTINE obj_Initiate(obj, param)
    IMPORT :: AbstractLinSolver_, ParameterList_
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

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
!                                                  GetLinSolverCodeFromName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get linear solver integer code from name

ABSTRACT INTERFACE
  FUNCTION obj_GetLinSolverCodeFromName(name) RESULT(Ans)
    IMPORT :: I4B
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetLinSolverCodeFromName
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetLinSolverNameFromCode@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get the linear solver name from integer code

ABSTRACT INTERFACE
  FUNCTION obj_GetLinSolverNameFromCode(name) RESULT(Ans)
    IMPORT :: I4B
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(15) :: ans
  END FUNCTION obj_GetLinSolverNameFromCode
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
!                                     ImportParamFromToml@ImportTomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param by reading the toml table

INTERFACE AbstractLinSolverImportParamFromToml
  MODULE SUBROUTINE obj_ImportParamFromToml(obj, param, table)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportParamFromToml
END INTERFACE AbstractLinSolverImportParamFromToml

!----------------------------------------------------------------------------
!                                         ImportFromToml@ImportTomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE AbstractLinSolverImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE AbstractLinSolverImportFromToml

!----------------------------------------------------------------------------
!                                           ImportFromToml@ImportTomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE AbstractLinSolverImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE AbstractLinSolverImportFromToml

!----------------------------------------------------------------------------
!                                                   Export@ExportHDFMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine exports the linear solver to external file

INTERFACE ExportAbstractLinSolver
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE ExportAbstractLinSolver

INTERFACE AbstractLinSolverExport
  MODULE PROCEDURE obj_Export
END INTERFACE AbstractLinSolverExport

!----------------------------------------------------------------------------
!                                                       Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Deallocate the linear solver

INTERFACE AbstractLinSolverDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractLinSolverDeallocate

!----------------------------------------------------------------------------
!                                            GetPreconditionOption@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 Sept 2021
! summary: Returns the preconditionOption

INTERFACE
  MODULE FUNCTION obj_GetPreconditionOption(obj) RESULT(Ans)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetPreconditionOption
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetPreconditionOption@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 Sept 2021
! summary: Returns the preconditionOption

INTERFACE
  MODULE SUBROUTINE obj_SetTolerance(obj, atol, rtol)
    CLASS(AbstractLinSolver_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
  END SUBROUTINE obj_SetTolerance
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetParam@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, isInitiated, engine, solverName, &
          ierr, preconditionOption, iter, maxIter, atol, rtol, tol, normRes, &
               error0, error, convergenceIn, convergenceType, relativeToRHS, &
                          KrylovSubspaceSize, globalNumRow, globalNumColumn, &
                                 localNumRow, localNumColumn, RES, Amat)
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

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, isInitiated, engine, solverName, &
          ierr, preconditionOption, iter, maxIter, atol, rtol, tol, normRes, &
               error0, error, convergenceIn, convergenceType, relativeToRHS, &
                          KrylovSubspaceSize, globalNumRow, globalNumColumn, &
                                 localNumRow, localNumColumn, RES, Amat)
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
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  Get the prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                           solverName_ToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  solver name to integer

INTERFACE
  MODULE FUNCTION solverName_ToInteger(obj, name) RESULT(ans)
    CLASS(AbstractLinSolver_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION solverName_ToInteger
END INTERFACE

END MODULE AbstractLinSolver_Class
