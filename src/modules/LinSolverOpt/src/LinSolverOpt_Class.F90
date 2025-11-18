! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE LinSolverOpt_Class
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: TypePrecondOpt, TypeConvergenceOpt, TypeSolverNameOpt
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table
USE GlobalData, ONLY: DFP, I4B, LGT, &
                      LIS_CG, NO_PRECONDITION, PRECOND_NONE, &
                      convergenceInRes, relativeConvergence, &
                      LIS_SOR

IMPLICIT NONE

PRIVATE

PUBLIC :: LinSolverOpt_, TypeLinSolverOpt

CHARACTER(*), PARAMETER :: modName = "LinSolverOpt_Class"

!----------------------------------------------------------------------------
!                                                              LinSolverOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Linear solver options classes

TYPE :: LinSolverOpt_
  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is true when the object is intiated
  LOGICAL(LGT) :: relativeToRHS = .FALSE.
  !! relative to RHS vector
  LOGICAL(LGT) :: initx_zeros = .TRUE.
  !! initx_zeros for LIS
  LOGICAL(LGT) :: saamg_unsym = .FALSE.
  !! SAAMG unsymmetric option
  LOGICAL(LGT) :: adds = .TRUE.
  !! ADDS option

  CHARACTER(128) :: engine = "NATIVE_SERIAL"
  !! engine name
  CHARACTER(128) :: solverName_char = "CG"
  !! solverName as string
  CHARACTER(128) :: preconditionOption_char = "NO"
  !! precondition option as string
  CHARACTER(128) :: p_name_char = "NONE"
  !! precondition name as string
  CHARACTER(128) :: convergenceIn_char = "RESIDUAL"
  !! convergence in residual or solution as string
  CHARACTER(128) :: convergenceType_char = "RELATIVE"
  !! convergence type as string
  CHARACTER(128) :: scale_char = "NONE"
  !! scaling option as string
  CHARACTER(128) :: hybrid_i_char = "SOR"
  !! hybrid solver as string

  INTEGER(I4B) :: solverName = TypeSolverNameOpt%cg
  !! solver name as integer code
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

  INTEGER(I4B) :: preconditionOption = TypePrecondOpt%NONE
  !! precondition option as integer code
  INTEGER(I4B) :: p_name = TypePrecondOpt%NONE
  !! precondition name as integer code
  INTEGER(I4B) :: convergenceIn = TypeConvergenceOpt%res
  !! convergence in residual or solution as integer code
  INTEGER(I4B) :: convergenceType = TypeConvergenceOpt%relative
  !! convergence type as integer code
  INTEGER(I4B) :: maxIter = 1000
  !! maximum number of iterations
  INTEGER(I4B) :: krylovSubspaceSize = 50
  !! size of the Krylov subspace
  INTEGER(I4B) :: bicgstab_ell = 2
  !! bicg stab ell parameter
  INTEGER(I4B) :: scale_none = 0
  !! no diagonal scaling
  INTEGER(I4B) :: scale_jacobi = 1
  !! jacobi scaling
  INTEGER(I4B) :: scale_symm_diag = 2
  !! symmetric diagonal scaling
  INTEGER(I4B) :: scale = 0
  !! scaling option: scale_none, scale_jacobi, scale_symm_diag
  INTEGER(I4B) :: ilu_lfil = 5
  !! Incomplete LU precondition option
  INTEGER(I4B) :: ilu_mbloc = 10
  !! Incomplete LU precondition option
  INTEGER(I4B) :: ilu_fill = 5
  !! Incomplete LU precondition option
  INTEGER(I4B) :: hybrid_i = TypeSolverNameOpt%sor
  !! option for hybrid preconditioner
  INTEGER(I4B) :: hybrid_maxiter = 25
  !! maximum iterations for hybrid preconditioner
  INTEGER(I4B) :: hybrid_ell = 2
  !! ell parameter for hybrid preconditioner
  INTEGER(I4B) :: hybrid_restart = 40
  !! restart parameter for hybrid preconditioner
  INTEGER(I4B) :: is_m = 3
  !! LIS preconditioner (TODO)
  INTEGER(I4B) :: adds_iter = 1
  !! ADDS iter

  REAL(DFP) :: rtol = 1.0E-8
  !! relative tolerance
  REAL(DFP) :: atol = 1.0E-8
  !! absolute tolerance
  REAL(DFP) :: sor_omega = 1.9_DFP
  !! sor omega parameter
  REAL(DFP) :: ilu_droptol = 0.005_DFP
  !! Incomplete LU precondition option
  REAL(DFP) :: ilu_permtol = 0.1_DFP
  !! Incomplete LU precondition option
  REAL(DFP) :: ilu_alpha = 1.0_DFP
  !! Incomplete LU precondition option
  REAL(DFP) :: ssor_omega = 1.0_DFP
  !! SSOR omega parameter
  REAL(DFP) :: hybrid_tol = 1.0E-3
  !! tolerance for hybrid preconditioner
  REAL(DFP) :: hybrid_omega = 1.5
  !! omega for hybrid preconditioner
  REAL(DFP) :: is_alpha = 1.0
  !! Preconditioner parameter (TODO)
  REAL(DFP) :: sainv_drop = 0.05
  !! SAINV drop tolerance
  REAL(DFP) :: saamg_theta = 0.05
  !! SAAMG theta parameter
  REAL(DFP) :: iluc_drop = 0.05
  !! ILUIC drop tolerance
  REAL(DFP) :: iluc_rate = 5.0
  !! ILUC rate parameter

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate the linear solver options
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the linear solver options

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set parameters of linear solver options
  PROCEDURE, PUBLIC, PASS(obj) :: SetTolerance => obj_SetTolerance
  !! Set tolerance of linear solver options
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondILU => obj_SetPrecondILU
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondHybrid => obj_SetPrecondHybrid
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondIS => obj_SetPrecondIS
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondADDS => obj_SetPrecondADDS
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondSSOR => obj_SetPrecondSSOR
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondSAINV => obj_SetPrecondSAINV
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondSAAMG => obj_SetPrecondSAAMG
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondILUC => obj_SetPrecondILUC

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SolverNameToInteger => &
    obj_SolverNameToInteger
  !! Convert solver name to integer code
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SolverIntegerToName => &
    obj_SolverIntegerToName
  !! Convert solver integer code to string
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: PrecondOptToInteger => &
    obj_PrecondOptToInteger
  !! Convert precondition option to integer code
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: ConvergenceInToInteger => &
    obj_ConvergenceInToInteger
  !! Convert convergence in to integer code
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    ConvergenceTypeToInteger => obj_ConvergenceTypeToInteger
  !! Convert convergence type to integer code
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: PrecondNameToInteger => &
    obj_PrecondNameToInteger
  !! Convert precondition name to integer code
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ScaleToInteger => &
    obj_ScaleToInteger
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Return isInit
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetPreconditionOption => obj_GetPreconditionOption
  !! Get precondition options
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the field values

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the linear solver options

  !IO:
  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import linear solver options from toml file
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import linear solver options from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
END TYPE LinSolverOpt_

!----------------------------------------------------------------------------
!                                                           TypeLinSolverOpt
!----------------------------------------------------------------------------

TYPE(LinSolverOpt_), PARAMETER :: TypeLinSolverOpt = LinSolverOpt_()

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Initiate the linear solver options

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, engine, solverName, preconditionOption, maxIter, atol, rtol, &
    convergenceIn, convergenceType, relativeToRHS, KrylovSubspaceSize, &
    scale, initx_zeros, bicgstab_ell, sor_omega, p_name, p_ilu_lfil, &
    p_ilu_mbloc, p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, p_ilu_fill, &
    p_ssor_omega, p_hybrid_i, p_hybrid_maxiter, p_hybrid_tol, &
    p_hybrid_omega, p_hybrid_ell, p_hybrid_restart, p_is_alpha, p_is_m, &
    p_sainv_drop, p_saamg_unsym, p_saamg_theta, p_iluc_drop, p_iluc_rate, &
    p_adds, p_adds_iter)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: engine
    !! Engine name
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

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary:  Deallocate the linear solver options

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetTolerance@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set tolerance of linear solver options

INTERFACE
  MODULE SUBROUTINE obj_SetTolerance(obj, atol, rtol)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
  END SUBROUTINE obj_SetTolerance
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set tolerance of linear solver options

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, isInitiated, engine, solverName, preconditionOption, &
    maxIter, atol, rtol, convergenceIn, convergenceType, relativeToRHS, &
    krylovSubspaceSize, globalNumRow, globalNumColumn, localNumRow, &
    localNumColumn, scale, initx_zeros, bicgstab_ell, sor_omega, p_name, &
    p_ilu_lfil, p_ilu_mbloc, p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, &
    p_ilu_fill, p_ssor_omega, p_hybrid_i, p_hybrid_maxiter, p_hybrid_tol, &
    p_hybrid_omega, p_hybrid_ell, p_hybrid_restart, p_is_alpha, p_is_m, &
    p_sainv_drop, p_saamg_unsym, p_saamg_theta, p_iluc_drop, p_iluc_rate, &
    p_adds, p_adds_iter)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isInitiated
    !! is object initiated?
    CHARACTER(*), OPTIONAL, INTENT(IN) :: engine
    !! Name of the engine
    !! NATIVE-SERIAL ! NATIVE-OMP ! NATIVE-ACC ! NATIVE-MPI ! PETSC
    !! LIS-OMP ! LIS-MPI
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: solverName
    !! Solver name
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: preconditionOption
    !! Name of preconditioner;
    !! NO_PRECONDITION ! LEFT_PRECONDITION ! RIGHT_PRECONDITION
    !! LEFT_RIGHT_PRECONDITON
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! Maximum iteration number
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    !! absolute tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
    !! relative tolerance
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
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetPrecondILU@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set the parameters for ILU preconditioner

INTERFACE
  MODULE SUBROUTINE obj_SetPrecondILU( &
    obj, p_ilu_lfil, p_ilu_mbloc, p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, &
    p_ilu_fill)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    !! Linear solver options
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
  END SUBROUTINE obj_SetPrecondILU
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetPrecondHybrid@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set the parameters for Hybrid preconditioner

INTERFACE
  MODULE SUBROUTINE obj_SetPrecondHybrid( &
    obj, p_hybrid_i, p_hybrid_maxiter, p_hybrid_tol, p_hybrid_omega, &
    p_hybrid_ell, p_hybrid_restart)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
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
  END SUBROUTINE obj_SetPrecondHybrid
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetPrecondIS@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary:  Set the parameters for I+S preconditioner

INTERFACE
  MODULE SUBROUTINE obj_SetPrecondIS(obj, p_is_m, p_is_alpha)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_is_alpha
    !! I+S, The parameter alpha of $I + \alpha {S}^{m}$
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_is_m
    !! I+S, The parameter m of $I + \alpha {S}^{m}$
  END SUBROUTINE obj_SetPrecondIS
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetPrecondADDS@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set the parameters for Additive Schwarz Preconditioner

INTERFACE
  MODULE SUBROUTINE obj_SetPrecondADDS(obj, p_adds_iter, p_adds)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_adds_iter
    !! default value is 1
    !! ILUT Additive Schwarz number of iteration
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_adds
    !! ilut Additive Schwarz, default is true
  END SUBROUTINE obj_SetPrecondADDS
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetPrecondSSOR@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set the parameters for SSOR preconditioner

INTERFACE
  MODULE SUBROUTINE obj_SetPrecondSSOR(obj, p_ssor_omega)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_ssor_omega
    !! The relaxation coefficient omega in (0.0, 2.0)
  END SUBROUTINE obj_SetPrecondSSOR
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetPrecondSAINV@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set the parameters for SAINV preconditioner

INTERFACE
  MODULE SUBROUTINE obj_SetPrecondSAINV(obj, p_sainv_drop)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_sainv_drop
    !! SA-AMG, The drop criteria
  END SUBROUTINE obj_SetPrecondSAINV
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetPrecondSAAMG@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set the parameters for SAAMG preconditioner

INTERFACE
  MODULE SUBROUTINE obj_SetPrecondSAAMG(obj, p_saamg_theta, p_saamg_unsym)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    !! Linear solver options
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_saamg_theta
    !! SA-AMG, The drop criteria
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: p_saamg_unsym
    !! SA-AMG, Select the unsymmetric version
    !! The matrix structure must be symmetric
  END SUBROUTINE obj_SetPrecondSAAMG
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetPrecondILUC@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Set the parameters for ILUC preconditioner

INTERFACE
  MODULE SUBROUTINE obj_SetPrecondILUC(obj, p_iluc_drop, p_iluc_rate)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_drop
    !! Crout ILU, default is 0.05, The drop criteria
    REAL(DFP), OPTIONAL, INTENT(IN) :: p_iluc_rate
    !! Crout ILU, The ratio of the maximum fill-in
  END SUBROUTINE obj_SetPrecondILUC
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Display the linear solver options

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                              SolverNameToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  solver name to integer

INTERFACE
  MODULE FUNCTION obj_SolverNameToInteger(obj, name) RESULT(ans)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_SolverNameToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                              SolverIntegerToName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get the linear solver name from integer code

INTERFACE
  MODULE FUNCTION obj_SolverIntegerToName(obj, name) RESULT(Ans)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_SolverIntegerToName
END INTERFACE

!----------------------------------------------------------------------------
!                                              PrecondOptToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  solver name to integer

INTERFACE
  MODULE FUNCTION obj_PrecondOptToInteger(obj, name) RESULT(ans)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_PrecondOptToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                           ConvergenceInToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  solver name to integer

INTERFACE
  MODULE FUNCTION obj_ConvergenceInToInteger(obj, name) RESULT(ans)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_ConvergenceInToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                         ConvergenceTypeToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  solver name to integer

INTERFACE
  MODULE FUNCTION obj_ConvergenceTypeToInteger(obj, name) RESULT(ans)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_ConvergenceTypeToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ScaleToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  solver name to integer

INTERFACE
  MODULE FUNCTION obj_ScaleToInteger(obj, name) RESULT(ans)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_ScaleToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                             PrecondNameToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-09
! summary:  solver name to integer

INTERFACE
  MODULE FUNCTION obj_PrecondNameToInteger(obj, name) RESULT(ans)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_PrecondNameToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Get IsInit from the object

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(LinSolverOpt_), INTENT(in) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetPreconditionOption@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 Sept 2021
! summary: Returns the preconditionOption

INTERFACE
  MODULE FUNCTION obj_GetPreconditionOption(obj) RESULT(Ans)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetPreconditionOption
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-18
! summary: Get the parameters of linear solver options

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, isInitiated, engine, solverName, preconditionOption, &
    maxIter, atol, rtol, convergenceIn, convergenceType, relativeToRHS, &
    krylovSubspaceSize, globalNumRow, globalNumColumn, localNumRow, &
    localNumColumn, scale, initx_zeros, bicgstab_ell, sor_omega, p_name, &
    p_ilu_lfil, p_ilu_mbloc, p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, &
    p_ilu_fill, p_ssor_omega, p_hybrid_i, p_hybrid_maxiter, p_hybrid_tol, &
    p_hybrid_omega, p_hybrid_ell, p_hybrid_restart, p_is_alpha, p_is_m, &
    p_sainv_drop, p_saamg_unsym, p_saamg_theta, p_iluc_drop, p_iluc_rate, &
    p_adds, p_adds_iter)
    CLASS(LinSolverOpt_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isInitiated
    !! is object initiated?
    CHARACTER(*), OPTIONAL, INTENT(INOUT) :: engine
    !! Name of the engine
    !! NATIVE-SERIAL ! NATIVE-OMP ! NATIVE-ACC ! NATIVE-MPI ! PETSC
    !! LIS-OMP ! LIS-MPI
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: solverName
    !! Solver name
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: preconditionOption
    !! Name of preconditioner;
    !! NO_PRECONDITION ! LEFT_PRECONDITION ! RIGHT_PRECONDITION
    !! LEFT_RIGHT_PRECONDITON
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: maxIter
    !! Maximum iteration number
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: atol
    !! absolute tolerance
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: rtol
    !! relative tolerance
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
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary: Initiate object from toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                           ImportFromToml@ImportTomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary: Initiate object from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LinSolverOpt_Class
