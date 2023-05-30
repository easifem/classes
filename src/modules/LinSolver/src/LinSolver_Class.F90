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
USE ExceptionHandler_Class, ONLY: e
USE Field
USE AbstractLinSolver_Class
USE HDF5File_Class
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "Linsolver_Class"
CHARACTER(*), PARAMETER :: myprefix = "Linsolver"
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
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & ls_checkEssentialParam
    !! Check essential parameters
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => ls_Initiate
    !! Initiate object
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => ls_Deallocate
  !! Deallocate Data
  FINAL :: ls_final
  !! Final
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => ls_Import
  !! Importing linsolver from external file
  PROCEDURE, PUBLIC, PASS(obj) :: Display => ls_Display
  !! Display
  PROCEDURE, PUBLIC, PASS(obj) :: Set => ls_Set
  !! Set the matrix and preconditioning matrix
  PROCEDURE, PUBLIC, PASS(obj) :: Solve => ls_solve
  !! Solve the system of linear equation
  PROCEDURE, PUBLIC, NOPASS :: &
     & getLinSolverCodeFromName => ls_getLinSolverCodeFromName
  PROCEDURE, PUBLIC, NOPASS :: &
     & getLinSolverNameFromCode => ls_getLinSolverNameFromCode
END TYPE LinSolver_

PUBLIC :: LinSolver_

!----------------------------------------------------------------------------
!                                                              TypeLinSolver
!----------------------------------------------------------------------------

TYPE(LinSolver_), PUBLIC, PARAMETER :: TypeLinSolver = LinSolver_()

!----------------------------------------------------------------------------
!                                                         LinSolverPointer_
!----------------------------------------------------------------------------

TYPE :: LinSolverPointer_
  CLASS(LinSolver_), POINTER :: Ptr => NULL()
END TYPE LinSolverPointer_

PUBLIC :: LinSolverPointer_

!----------------------------------------------------------------------------
!                                      getLinSolverCodeFromName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get linear solver integer code from name

INTERFACE
  MODULE PURE FUNCTION ls_getLinSolverCodeFromName(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION ls_getLinSolverCodeFromName
END INTERFACE

!----------------------------------------------------------------------------
!                                      getLinSolverNameFromCode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Get the linear solver name from integer code

INTERFACE
  MODULE PURE FUNCTION ls_getLinSolverNameFromCode(name) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(15) :: ans
  END FUNCTION ls_getLinSolverNameFromCode
END INTERFACE

!----------------------------------------------------------------------------
!                                              setLinSolverParam@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: Set linear solver parameters

INTERFACE
  MODULE SUBROUTINE setLinSolverParam( &
    & param, &
    & solverName, &
    & preconditionOption, &
    & convergenceIn, &
    & convergenceType, &
    & maxIter, &
    & relativeToRHS, &
    & KrylovSubspaceSize, &
    & rtol, &
    & atol, &
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: solverName
    !! Solver name
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: preconditionOption
    !! precondition option
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceIn
    !! convergenceInRes
    !! convergenceInSol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceType
    !! relativeConvergence
    !! absoluteConvergence
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! maximum iteration
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativeToRHS
    !! relative to RHS
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: KrylovSubspaceSize
    !! KrylovSubspaceSize
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
    !! relative tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
    !! absolute tolerance
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: p_name
    !! Name of preconditioner
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
  END SUBROUTINE setLinSolverParam
END INTERFACE

PUBLIC :: setLinSolverParam

!----------------------------------------------------------------------------
!                                              getLinSolverParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: Returns the linear solver parameters

INTERFACE
  MODULE SUBROUTINE getLinSolverParam( &
    & param, &
    & solverName, &
    & preconditionOption, &
    & convergenceIn, &
    & convergenceType, &
    & maxIter, &
    & relativeToRHS, &
    & KrylovSubspaceSize, &
    & rtol, &
    & atol,  &
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
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: solverName
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: preconditionOption
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: convergenceIn
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: convergenceType
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: maxIter
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: relativeToRHS
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: KrylovSubspaceSize
    REAL(DFP), OPTIONAL, INTENT(OUT) :: rtol
    REAL(DFP), OPTIONAL, INTENT(OUT) :: atol
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
  END SUBROUTINE getLinSolverParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     checkEssentialParam@ConstructorMethods
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

INTERFACE LinSolverCheckEssentialParam
  MODULE PROCEDURE ls_checkEssentialParam
END INTERFACE LinSolverCheckEssentialParam

PUBLIC :: LinSolverCheckEssentialParam

!-----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
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

INTERFACE LinSolverInitiate
  MODULE PROCEDURE ls_Initiate
END INTERFACE LinSolverInitiate

PUBLIC :: LinSolverInitiate

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

INTERFACE
  MODULE SUBROUTINE ls_Display(obj, msg, unitno)
    CLASS(LinSolver_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Unitno
  END SUBROUTINE ls_Display
END INTERFACE

INTERFACE LinSolverDisplay
  MODULE PROCEDURE ls_Display
END INTERFACE LinSolverDisplay

PUBLIC :: LinSolverDisplay

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: This routine intiates the linear solver from import

INTERFACE
  MODULE SUBROUTINE ls_Import(obj, hdf5, group)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ls_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ls_Deallocate(obj)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE ls_Deallocate
END INTERFACE

INTERFACE LinSolverDeallocate
  MODULE PROCEDURE ls_Deallocate
END INTERFACE LinSolverDeallocate

PUBLIC :: LinSolverDeallocate

!----------------------------------------------------------------------------
!                                                 Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ls_final(obj)
    TYPE(LinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE ls_final
END INTERFACE

END MODULE LinSolver_Class
