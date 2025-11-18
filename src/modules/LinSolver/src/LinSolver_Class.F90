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
  FINAL :: obj_Final
  !! Final

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! Set the matrix and preconditioning matrix
  PROCEDURE, PUBLIC, PASS(obj) :: Solve => obj_solve
  !! Solve the system of linear equation
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
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-15
! summary: Initiate the linear solver

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
    CLASS(LinSolver_), INTENT(INOUT) :: obj
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

INTERFACE LinSolverInitiate
  MODULE PROCEDURE obj_Initiate
END INTERFACE LinSolverInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(LinSolver_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE LinSolverDeallocate
  MODULE PROCEDURE obj_Deallocate
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
