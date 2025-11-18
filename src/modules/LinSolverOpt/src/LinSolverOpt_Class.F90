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
USE GlobalData, ONLY: DFP, I4B, LGT, &
                      LIS_CG, NO_PRECONDITION, PRECOND_NONE, &
                      convergenceInRes, relativeConvergence, &
                      LIS_SOR
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: TypePrecondOpt, TypeConvergenceOpt, TypeSolverNameOpt

IMPLICIT NONE

PRIVATE

PUBLIC :: LinSolverOpt_, TypeLinSolverOpt

CHARACTER(*), PARAMETER :: modName = "LinSolverOpt_Class"

!----------------------------------------------------------------------------
!                                                              LinSolverOpt_
!----------------------------------------------------------------------------

TYPE :: LinSolverOpt_
  CHARACTER(128) :: engine = "NATIVE_SERIAL"
  CHARACTER(128) :: solverName_char = "CG"
  INTEGER(I4B) :: solverName = TypeSolverNameOpt%cg
  CHARACTER(128) :: preconditionOption_char = "NO"
  INTEGER(I4B) :: preconditionOption = TypePrecondOpt%NONE
  CHARACTER(128) :: p_name_char = "NONE"
  INTEGER(I4B) :: p_name = TypePrecondOpt%NONE
  INTEGER(I4B) :: convergenceIn = TypeConvergenceOpt%res
  CHARACTER(128) :: convergenceIn_char = "RESIDUAL"
  INTEGER(I4B) :: convergenceType = TypeConvergenceOpt%relative
  CHARACTER(128) :: convergenceType_char = "RELATIVE"
  INTEGER(I4B) :: maxIter = 1000
  LOGICAL(LGT) :: relativeToRHS = .FALSE.
  INTEGER(I4B) :: krylovSubspaceSize = 50
  REAL(DFP) :: rtol = 1.0E-8
  REAL(DFP) :: atol = 1.0E-8
  REAL(DFP) :: sor_omega = 1.9_DFP
  INTEGER(I4B) :: bicgstab_ell = 2
  INTEGER(I4B) :: scale_none = 0
  INTEGER(I4B) :: scale_jacobi = 1
  INTEGER(I4B) :: scale_symm_diag = 2
  INTEGER(I4B) :: scale = 0
  CHARACTER(128) :: scale_char = "NONE"
  LOGICAL(LGT) :: initx_zeros = .TRUE.

  INTEGER(I4B) :: ilu_lfil = 5
  INTEGER(I4B) :: ilu_mbloc = 10
  REAL(DFP) :: ilu_droptol = 0.005_DFP
  REAL(DFP) :: ilu_permtol = 0.1_DFP
  REAL(DFP) :: ilu_alpha = 1.0_DFP
  INTEGER(I4B) :: ilu_fill = 5

  REAL(DFP) :: ssor_omega = 1.0_DFP

  INTEGER(I4B) :: hybrid_i = TypeSolverNameOpt%sor
  CHARACTER(128) :: hybrid_i_char = "SOR"
  INTEGER(I4B) :: hybrid_maxiter = 25
  REAL(DFP) :: hybrid_tol = 1.0E-3
  REAL(DFP) :: hybrid_omega = 1.5
  INTEGER(I4B) :: hybrid_ell = 2
  INTEGER(I4B) :: hybrid_restart = 40

  REAL(DFP) :: is_alpha = 1.0
  INTEGER(I4B) :: is_m = 3

  REAL(DFP) :: sainv_drop = 0.05

  LOGICAL(LGT) :: saamg_unsym = .FALSE.
  REAL(DFP) :: saamg_theta = 0.05

  REAL(DFP) :: iluc_drop = 0.05
  REAL(DFP) :: iluc_rate = 5.0

  LOGICAL(LGT) :: adds = .TRUE.
  INTEGER(I4B) :: adds_iter = 1

CONTAINS
  PRIVATE

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SolverNameToInteger => &
    obj_SolverNameToInteger
  !! Convert solver name to integer code

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
END TYPE LinSolverOpt_

!----------------------------------------------------------------------------
!                                                           TypeLinSolverOpt
!----------------------------------------------------------------------------

TYPE(LinSolverOpt_), PARAMETER :: TypeLinSolverOpt = LinSolverOpt_()

!----------------------------------------------------------------------------
!                                                 SolverNameToInteger@Methods
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
!                                                PrecondOptToInteger@Methods
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
!                                              ConvergenceInToInteger@Methods
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
!                                            ConvergenceTypeToInteger@Methods
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
!                                                      ScaleToInteger@Methods
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
!                                               PrecondNameToInteger@Methods
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

END MODULE LinSolverOpt_Class
