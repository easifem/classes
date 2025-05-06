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

MODULE AbstractLinSolverParam
USE GlobalData, ONLY: DFP, I4B, LGT, &
                      LIS_CG, NO_PRECONDITION, PRECOND_NONE, &
                      convergenceInRes, relativeConvergence, &
                      LIS_SOR

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER, PUBLIC :: default_engine = "NATIVE_SERIAL"
CHARACTER(*), PARAMETER, PUBLIC :: default_solverName_char = "CG"
INTEGER(I4B), PARAMETER, PUBLIC :: default_solverName = LIS_CG
CHARACTER(*), PARAMETER, PUBLIC :: default_preconditionOption_char = "NO"
INTEGER(I4B), PARAMETER, PUBLIC :: default_preconditionOption = NO_PRECONDITION
CHARACTER(*), PARAMETER, PUBLIC :: default_p_name_char = "NONE"
INTEGER(I4B), PARAMETER, PUBLIC :: default_p_name = PRECOND_NONE

INTEGER(I4B), PARAMETER, PUBLIC :: default_convergenceIn = convergenceInRes
CHARACTER(*), PARAMETER, PUBLIC :: default_convergenceIn_char = "RESIDUAL"

INTEGER(I4B), PARAMETER, PUBLIC :: default_convergenceType = relativeConvergence
CHARACTER(*), PARAMETER, PUBLIC :: default_convergenceType_char = "RELATIVE"

INTEGER(I4B), PARAMETER, PUBLIC :: default_maxIter = 1000
LOGICAL(LGT), PARAMETER, PUBLIC :: default_relativeToRHS = .FALSE.
INTEGER(I4B), PARAMETER, PUBLIC :: default_KrylovSubspaceSize = 50
REAL(DFP), PARAMETER, PUBLIC :: default_rtol = 1.0E-8
REAL(DFP), PARAMETER, PUBLIC :: default_atol = 1.0E-8
REAL(DFP), PARAMETER, PUBLIC :: default_sor_omega = 1.9_DFP
INTEGER(I4B), PARAMETER, PUBLIC :: default_bicgstab_ell = 2
INTEGER(I4B), PARAMETER, PUBLIC :: scale_none = 0
INTEGER(I4B), PARAMETER, PUBLIC :: scale_jacobi = 1
INTEGER(I4B), PARAMETER, PUBLIC :: scale_symm_diag = 2
INTEGER(I4B), PARAMETER, PUBLIC :: default_scale = scale_none
CHARACTER(*), PARAMETER, PUBLIC :: default_scale_char = "NONE"

LOGICAL(LGT), PARAMETER, PUBLIC :: default_initx_zeros = .TRUE.

INTEGER(I4B), PARAMETER, PUBLIC :: default_ilu_lfil = 5
INTEGER(I4B), PARAMETER, PUBLIC :: default_ilu_mbloc = 10
REAL(DFP), PARAMETER, PUBLIC :: default_ilu_droptol = 0.005_DFP
REAL(DFP), PARAMETER, PUBLIC :: default_ilu_permtol = 0.1_DFP
REAL(DFP), PARAMETER, PUBLIC :: default_ilu_alpha = 1.0_DFP

INTEGER(I4B), PARAMETER, PUBLIC :: default_ilu_fill = 5
REAL(DFP), PARAMETER, PUBLIC :: default_ssor_omega = 1.0

INTEGER(I4B), PARAMETER, PUBLIC :: default_hybrid_i = LIS_SOR
CHARACTER(*), PARAMETER, PUBLIC :: default_hybrid_i_char = "SOR"
INTEGER(I4B), PARAMETER, PUBLIC :: default_hybrid_maxiter = 25
REAL(DFP), PARAMETER, PUBLIC :: default_hybrid_tol = 1.0E-3
REAL(DFP), PARAMETER, PUBLIC :: default_hybrid_omega = 1.5
INTEGER(I4B), PARAMETER, PUBLIC :: default_hybrid_ell = 2
INTEGER(I4B), PARAMETER, PUBLIC :: default_hybrid_restart = 40

REAL(DFP), PARAMETER, PUBLIC :: default_is_alpha = 1.0
INTEGER(I4B), PARAMETER, PUBLIC :: default_is_m = 3

REAL(DFP), PARAMETER, PUBLIC :: default_sainv_drop = 0.05

LOGICAL(LGT), PARAMETER, PUBLIC :: default_saamg_unsym = .FALSE.

REAL(DFP), PARAMETER, PUBLIC :: default_saamg_theta = 0.05

REAL(DFP), PARAMETER, PUBLIC :: default_iluc_drop = 0.05
REAL(DFP), PARAMETER, PUBLIC :: default_iluc_rate = 5.0

LOGICAL(LGT), PARAMETER, PUBLIC :: default_adds = .TRUE.
INTEGER(I4B), PARAMETER, PUBLIC :: default_adds_iter = 1
END MODULE AbstractLinSolverParam
