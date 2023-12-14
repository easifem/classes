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

SUBMODULE(LinSolver_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_GetPrefix
ans = myprefix
END PROCEDURE ls_GetPrefix

!----------------------------------------------------------------------------
!                                                 GetLinSolverCodeFromName
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_GetLinSolverCodeFromName
TYPE(String) :: astr
astr = UpperCase(name)

SELECT CASE (astr%chars())
CASE ("CG") !1
  ans = LIS_CG
CASE ("BICG", "BCG") !2
  ans = LIS_BICG
CASE ("CGS") !3
  ans = LIS_CGS
CASE ("BICGSTAB", "BCGSTAB") !4
  ans = LIS_BICGSTAB
CASE ("BICGSTABL", "BCGSTABL") !5
  ans = LIS_BICGSTABL
CASE ("GPBICG") !6
  ans = LIS_GPBICG
CASE ("TFQMR") !7
  ans = LIS_TFQMR
CASE ("OMN", "FOM", "ORTHOMIN") !8
  ans = LIS_OMN
CASE ("GMRES", "GMR") !9
  ans = LIS_GMRES
CASE ("JACOBI") !10
  ans = LIS_JACOBI
CASE ("GS") !11
  ans = LIS_GS
CASE ("SOR") !12
  ans = LIS_SOR
CASE ("BICGSAFE") !13
  ans = LIS_BICGSAFE
CASE ("CR") !14
  ans = LIS_CR
CASE ("BICR") !15
  ans = LIS_BICR
CASE ("CRS") !16
  ans = LIS_CRS
CASE ("BICRSTAB") !17
  ans = LIS_BICRSTAB
CASE ("GPBICR") !18
  ans = LIS_GPBICR
CASE ("BICRSAFE") !19
  ans = LIS_BICRSAFE
CASE ("FGMRES") !20
  ans = LIS_FGMRES
CASE ("IDRS") !21
  ans = LIS_IDRS
CASE ("IDR1") !22
  ans = LIS_IDR1
CASE ("MINRES") !23
  ans = LIS_MINRES
CASE ("COCG") !24
  ans = LIS_COCG
CASE ("COCR") !25
  ans = LIS_COCR
CASE ("CGNR", "CGN") !26
  ans = LIS_CGNR
CASE ("DBICG", "DBCG") !27
  ans = LIS_DBICG
CASE ("DQGMRES") !28
  ans = LIS_DQGMRES
CASE ("SUPERLU") !29
  ans = LIS_SUPERLU
END SELECT
astr = ""
END PROCEDURE ls_GetLinSolverCodeFromName

!----------------------------------------------------------------------------
!                                                 GetLinSolverNameFromCode
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_GetLinSolverNameFromCode
SELECT CASE (name)
CASE (LIS_SUPERLU)
  ans = "SUPERLU" !1
CASE (LIS_CG)
  ans = "CG" !1
CASE (LIS_BICG)
  ans = "BICG" !2
CASE (LIS_CGS)
  ans = "CGS" !3
CASE (LIS_BICGSTAB)
  ans = "BICGSTAB" !4
CASE (LIS_BICGSTABL)
  ans = "BICGSTABL" !5
CASE (LIS_GPBICG)
  ans = "GPBICG" !6
CASE (LIS_TFQMR)
  ans = "TFQMR" !7
CASE (LIS_OMN)
  ans = "ORTHOMIN" !8
CASE (LIS_GMRES)
  ans = "GMRES" !9
CASE (LIS_JACOBI)
  ans = "JACOBI" !10
CASE (LIS_GS)
  ans = "GS" !11
CASE (LIS_SOR)
  ans = "SOR" !12
CASE (LIS_BICGSAFE)
  ans = "BICGSAFE" !13
CASE (LIS_CR)
  ans = "CR" !14
CASE (LIS_BICR)
  ans = "BICR" !15
CASE (LIS_CRS)
  ans = "CRS" !16
CASE (LIS_BICRSTAB)
  ans = "BICRSTAB" !17
CASE (LIS_GPBICR)
  ans = "GPBICR" !18
CASE (LIS_BICRSAFE)
  ans = "BICRSAFE" !19
CASE (LIS_FGMRES)
  ans = "FGMRES" !20
CASE (LIS_IDRS)
  ans = "IDRS" !21
CASE (LIS_IDR1)
  ans = "IDR1" !22
CASE (LIS_MINRES)
  ans = "MINRES" !23
CASE (LIS_COCG)
  ans = "COCG" !24
CASE (LIS_COCR)
  ans = "COCR" !25
CASE (LIS_CGNR)
  ans = "CGNR" !26
CASE (LIS_DBICG)
  ans = "DBICG" !27
CASE (LIS_DQGMRES)
  ans = "DQGMRES" !28
END SELECT
END PROCEDURE ls_GetLinSolverNameFromCode

!----------------------------------------------------------------------------
!                                                   GetLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE GetLinSolverParam
CALL GetAbstractLinSolverParam( &
  & param=param, &
  & prefix=myPrefix, &
  & solverName=solverName, &
  & preconditionOption=preconditionOption, &
  & maxIter=maxIter, &
  & atol=atol, &
  & rtol=rtol, &
  & convergenceIn=convergenceIn, &
  & convergenceType=convergenceType, &
  & relativeToRHS=relativeToRHS, &
  & KrylovSubspaceSize=KrylovSubspaceSize, &
  & scale=scale, &
  & initx_zeros=initx_zeros, &
  & bicgstab_ell=bicgstab_ell, &
  & sor_omega=sor_omega, &
  & p_name=p_name, &
  & p_ilu_lfil=p_ilu_lfil, &
  & p_ilu_mbloc=p_ilu_mbloc, &
  & p_ilu_droptol=p_ilu_droptol, &
  & p_ilu_permtol=p_ilu_permtol, &
  & p_ilu_alpha=p_ilu_alpha, &
  & p_ilu_fill=p_ilu_fill, &
  & p_ssor_omega=p_ssor_omega, &
  & p_hybrid_i=p_hybrid_i, &
  & p_hybrid_maxiter=p_hybrid_maxiter, &
  & p_hybrid_tol=p_hybrid_tol, &
  & p_hybrid_omega=p_hybrid_omega, &
  & p_hybrid_ell=p_hybrid_ell, &
  & p_hybrid_restart=p_hybrid_restart, &
  & p_is_alpha=p_is_alpha, &
  & p_is_m=p_is_m, &
  & p_sainv_drop=p_sainv_drop, &
  & p_saamg_unsym=p_saamg_unsym, &
  & p_saamg_theta=p_saamg_theta, &
  & p_iluc_drop=p_iluc_drop, &
  & p_iluc_rate=p_iluc_rate, &
  & p_adds=p_adds, &
  & p_adds_iter=p_adds_iter &
  & )
END PROCEDURE GetLinSolverParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
