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
!

SUBMODULE(AbstractStfemAlgorithm2_Class) Methods
USE ApproxUtility, ONLY: OPERATOR(.approxeq.)
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE InputUtility, ONLY: Input
USE Lapack_Method, ONLY: GetInvMat
USE MassMatrix_Method, ONLY: MassMatrix_
USE ProductUtility, ONLY: OuterProd_
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractStfemAlgorithm2_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                               AllocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Reallocate(obj%rhs_m_u1_zero, nnt)
CALL Reallocate(obj%rhs_k_u1_zero, nnt)
CALL Reallocate(obj%rhs_c_u1_zero, nnt)
CALL Reallocate(obj%rhs_m_v1_zero, nnt)
CALL Reallocate(obj%rhs_k_v1_zero, nnt)
CALL Reallocate(obj%rhs_c_v1_zero, nnt)

CALL Reallocate(obj%initialGuess_zero, nnt + math%three_i)
CALL Reallocate(obj%jumpDis_zero, nnt + math%three_i)
CALL Reallocate(obj%jumpVel_zero, nnt + math%three_i)
CALL Reallocate(obj%dis_zero, nnt + math%three_i)
CALL Reallocate(obj%vel_zero, nnt + math%three_i)
CALL Reallocate(obj%acc_zero, nnt + math%three_i)

CALL Reallocate(obj%rhs_m_u1, nnt)
CALL Reallocate(obj%rhs_k_u1, nnt)
CALL Reallocate(obj%rhs_c_u1, nnt)
CALL Reallocate(obj%rhs_m_v1, nnt)
CALL Reallocate(obj%rhs_k_v1, nnt)
CALL Reallocate(obj%rhs_c_v1, nnt)
! TODO: Remove the kt_inv_tn
CALL Reallocate(obj%kt_inv_tn, nnt)
CALL Reallocate(obj%stvel_u0, nnt)
CALL Reallocate(obj%stdisp_u0, nnt)

CALL Reallocate(obj%initialGuess, nnt + math%three_i)
CALL Reallocate(obj%jumpDis, nnt + math%three_i)
CALL Reallocate(obj%jumpVel, nnt + math%three_i)
CALL Reallocate(obj%dis, nnt + math%three_i)
CALL Reallocate(obj%vel, nnt + math%three_i)
CALL Reallocate(obj%acc, nnt + math%three_i)

CALL Reallocate(obj%massMatCoeff, nnt, nnt)
CALL Reallocate(obj%dampMatCoeff, nnt, nnt)
CALL Reallocate(obj%stiffMatCoeff, nnt, nnt)
CALL Reallocate(obj%forceCoeff, nnt, nnt)
CALL Reallocate(obj%stvel_sol, nnt, nnt)
CALL Reallocate(obj%stdisp_sol, nnt, nnt)
CALL Reallocate(obj%kt, nnt, nnt)
CALL Reallocate(obj%mt, nnt, nnt)
CALL Reallocate(obj%mt_inv, nnt, nnt)
CALL Reallocate(obj%kt_inv, nnt, nnt)
CALL Reallocate(obj%kt_inv_mt, nnt, nnt)
CALL Reallocate(obj%mt_inv_kt, nnt, nnt)
CALL Reallocate(obj%kt_mt_inv, nnt, nnt)
CALL Reallocate(obj%mt_kt_inv, nnt, nnt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateData

!----------------------------------------------------------------------------
!                                                                   MakeZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeZeros
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MakeZeros()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%rhs_m_u1_zero = obj%rhs_m_u1.approxeq.math%zero
obj%rhs_m_v1_zero = obj%rhs_m_v1.approxeq.math%zero
obj%rhs_c_u1_zero = obj%rhs_c_u1.approxeq.math%zero
obj%rhs_c_v1_zero = obj%rhs_c_v1.approxeq.math%zero
obj%rhs_k_u1_zero = obj%rhs_k_u1.approxeq.math%zero
obj%rhs_k_v1_zero = obj%rhs_k_v1.approxeq.math%zero

obj%initialGuess_zero = obj%initialGuess.approxeq.math%zero
obj%jumpDis_zero = obj%jumpDis.approxeq.math%zero
obj%jumpVel_zero = obj%jumpVel.approxeq.math%zero
obj%dis_zero = obj%dis.approxeq.math%zero
obj%vel_zero = obj%vel.approxeq.math%zero
obj%acc_zero = obj%acc.approxeq.math%zero

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_MakeZeros

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%scalingOpt = "M"
obj%isInit = math%no
obj%nrow = math%zero_i
obj%ncol = math%zero_i

IF (ALLOCATED(obj%rhs_m_u1_zero)) DEALLOCATE (obj%rhs_m_u1_zero)
IF (ALLOCATED(obj%rhs_k_u1_zero)) DEALLOCATE (obj%rhs_k_u1_zero)
IF (ALLOCATED(obj%rhs_c_u1_zero)) DEALLOCATE (obj%rhs_c_u1_zero)
IF (ALLOCATED(obj%rhs_m_v1_zero)) DEALLOCATE (obj%rhs_m_v1_zero)
IF (ALLOCATED(obj%rhs_k_v1_zero)) DEALLOCATE (obj%rhs_k_v1_zero)
IF (ALLOCATED(obj%rhs_c_v1_zero)) DEALLOCATE (obj%rhs_c_v1_zero)
IF (ALLOCATED(obj%initialGuess_zero)) DEALLOCATE (obj%initialGuess_zero)
IF (ALLOCATED(obj%jumpDis_zero)) DEALLOCATE (obj%jumpDis_zero)
IF (ALLOCATED(obj%jumpVel_zero)) DEALLOCATE (obj%jumpVel_zero)
IF (ALLOCATED(obj%dis_zero)) DEALLOCATE (obj%dis_zero)
IF (ALLOCATED(obj%vel_zero)) DEALLOCATE (obj%vel_zero)
IF (ALLOCATED(obj%acc_zero)) DEALLOCATE (obj%acc_zero)

IF (ALLOCATED(obj%rhs_m_u1)) DEALLOCATE (obj%rhs_m_u1)
IF (ALLOCATED(obj%rhs_k_u1)) DEALLOCATE (obj%rhs_k_u1)
IF (ALLOCATED(obj%rhs_c_u1)) DEALLOCATE (obj%rhs_c_u1)
IF (ALLOCATED(obj%rhs_m_v1)) DEALLOCATE (obj%rhs_m_v1)
IF (ALLOCATED(obj%rhs_k_v1)) DEALLOCATE (obj%rhs_k_v1)
IF (ALLOCATED(obj%rhs_c_v1)) DEALLOCATE (obj%rhs_c_v1)
IF (ALLOCATED(obj%initialGuess)) DEALLOCATE (obj%initialGuess)
IF (ALLOCATED(obj%jumpDis)) DEALLOCATE (obj%jumpDis)
IF (ALLOCATED(obj%jumpVel)) DEALLOCATE (obj%jumpVel)
IF (ALLOCATED(obj%dis)) DEALLOCATE (obj%dis)
IF (ALLOCATED(obj%vel)) DEALLOCATE (obj%vel)
IF (ALLOCATED(obj%acc)) DEALLOCATE (obj%acc)
IF (ALLOCATED(obj%kt_inv_tn)) DEALLOCATE (obj%kt_inv_tn)

IF (ALLOCATED(obj%massMatCoeff)) DEALLOCATE (obj%massMatCoeff)
IF (ALLOCATED(obj%dampMatCoeff)) DEALLOCATE (obj%dampMatCoeff)
IF (ALLOCATED(obj%stiffMatCoeff)) DEALLOCATE (obj%stiffMatCoeff)
IF (ALLOCATED(obj%forceCoeff)) DEALLOCATE (obj%forceCoeff)
IF (ALLOCATED(obj%kt)) DEALLOCATE (obj%kt)
IF (ALLOCATED(obj%mt)) DEALLOCATE (obj%mt)
IF (ALLOCATED(obj%mt_inv)) DEALLOCATE (obj%mt_inv)
IF (ALLOCATED(obj%kt_inv)) DEALLOCATE (obj%kt_inv)
IF (ALLOCATED(obj%kt_inv_mt)) DEALLOCATE (obj%kt_inv_mt)
IF (ALLOCATED(obj%mt_inv_kt)) DEALLOCATE (obj%mt_inv_kt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: nns

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInit: ", unitno=unitno)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display(obj%nrow, "nrow: ", unitno=unitno)
CALL Display(obj%ncol, "ncol: ", unitno=unitno)

nns = obj%nrow

CALL Display(obj%initialGuess(1:nns + 3), "initialGuess: ", &
             unitno=unitno, advance="NO")
CALL Display(obj%initialGuess_zero(1:nns + 3), "initialGuess_zero: ", &
             unitno=unitno)

CALL Display(obj%jumpDis(1:nns + 3), "jumpDis: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%jumpDis_zero(1:nns + 3), "jumpDis_zero: ", unitno=unitno)

CALL Display(obj%jumpVel(1:nns + 3), "jumpVel: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%jumpVel_zero(1:nns + 3), "jumpVel_zero: ", unitno=unitno)

CALL Display(obj%dis(1:nns + 3), "dis: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%dis_zero(1:nns + 3), "dis_zero: ", unitno=unitno)

CALL Display(obj%vel(1:nns + 3), "vel: ", unitno=unitno, advance="NO")
CALL Display(obj%vel_zero(1:nns + 3), "vel_zero: ", unitno=unitno)

CALL Display(obj%acc(1:nns + 3), "acc: ", unitno=unitno, advance="NO")
CALL Display(obj%acc_zero(1:nns + 3), "acc_zero: ", unitno=unitno)

CALL Display(obj%massMatCoeff(1:nns, 1:nns), "massMatCoeff: ", &
             unitno=unitno)
CALL Display(obj%dampMatCoeff(1:nns, 1:nns), "dampMatCoeff: ", &
             unitno=unitno)
CALL Display(obj%stiffMatCoeff(1:nns, 1:nns), "stiffMatCoeff: ", &
             unitno=unitno)

CALL Display(obj%rhs_m_u1(1:nns), "rhs_m_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_u1_zero(1:nns), "rhs_m_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_m_v1(1:nns), "rhs_m_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_v1_zero(1:nns), "rhs_m_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_u1(1:nns), "rhs_c_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_u1_zero(1:nns), "rhs_c_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_v1(1:nns), "rhs_c_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_v1_zero(1:nns), "rhs_c_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_u1(1:nns), "rhs_k_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_u1_zero(1:nns), "rhs_k_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_v1(1:nns), "rhs_k_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_v1_zero(1:nns), "rhs_k_v1_zero: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
