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

MODULE LinearElasticModelUtility
USE GlobalData, ONLY: DFP, LGT
USE ExceptionHandler_Class, ONLY: err => e
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "LinearElasticModelUtility"

PUBLIC :: GetYoungsModulus
PUBLIC :: GetShearModulus
PUBLIC :: GetElasticParam
PUBLIC :: Get_PlaneStrain_C_InvC
PUBLIC :: Get_PlaneStress_C_InvC
PUBLIC :: Get_3D_C_InvC

!----------------------------------------------------------------------------
!                                                  Get_3D_C_InvC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-01
! summary:  This routine returns C and invC from E and nu

INTERFACE
  MODULE SUBROUTINE Get_3D_C_InvC(C, invC, youngsModulus, nu)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
    REAL(DFP), INTENT(IN) :: youngsModulus
    REAL(DFP), INTENT(IN) :: nu
  END SUBROUTINE Get_3D_C_InvC
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Get_PlaneStrain_C_InvC
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-01
! summary:  This routine returns C and invC from E and nu

INTERFACE
  MODULE SUBROUTINE Get_PlaneStrain_C_InvC(C, invC, youngsModulus, nu)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
    REAL(DFP), INTENT(IN) :: youngsModulus
    REAL(DFP), INTENT(IN) :: nu
  END SUBROUTINE Get_PlaneStrain_C_InvC
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Get_PlaneStress_C_InvC
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Get_PlaneStress_C_InvC(C, invC, youngsModulus, nu)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
    REAL(DFP), INTENT(IN) :: youngsModulus
    REAL(DFP), INTENT(IN) :: nu
  END SUBROUTINE Get_PlaneStress_C_InvC
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetYoungsModulus
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION GetYoungsModulus(E, G, lambda, mu, nu, K) RESULT(ans)
    REAL(DFP), OPTIONAL, INTENT(IN) :: E
    REAL(DFP), OPTIONAL, INTENT(IN) :: G
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: mu
    REAL(DFP), OPTIONAL, INTENT(IN) :: nu
    REAL(DFP), OPTIONAL, INTENT(IN) :: K
    REAL(DFP) :: ans
  END FUNCTION GetYoungsModulus
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetShearModulus
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION GetShearModulus(E, G, lambda, mu, nu, K) RESULT(ans)
    REAL(DFP), OPTIONAL, INTENT(IN) :: E
    REAL(DFP), OPTIONAL, INTENT(IN) :: G
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: mu
    REAL(DFP), OPTIONAL, INTENT(IN) :: nu
    REAL(DFP), OPTIONAL, INTENT(IN) :: K
    REAL(DFP) :: ans
  END FUNCTION GetShearModulus
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE GetElasticParam(lam, G, EE, nu, shearModulus, &
                                    youngsModulus, poissonRatio, lambda)
    REAL(DFP), INTENT(OUT) :: lam
    REAL(DFP), INTENT(OUT) :: G
    REAL(DFP), INTENT(OUT) :: EE
    REAL(DFP), INTENT(OUT) :: nu
    REAL(DFP), OPTIONAL, INTENT(IN) :: shearModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: poissonRatio
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
  END SUBROUTINE GetElasticParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Include error
!----------------------------------------------------------------------------

END MODULE LinearElasticModelUtility
