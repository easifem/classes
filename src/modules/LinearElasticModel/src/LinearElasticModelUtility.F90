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
USE GlobalData, ONLY: DFP, LGT, I4B
USE ExceptionHandler_Class, ONLY: err => e
USE BaseType, ONLY: FEVariable_
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "LinearElasticModelUtility"

PUBLIC :: GetYoungsModulus
PUBLIC :: GetYoungsModulusFEVar
PUBLIC :: GetShearModulus
PUBLIC :: GetShearModulusFEVar
PUBLIC :: GetPoissonRatio
PUBLIC :: GetElasticParam
PUBLIC :: Get_PlaneStrain_C_InvC
PUBLIC :: Get_PlaneStress_C_InvC
PUBLIC :: Get_3D_C_InvC
PUBLIC :: GetPlaneStrainC
PUBLIC :: GetPlaneStressC
PUBLIC :: GetC
PUBLIC :: Get3DC

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
!                                                                        GetC
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-07
! summary:  master of Get C

INTERFACE
  MODULE SUBROUTINE GetC(C, E, G, lambda, mu, nu, K, &
                         nsd, isPlaneStrain)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: E, G, lambda, mu, nu, K
    INTEGER(I4B), INTENT(IN) :: nsd
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStrain
  END SUBROUTINE GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                                        GetC
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-07
! summary:  master of Get C

INTERFACE
  MODULE SUBROUTINE Get3DC(C, E, G, lambda, mu, nu, K)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: E, G, lambda, mu, nu, K
  END SUBROUTINE Get3DC
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPlaneStrainC
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-07
! summary:  Get C for plane strain

INTERFACE
  MODULE SUBROUTINE GetPlaneStrainC(C, E, G, lambda, mu, nu, K)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: E, G, lambda, mu, nu, K
  END SUBROUTINE GetPlaneStrainC
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetPlaneStressC
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE GetPlaneStressC(C, E, G, lambda, mu, nu, K)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: E, G, lambda, mu, nu, K
  END SUBROUTINE GetPlaneStressC
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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION GetYoungsModulusFEVar(E, G, lambda, mu, nu, K) RESULT(ans)
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: E
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: G
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: lambda
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: mu
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: nu
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: K
    TYPE(FEVariable_) :: ans
  END FUNCTION GetYoungsModulusFEVar
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
  MODULE FUNCTION GetShearModulusFEVar(E, G, lambda, mu, nu, K) RESULT(ans)
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: E
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: G
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: lambda
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: mu
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: nu
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: K
    TYPE(FEVariable_) :: ans
  END FUNCTION GetShearModulusFEVar
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-08
! summary:  Get Poisson ratio from other parameters

INTERFACE
  MODULE FUNCTION GetPoissonRatio(E, G, lambda, mu, nu, K) RESULT(ans)
    REAL(DFP), OPTIONAL, INTENT(IN) :: E
    REAL(DFP), OPTIONAL, INTENT(IN) :: G
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: mu
    REAL(DFP), OPTIONAL, INTENT(IN) :: nu
    REAL(DFP), OPTIONAL, INTENT(IN) :: K
    REAL(DFP) :: ans
  END FUNCTION GetPoissonRatio
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
