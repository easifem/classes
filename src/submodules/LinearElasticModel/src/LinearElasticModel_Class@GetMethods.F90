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

SUBMODULE(LinearElasticModel_Class) GetMethods
USE BaseMethod, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetElasticParam
!----------------------------------------------------------------------------

MODULE PROCEDURE GetElasticParam
LOGICAL(LGT) :: isLam, isG, isNu, isE
REAL(DFP) :: r

IF (PRESENT(shearModulus)) THEN
  isG = .TRUE.
  G = shearModulus
ELSE
  isG = .FALSE.
END IF

IF (PRESENT(youngsModulus)) THEN
  isE = .TRUE.
  EE = youngsModulus
ELSE
  isE = .FALSE.
END IF

IF (PRESENT(poissonRatio)) THEN
  isNu = .TRUE.
  Nu = poissonRatio
ELSE
  isNu = .FALSE.
END IF

IF (PRESENT(lambda)) THEN
  isLam = .TRUE.
  lam = lambda
ELSE
  isLam = .FALSE.
END IF

!1
IF (isNu .AND. isE) THEN
  lam = EE * nu / (1.0 + nu) / (1.0 - 2.0 * nu)
  G = EE * 0.5_DFP / (1.0 + nu)
  !2
ELSE IF (isG .AND. isE) THEN
  lam = G * (EE - 2.0 * G) / (3.0 * G - EE)
  nu = (EE - 2.0 * G) * 0.5_DFP / G
  !3
ELSE IF (isNu .AND. isG) THEN
  lam = 2.0 * G * nu / (1.0 - 2.0 * nu)
  EE = 2.0 * G * (1.0 + nu)
  !4
ELSE IF (isLam .AND. isG) THEN
  EE = G * (3.0 * lam + 2.0 * G) / (lam + G)
  nu = lam * 0.5 / (lam + G)
  !5
ELSE IF (isE .AND. isLam) THEN
  r = SQRT(EE * EE + 9.0 * lam * lam + 2.0 * EE * lam)
  G = (EE - 3.0 * lam + r) / 4.0
  nu = 2.0 * lam / (EE + lam + r)
  !6
ELSE IF (isNu .AND. isLam) THEN
  EE = lam * (1.0 + nu) * (1.0 - 2.0 * nu) / nu
  G = lam * (1.0 - 2.0 * nu) * 0.5_DFP / nu
END IF
END PROCEDURE GetElasticParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Get_PlaneStress_C_invC
REAL(DFP) :: a
C = 0.0_DFP
invC = 0.0_DFP
a = youngsModulus / (1.0 - nu * nu)
C(1, 1) = a
C(2, 2) = C(1, 1)
C(3, 3) = a * (1.0 - nu) * 0.5_DFP
C(1, 2) = a * nu
C(2, 1) = C(1, 2)
a = 1.0_DFP / youngsModulus
invC(1, 1) = a
invC(2, 2) = invC(1, 1)
invC(3, 3) = a * 2.0_DFP * (1.0 + nu)
invC(1, 2) = -nu * a
invC(2, 1) = invC(1, 2)
END PROCEDURE Get_PlaneStress_C_invC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Get_PlaneStrain_C_invC
REAL(DFP) :: a
C = 0.0_DFP
invC = 0.0_DFP
a = youngsModulus / (1.0 - 2.0 * nu) / (1.0 + nu)
C(1, 1) = a * (1.0 - nu)
C(2, 2) = C(1, 1)
C(3, 3) = a * (1.0 - 2.0 * nu) * 0.5_DFP
C(1, 2) = a * nu
C(2, 1) = C(1, 2)
a = (1.0 + nu) / youngsModulus
invC(1, 1) = a * (1.0 - nu)
invC(2, 2) = invC(1, 1)
invC(3, 3) = 2.0_DFP * a
invC(1, 2) = -nu * a
invC(2, 1) = invC(1, 2)
END PROCEDURE Get_PlaneStrain_C_invC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Get_3D_C_invC
REAL(DFP) :: a
C = 0.0_DFP
invC = 0.0_DFP
a = youngsModulus / (1.0 + nu) / (1.0 - 2.0 * nu)
C(1, 1) = (1.0 - nu) * a
C(2, 2) = C(1, 1)
C(3, 3) = C(1, 1)
C(4, 4) = a * 0.5 * (1.0 - 2.0 * nu)
C(5, 5) = C(4, 4)
C(6, 6) = C(4, 4)
C(1, 2) = nu * a
C(1, 3) = C(1, 2)
C(2, 1) = C(1, 2)
C(2, 3) = C(1, 2)
C(3, 1) = C(1, 2)
C(3, 2) = C(1, 2)
a = 1.0_DFP / youngsModulus
invC(1, 1) = a
invC(2, 2) = invC(1, 1)
invC(3, 3) = invC(1, 1)
invC(4, 4) = a * 2.0 * (1.0 + nu)
invC(5, 5) = invC(4, 4)
invC(6, 6) = invC(4, 4)
invC(1, 2) = -nu * a
invC(1, 3) = invC(1, 2)
invC(2, 1) = invC(1, 2)
invC(2, 3) = invC(1, 2)
invC(3, 1) = invC(1, 2)
invC(3, 2) = invC(1, 2)
END PROCEDURE Get_3D_C_invC

!----------------------------------------------------------------------------
!                                                            GetElasticParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElasticParam
IF (PRESENT(poissonRatio)) poissonRatio = obj%nu
IF (PRESENT(shearModulus)) shearModulus = obj%G
IF (PRESENT(youngsModulus)) youngsModulus = obj%E
IF (PRESENT(lambda)) lambda = obj%lambda
IF (PRESENT(stiffnessPower)) stiffnessPower = obj%stiffnessPower
IF (PRESENT(C)) C = obj%C
IF (PRESENT(invC)) invC = invC
END PROCEDURE obj_GetElasticParam

!----------------------------------------------------------------------------
!                                                                       GetC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetC
C = obj%C
END PROCEDURE obj_GetC

!----------------------------------------------------------------------------
!                                                                    GetinvC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetinvC
invC = obj%invC
END PROCEDURE obj_GetinvC

!----------------------------------------------------------------------------
!                                                         GetElasticityType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElasticityType
ans = obj%elasticityType
END PROCEDURE obj_GetElasticityType

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myPrefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                               GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
IF (PRESENT(elasticityType)) elasticityType = obj%elasticityType
IF (PRESENT(nu)) nu = obj%nu
IF (PRESENT(G)) G = obj%G
IF (PRESENT(youngsModulus)) youngsModulus = obj%E
IF (PRESENT(lambda)) lambda = obj%lambda
IF (PRESENT(C)) C = obj%C
IF (PRESENT(invC)) invC = obj%invC
IF (PRESENT(stiffnessPower)) stiffnessPower = obj%stiffnessPower
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                               GetDataSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDataSize
CHARACTER(*), PARAMETER :: myName = "obj_GetDataSize()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

ans = 0
SELECT CASE (obj%elasticityType)
CASE (IsoLinearElasticModel)
  ans = 2
CASE (AnisoLinearElasticModel)
  ans = 21
CASE (TransLinearElasticModel)
  ans = 5
CASE (OrthoLinearElasticModel)
  ans = 9
CASE default
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for elasticityType = '//  &
    & tostring(obj%elasticityType))
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_GetDataSize

!----------------------------------------------------------------------------
!                                                                    GetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetData
CHARACTER(*), PARAMETER :: myName = "obj_GetData()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

SELECT CASE (obj%elasticityType)
CASE (IsoLinearElasticModel)
  CALL LinearElasticModelGetData_Iso(obj, DATA)
CASE (AnisoLinearElasticModel)
  CALL LinearElasticModelGetData_Aniso(obj, DATA)
CASE (TransLinearElasticModel)
  CALL LinearElasticModelGetData_Trans(obj, DATA)
CASE (OrthoLinearElasticModel)
  CALL LinearElasticModelGetData_Ortho(obj, DATA)
CASE default
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for elasticityType = '//  &
    & tostring(obj%elasticityType))
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_GetData

!----------------------------------------------------------------------------
!                                             LinearElasticModelGetData_Iso
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelGetData_Iso
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Iso()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DATA(1) = obj%lambda
DATA(2) = obj%G

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE LinearElasticModelGetData_Iso

!----------------------------------------------------------------------------
!                                            LinearElasticModelGetData_Aniso
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelGetData_Aniso
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Aniso()"
INTEGER(I4B) :: ii, jj, kk
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

kk = 0
DO jj = 1, 6
  DO ii = jj, 6
    kk = kk + 1
    DATA(kk) = obj%C(ii, jj)
  END DO
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE LinearElasticModelGetData_Aniso

!----------------------------------------------------------------------------
!                                           LinearElasticModelGetData_Ortho
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelGetData_Ortho
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Ortho()"
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO ii = 1, 6
  DATA(ii) = obj%C(ii, ii)
END DO
DATA(7) = obj%C(1, 2)
DATA(8) = obj%C(2, 3)
DATA(9) = obj%C(1, 3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE LinearElasticModelGetData_Ortho

!----------------------------------------------------------------------------
!                                           LinearElasticModelGetData_Trans
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelGetData_Trans
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Trans()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DATA(1) = obj%C(1, 1)
DATA(2) = obj%C(3, 3)
DATA(3) = obj%C(5, 5)
DATA(4) = obj%C(1, 2)
DATA(5) = obj%C(1, 3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE LinearElasticModelGetData_Trans

END SUBMODULE GetMethods
