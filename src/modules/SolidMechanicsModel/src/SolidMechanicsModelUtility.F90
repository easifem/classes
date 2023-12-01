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

MODULE SolidMechanicsModelUtility
USE GlobalData, ONLY: I4B, LGT, DFP

USE LinearElasticModel_Class, ONLY: LinearElasticModel_prefix,  &
  & SetLinearElasticModelParam

USE LinearPoroElasticModel_Class, ONLY: LinearPoroElasticModel_Prefix,  &
  & SetLinearPoroElasticModelParam

USE FPL, ONLY: ParameterList_

USE ExceptionHandler_Class, ONLY: e

PRIVATE

PUBLIC :: SetSolidMechanicsModelParam

CHARACTER(*), PARAMETER :: modName = "SolidMechanicsModelUtility"

CONTAINS

!----------------------------------------------------------------------------
!                                              SetSolidMechanicsModelParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-02
! summary: Get data for solid mechanics model

SUBROUTINE SetSolidMechanicsModelParam(param, prefix,  &
  & elasticityType, isPlaneStrain, isPlaneStress, poissonRatio,  &
  & youngsModulus, shearModulus, lambda, C, invC, stiffnessPower,  &
  & stress, strain)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: elasticityType
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStrain
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStress
  REAL(DFP), OPTIONAL, INTENT(IN) :: poissonRatio
  REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
  REAL(DFP), OPTIONAL, INTENT(IN) :: shearModulus
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
  REAL(DFP), OPTIONAL, INTENT(IN) :: C(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: invC(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: stiffnessPower
  REAL(DFP), OPTIONAL, INTENT(IN) :: stress(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: strain(:)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "SetSolidMechanicsModelData()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  SELECT CASE (prefix)
  CASE (LinearElasticModel_prefix)

    CALL SetLinearElasticModelParam(param=param,  &
      & elasticityType=elasticityType, isPlaneStress=isPlaneStress,  &
      & poissonRatio=poissonRatio, youngsModulus=youngsModulus,  &
      & shearModulus=shearModulus, lambda=lambda, C=C, invC=invC,  &
      & stiffnessPower=stiffnessPower, isPlaneStrain=isPlaneStrain)

  CASE (LinearPoroElasticModel_prefix)

    CALL SetLinearPoroElasticModelParam(param=param,  &
      & elasticityType=elasticityType, isPlaneStress=isPlaneStress,  &
      & poissonRatio=poissonRatio, youngsModulus=youngsModulus,  &
      & shearModulus=shearModulus, lambda=lambda, C=C, invC=invC,  &
      & isPlaneStrain=isPlaneStrain)

  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: No case found for '//prefix)
    RETURN
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetSolidMechanicsModelParam

END MODULE SolidMechanicsModelUtility
