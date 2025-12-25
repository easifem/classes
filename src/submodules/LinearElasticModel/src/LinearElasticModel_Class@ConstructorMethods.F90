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

SUBMODULE(LinearElasticModel_Class) ConstructorMethods
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString
USE StringUtility, ONLY: UpperCase
USE FPL_Method, ONLY: Set, CheckEssentialParam
USE AbstractSolidMechanicsModel_Class, ONLY: &
  AbstractSolidMechanicsModelDeallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 LinearElasticModelInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

LOGICAL(LGT) :: isok, isPlaneStress0, isPlaneStrain0, isIsotropic, abool
INTEGER(I4B) :: shapeOfC(2)
CHARACTER(:), ALLOCATABLE :: myprefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
CALL obj%SetIsInitiated(.TRUE.)
myprefix = "LinearElasticModel::"
CALL obj%SetName(myprefix)

obj%nc = 6
! isPlaneStress
isok = PRESENT(isPlaneStress)
IF (isok) THEN
  CALL obj%SetPlaneStress(isPlaneStress)
  IF (isPlaneStress) obj%nc = TypeElasticityOpt%size_c_plane_stress
END IF

! isPlaneStrain
isok = PRESENT(isPlaneStrain)
IF (isok) THEN
  CALL obj%SetPlaneStrain(isPlaneStrain)
  IF (isPlaneStrain) obj%nc = TypeElasticityOpt%size_c_plane_strain
END IF

isPlaneStress0 = obj%IsPlaneStress()
isPlaneStrain0 = obj%IsPlaneStrain()

! stiffnessPower
isok = PRESENT(stiffnessPower)
IF (isok) obj%stiffnessPower = stiffnessPower

! elasticityType
obj%elasticityType = elasticityType

! IsoLinearElasticModel
isIsotropic = obj%elasticityType .EQ. TypeElasticityOpt%Isotropic
IF (isIsotropic) THEN
  ! Check that atleast two elastic parameters should be given
  obj%lambda = 1.0
  obj%G = 1.0
  obj%E = 1.0
  obj%nu = 0.3
  CALL GetElasticParam(lam=obj%lambda, G=obj%G, EE=obj%E, nu=obj%nu, &
                       shearModulus=shearModulus, &
                       youngsModulus=youngsModulus, &
                       poissonRatio=poissonRatio, &
                       lambda=lambda)

  IF (isPlaneStress0) THEN
    CALL Get_PlaneStress_C_InvC(C=obj%C, invC=obj%invC, &
                                youngsModulus=obj%E, nu=obj%nu)
  ELSE IF (isPlaneStrain0) THEN
    CALL Get_PlaneStrain_C_InvC(C=obj%C, invC=obj%invC, youngsModulus=obj%E, &
                                nu=obj%nu)
  ELSE
    CALL Get_3D_C_InvC(C=obj%C, invC=obj%invC, youngsModulus=obj%E, &
                       nu=obj%nu)
  END IF

  myprefix = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN

END IF

! The following code is executed for non-isotropic elasticity types
abool = PRESENT(c)
IF (abool) THEN

#ifdef DEBUG_VER
  shapeOfC = SHAPE(c)
  isok = (shapeOfC(1) .GE. obj%nc) .AND. (shapeOfC(2) .GE. obj%nc)
  CALL AssertError1(isok, myName, &
      'The shape of c should be at least [3,3] in case of plane-stress or &
      &plane-strain. In 3D case it should be [6,6]. But it is '// &
      ToString(shapeOfC))
#endif

  obj%c(1:obj%nc, 1:obj%nc) = c(1:obj%nc, 1:obj%nc)

END IF

abool = PRESENT(invC)
IF (abool) THEN

#ifdef DEBUG_VER
  shapeOfC = SHAPE(invC)
  isok = (shapeOfC(1) .GE. obj%nc) .AND. (shapeOfC(2) .GE. obj%nc)
  CALL AssertError1(isok, myName, &
      'The shape of invC should be at least [3,3] in case of plane-stress or &
      &plane-strain. In 3D case it should be [6,6]. But it is '// &
      ToString(shapeOfC))
#endif

  obj%invC(1:obj%nc, 1:obj%nc) = invC(1:obj%nc, 1:obj%nc)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractSolidMechanicsModelDeallocate(obj)
obj%elasticityType = -1
obj%nu = 0.0
obj%G = 0.0
obj%E = 0.0
obj%lambda = 0.0
obj%C = 0.0_DFP
obj%invC = 0.0_DFP
obj%stiffnessPower = 0.0_DFP
obj%nc = 6

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
