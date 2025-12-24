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
! date: 27 Aug 2021
! summary: This submodule contains input-output methods

SUBMODULE(LinearElasticModel_Class) IOMethods
USE Display_Method, ONLY: Display, ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: abool
LOGICAL(LGT) :: isPlaneStress
LOGICAL(LGT) :: isPlaneStrain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)

abool = obj%isInitiated()
CALL Display(abool, "isInitiated: ", unitNo=unitNo)

IF (.NOT. abool) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isPlaneStress = obj%isPlaneStress()
isPlaneStrain = obj%isPlaneStrain()

CALL Display("name: LinearElasticModel", unitNo=unitNo)

SELECT CASE (obj%elasticityType)
CASE (TypeElasticityOpt%isotropic)
  CALL Display("elasticityType: IsoLinearElasticModel", unitNo=unitNo)
CASE (TypeElasticityOpt%anisotropic)
  CALL Display("elasticityType: AnisoLinearElasticModel", unitNo=unitNo)
CASE (TypeElasticityOpt%orthotropic)
  CALL Display("elasticityType: OrthoLinearElasticModel", unitNo=unitNo)
CASE (TypeElasticityOpt%transIsotropic)
  CALL Display("elasticityType: TransLinearElasticModel", unitNo=unitNo)
CASE DEFAULT
  CALL Display("unknown elasticityType: "//ToString(obj%elasticityType), &
               unitNo=unitNo)
END SELECT

CALL Display(isPlaneStress, "isPlaneStress: ", unitNo=unitNo)
CALL Display(isPlaneStrain, "isPlaneStrain: ", unitNo=unitNo)
CALL Display(obj%stiffnessPower, "stiffnessPower: ", unitNo=unitNo)

IF (obj%elasticityType .EQ. TypeElasticityOpt%isotropic) THEN
  CALL Display(obj%nu, "Poisson's ratio: ", unitNo=unitNo)
  CALL Display(obj%G, "Shear modulus: ", unitNo=unitNo)
  CALL Display(obj%E, "Young's modulus: ", unitNo=unitNo)
  CALL Display(obj%lambda, "Lambda: ", unitNo=unitNo)
END IF

CALL Display(obj%C(1:obj%nc, 1:obj%nc), "tangent matrix: ", &
             unitNo=unitNo)
CALL Display(obj%invC(1:obj%nc, 1:obj%nc), "compliance matrix: ", &
             unitNo=unitNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
