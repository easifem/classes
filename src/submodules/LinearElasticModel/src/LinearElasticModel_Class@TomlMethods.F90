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

SUBMODULE(LinearElasticModel_Class) TomlMethods
USE EyeUtility, ONLY: eye
USE TomlUtility, ONLY: GetValue, GetValue_
USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_array, &
                 toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            ReadNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadNameFromToml(obj, table, name)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: name

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadNameFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  CHARACTER(:), ALLOCATABLE :: default_value

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading name ...')
#endif

  default_value = obj%GetPrefix()
  CALL GetValue(table=table, key="name", VALUE=name, &
                default_value=default_value, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ReadNameFromToml

!----------------------------------------------------------------------------
!                                                            ReadNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsPlaneStrainFromToml(obj, table, isPlaneStrain)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  LOGICAL(LGT), INTENT(OUT) :: isPlaneStrain

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsPlaneStrainFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isPlaneStrain ...')
#endif

  CALL GetValue(table=table, key="isPlaneStrain", VALUE=isPlaneStrain, &
                default_value=.FALSE., &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ReadIsPlaneStrainFromToml

!----------------------------------------------------------------------------
!                                                  ReadIsPlaneStressFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsPlaneStressFromToml(obj, table, isPlaneStress)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  LOGICAL(LGT), INTENT(OUT) :: isPlaneStress

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsPlaneStressFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isPlaneStress ...')
#endif

  CALL GetValue(table=table, key="isPlaneStress", VALUE=isPlaneStress, &
                default_value=.FALSE., &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ReadIsPlaneStressFromToml

!----------------------------------------------------------------------------
!                                                  ReadElasticityTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadElasticityTypeFromToml(obj, table, elasticityType)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(OUT) :: elasticityType

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadElasticityTypeFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  TYPE(String) :: elasticityType_char

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading elasticityType...')
#endif

  CALL GetValue(table=table, key="elasticityType", &
                VALUE=elasticityType_char, &
                default_value=TypeElasticityOpt%isotropic_char, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  IF (.NOT. isok) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  'elasticityType field not found in toml table, so using elasticityType='// &
                            TypeElasticityOpt%isotropic_char)
  END IF
#endif

  elasticityType = TypeElasticityOpt%ToNumber(elasticityType_char%chars())

  elasticityType_char = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadElasticityTypeFromToml

!----------------------------------------------------------------------------
!                                                  ReadPoissonRatioFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadPoissonRatioFromToml(obj, table, poissonRatio)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  REAL(DFP), INTENT(OUT) :: poissonRatio

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadPoissonRatioFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  REAL(DFP), PARAMETER :: default_value = 0.33_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading poissonRatio ...')
#endif

  CALL GetValue(table=table, key="poissonRatio", &
                VALUE=poissonRatio, &
                default_value=default_value, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  IF (.NOT. isok) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
         'In case of Isotropic elasticity "poissonRatio" (missing) and &
         &"youngsModulus" should be present. Using poissonRatio = 0.33')
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadPoissonRatioFromToml

!----------------------------------------------------------------------------
!                                                   ReadYoungsModulusFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadYoungsModulusFromToml(obj, table, youngsModulus)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  REAL(DFP), INTENT(OUT) :: youngsModulus

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadYoungsModulusFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  REAL(DFP), PARAMETER :: default_value = 3.0E+6

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading youngsModulus ...')
#endif

  CALL GetValue(table=table, key="youngsModulus", &
                VALUE=youngsModulus, &
                default_value=default_value, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  IF (.NOT. isok) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
         'In case of Isotropic elasticity "youngsModulus" (missing) and &
         &"poissonRatio" should be present. Using youngsModulus = 3.0e+6')
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadYoungsModulusFromToml

!----------------------------------------------------------------------------
!                                                 ReadStiffnessPowerFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadStiffnessPowerFromToml(obj, table, stiffnessPower)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  REAL(DFP), INTENT(OUT) :: stiffnessPower

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadStiffnessPowerFromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  REAL(DFP), PARAMETER :: default_value = 0.0_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading stiffnessPower ...')
#endif

  CALL GetValue(table=table, key="stiffnessPower", &
                VALUE=stiffnessPower, &
                default_value=default_value, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  IF (.NOT. isok) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
               'stiffnesPower not found in toml table so using default value')
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadStiffnessPowerFromToml

!----------------------------------------------------------------------------
!                                                              ReadCFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadCFromToml(obj, table, c)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  REAL(DFP), INTENT(INOUT) :: c(6, 6)

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadCFromToml()"
#endif
  INTEGER(I4B) :: origin, stat, nrow, ncol
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading c ...')
#endif

  c = eye(6, 1.0_DFP)

  CALL GetValue_(table=table, key="c", VALUE=c, origin=origin, stat=stat, &
                 isFound=isok, nrow=nrow, ncol=ncol)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        'In case of Anisotropic elasticity c should be present. c should &
        &be a 3 by 3 matrix for plane-stress and plane-strain case. &
        &c should be 6 by 6 in other cases. Using default value identity.')
#endif
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadCFromToml

!----------------------------------------------------------------------------
!                                                           ReadInvCFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadInvCFromToml(obj, table, c)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  REAL(DFP), INTENT(INOUT) :: c(6, 6)

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadInvCFromToml()"
#endif
  INTEGER(I4B) :: origin, stat, nrow, ncol
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading invC ...')
#endif

  c = eye(6, 1.0_DFP)

  CALL GetValue_(table=table, key="invC", VALUE=c, origin=origin, stat=stat, &
                 isFound=isok, nrow=nrow, ncol=ncol)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
      'In case of Anisotropic elasticity invC should be present. invC should &
      &be a 3 by 3 matrix for plane-stress and plane-strain case. &
      &invC should be 6 by 6 in other cases. Using default value identity.')
#endif
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadInvCFromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: elasticityType
LOGICAL(LGT) :: isPlaneStress, isPlaneStrain
REAL(DFP) :: poissonRatio, youngsModulus, stiffnessPower
REAL(DFP) :: c(6, 6), invc(6, 6)
TYPE(String) :: name

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ReadNameFromToml(obj=obj, table=table, name=name)

CALL ReadIsPlaneStrainFromToml(obj=obj, table=table, &
                               isPlaneStrain=isPlaneStrain)

CALL ReadIsPlaneStressFromToml(obj=obj, table=table, &
                               isPlaneStress=isPlaneStress)

CALL ReadElasticityTypeFromToml(obj=obj, table=table, &
                                elasticityType=elasticityType)

CALL ReadPoissonRatioFromToml(obj=obj, table=table, &
                              poissonRatio=poissonRatio)

CALL ReadYoungsModulusFromToml(obj=obj, table=table, &
                               youngsModulus=youngsModulus)

CALL ReadStiffnessPowerFromToml(obj=obj, table=table, &
                                stiffnessPower=stiffnessPower)

CALL ReadCFromToml(obj=obj, table=table, c=c)

CALL ReadInvCFromToml(obj=obj, table=table, c=invC)

CALL LinearElasticModelInitiate(obj=obj, &
                                elasticityType=elasticityType, &
                                isPlaneStrain=isPlaneStrain, &
                                isPlaneStress=isPlaneStress, &
                                poissonRatio=poissonRatio, &
                                youngsModulus=youngsModulus, &
                                stiffnessPower=stiffnessPower, &
                                C=c, invC=invC)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE TomlMethods
