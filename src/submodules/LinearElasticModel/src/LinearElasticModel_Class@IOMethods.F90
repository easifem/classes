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
USE BaseMethod
USE TomlUtility
USE tomlf, ONLY:  &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_array,  &
  & toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import"
INTEGER(I4B) :: elasticityType
TYPE(String) :: dsetname, strval
LOGICAL(LGT) :: isPlaneStrain, isPlaneStress, isIsotropic
REAL(DFP) :: poissonRatio, youngsModulus, shearModulus, lambda, stiffnessPower
REAL(DFP), ALLOCATABLE :: C(:, :), invC(:, :)
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Import()')
#endif

IF (obj%isInitiated()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: The object is already initiated, deallocate first!')
END IF

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isRead()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: HDF5 file does not have read permission')
END IF

! READ name
dsetname = TRIM(group)//"/name"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: The dataset name should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)

! READ elasticityType
dsetname = TRIM(group)//"/elasticityType"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: The dataset elasticityType should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
elasticityType = ElasticityType_tonumber(strval%chars())

! READ isPlaneStrain
dsetname = TRIM(group)//"/isPlaneStrain"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=isPlaneStrain)
ELSE
  isPlaneStrain = .FALSE.
END IF

! READ isPlaneStress
dsetname = TRIM(group)//"/isPlaneStress"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=isPlaneStress)
ELSE
  isPlaneStress = .FALSE.
END IF

isIsotropic = elasticityType .EQ. TypeElasticityOpt%Isotropic

IF (isIsotropic) THEN
  dsetname = TRIM(group)//"/poissonRatio"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[CONFIG ERROR] :: The dataset poissonRatio should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=poissonRatio)
  END IF

  dsetname = TRIM(group)//"/youngsModulus"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[CONFIG ERROR] :: The dataset youngsModulus should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=youngsModulus)
  END IF
  dsetname = TRIM(group)//"/shearModulus"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[CONFIG ERROR] :: The dataset shearModulus should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=shearModulus)
  END IF
  dsetname = TRIM(group)//"/lambda"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[CONFIG ERROR] :: The dataset lambda should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=lambda)
  END IF
ELSE
  dsetname = TRIM(group)//"/c"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[CONFIG ERROR] :: The dataset c should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=C)
  END IF
  dsetname = TRIM(group)//"/invC"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[CONFIG ERROR] :: The dataset invC should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=invC)
  END IF
END IF

dsetname = TRIM(group)//"/stiffnessPower"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=stiffnessPower)
ELSE
  stiffnessPower = 0.0_DFP
END IF

CALL param%initiate()

CALL SetLinearElasticModelParam( &
  & param=param, &
  & elasticityType=elasticityType, &
  & isPlaneStrain=isPlaneStrain, &
  & isPlaneStress=isPlaneStress, &
  & poissonRatio=poissonRatio, &
  & youngsModulus=youngsModulus, &
  & shearModulus=shearModulus, &
  & lambda=lambda, &
  & stiffnessPower=stiffnessPower, &
  & C=C, &
  & invC=invC)

CALL obj%Initiate(param)

CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Import()')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export"
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Export()')
#endif

IF (.NOT. obj%isInitiated()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: The object is not initiated, initiate first!')
END IF

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: HDF5 file does not have write permission')
END IF

! WRITE name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

! WRITE elasticityType
dsetname = TRIM(group)//"/elasticityType"
SELECT CASE (obj%elasticityType)
CASE (IsoLinearElasticModel)
  strval = "ISO"
CASE (AnisoLinearElasticModel)
  strval = "ANISO"
CASE (OrthoLinearElasticModel)
  strval = "ORTHO"
CASE (TransLinearElasticModel)
  strval = "TRANS"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

! WRITE isPlaneStrain
dsetname = TRIM(group)//"/isPlaneStrain"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isPlaneStrain())

! WRITE isPlaneStress
dsetname = TRIM(group)//"/isPlaneStress"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isPlaneStress())

! C and invC
IF (obj%elasticityType .EQ. IsoLinearElasticModel) THEN
  dsetname = TRIM(group)//"/poissonRatio"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%nu)
  dsetname = TRIM(group)//"/youngsModulus"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%E)
  dsetname = TRIM(group)//"/shearModulus"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%G)
  dsetname = TRIM(group)//"/lambda"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%lambda)
  dsetname = TRIM(group)//"/c"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%C)
  dsetname = TRIM(group)//"/invC"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%invC)
ELSE
  dsetname = TRIM(group)//"/c"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%C)
  dsetname = TRIM(group)//"/invC"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%invC)
END IF

dsetname = TRIM(group)//"/stiffnessPower"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%stiffnessPower)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Export()')
#endif

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: isPlaneStress
LOGICAL(LGT) :: isPlaneStrain

CALL Display(TRIM(msg), unitNo=unitNo)

IF (.NOT. obj%isInitiated()) THEN
  CALL Display("LinearElasticModel is not initiated! &
  & Noting to display", unitNo=unitNo)
END IF

isPlaneStress = obj%isPlaneStress()
isPlaneStrain = obj%isPlaneStrain()

CALL Display("name: LinearElasticModel", unitNo=unitNo)

IF (obj%elasticityType .EQ. IsoLinearElasticModel) THEN
  CALL Display("elasticityType: IsoLinearElasticModel", &
  & unitNo=unitNo)
ELSE IF (obj%elasticityType .EQ. AnisoLinearElasticModel) THEN
  CALL Display("elasticityType: AnisoLinearElasticModel", &
  & unitNo=unitNo)
ELSE IF (obj%elasticityType .EQ. OrthoLinearElasticModel) THEN
  CALL Display("elasticityType: OrthoLinearElasticModel", &
  & unitNo=unitNo)
ELSE IF (obj%elasticityType .EQ. TransLinearElasticModel) THEN
  CALL Display("elasticityType: TransLinearElasticModel", &
  & unitNo=unitNo)
END IF

CALL Display(isPlaneStress, "isPlaneStress: ", unitNo=unitNo)
CALL Display(isPlaneStrain, "isPlaneStrain: ", unitNo=unitNo)
CALL Display(obj%stiffnessPower, "stiffnessPower: ", unitNo=unitNo)

IF (obj%elasticityType .EQ. IsoLinearElasticModel) THEN
  CALL Display(obj%nu, "Poisson's ratio: ", unitNo=unitNo)
  CALL Display(obj%G, "Shear modulus: ", unitNo=unitNo)
  CALL Display(obj%E, "Young's modulus: ", unitNo=unitNo)
  CALL Display(obj%lambda, "Lambda: ", unitNo=unitNo)
END IF

CALL Display(obj%C(1:obj%nc, 1:obj%nc), "tangent matrix: ", &
  & unitNo=unitNo)
CALL Display(obj%invC(1:obj%nc, 1:obj%nc), "compliance matrix: ", &
  & unitNo=unitNo)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
CHARACTER(:), ALLOCATABLE :: name, elas_type_char
INTEGER(I4B) :: origin, stat, elasticityType
LOGICAL(LGT) :: bool1, isPlaneStress, isPlaneStrain, isFound_c,  &
& isFound_invc, isIsotropic
REAL(DFP) :: poissonRatio, youngsModulus, stiffnessPower
REAL(DFP), ALLOCATABLE :: c(:, :), invc(:, :)
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL toml_get(table, "name", name, origin=origin, stat=stat)
bool1 = (stat .NE. toml_stat%success) .OR. (.NOT. ALLOCATED(name))
IF (bool1) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'name field not found in toml file, so using name = '//  &
    & LinearElasticModel_Prefix)
  name = LinearElasticModel_Prefix
END IF

isPlaneStrain = .FALSE.
CALL toml_get(table, "isPlaneStrain", isPlaneStrain, .FALSE., &
  & origin=origin, stat=stat)

isPlaneStress = .FALSE.
CALL toml_get(table, "isPlaneStress", isPlaneStress, .FALSE., &
  & origin=origin, stat=stat)

CALL toml_get(table, "elasticityType", elas_type_char, origin=origin,  &
  & stat=stat)
bool1 = (stat .NE. toml_stat%success) .OR. (.NOT. ALLOCATED(elas_type_char))
IF (bool1) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'elasticityType field not found in toml file, so using  '//  &
    & 'elasticityType ='//TypeElasticityOpt%Isotropic_char)
  elas_type_char = TypeElasticityOpt%Isotropic_char
END IF

elasticityType = ElasticityType_tonumber(elas_type_char)

CALL toml_get(table, "poissonRatio", poissonRatio, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  IF (elasticityType .EQ. TypeElasticityOpt%Isotropic) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
      & 'In case of Isotropic elasticity '//  &
      & 'poissonRatio (missing) and youngsModulus should be present.'//  &
      & 'using default value = 0.33')
    poissonRatio = 0.33
  END IF
END IF

isIsotropic = elasticityType .EQ. TypeElasticityOpt%Isotropic

CALL toml_get(table, "youngsModulus", youngsModulus, origin=origin,  &
  & stat=stat)
bool1 = (stat .NE. toml_stat%success) .AND. isIsotropic
IF (bool1) THEN
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'In case of Isotropic elasticity '//  &
    & 'poissonRatio and youngsModulus (missing) should be present.'//  &
    & ' Using default value = 3.0e+6.')
  youngsModulus = 3.0E+6
END IF

CALL toml_get(table, "stiffnessPower", stiffnessPower, 0.0_DFP,  &
  & origin=origin, stat=stat)

isFound_c = .TRUE.
CALL GetValue(table=table, key="c", VALUE=c, origin=origin, stat=stat,  &
  & isFound=isFound_c)
bool1 = ((stat .NE. toml_stat%success) .OR. (.NOT. isFound_c)  &
      & .OR. (.NOT. ALLOCATED(c))) .AND. (.NOT. isIsotropic)

IF (bool1) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'In case of Anisotropic elasticity '//  &
    & 'c should be present. c should be a 3 by 3 matrix for '//  &
    & 'plane-stress and plane-strain case. c should be 6 by c '//  &
    & 'in other cases. Using default value identity.')
  c = eye(6, 1.0_DFP)
END IF

isFound_invc = .TRUE.
CALL GetValue(table=table, key="invC", VALUE=invC, origin=origin,  &
  & stat=stat, isFound=isFound_invc)
bool1 = ((stat .NE. toml_stat%success) .OR. (.NOT. isFound_invc) .OR.  &
      & (.NOT. ALLOCATED(invC))) .AND. (.NOT. isIsotropic)

IF (bool1) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'In case of Anisotropic elasticity '//  &
    & 'invC should be present. invC should be a 3 by 3 matrix for '//  &
    & 'plane-stress and plane-strain case. invC should be 6 by invC '//  &
    & 'in other cases. using default value identity.')
  invC = eye(6, 1.0_DFP)
END IF

CALL param%Initiate()
IF (isIsotropic) THEN
  CALL SetLinearElasticModelParam(param=param,  &
    & elasticityType=elasticityType, isPlaneStrain=isPlaneStrain,  &
    & isPlaneStress=isPlaneStress, poissonRatio=poissonRatio,  &
    & youngsModulus=youngsModulus, stiffnessPower=stiffnessPower)
ELSE
  CALL SetLinearElasticModelParam(param=param,  &
    & elasticityType=elasticityType, isPlaneStrain=isPlaneStrain,  &
    & isPlaneStress=isPlaneStress, stiffnessPower=stiffnessPower,  &
    & C=c, invC=invC)
END IF

CALL obj%Initiate(param)
CALL param%DEALLOCATE()

IF (ALLOCATED(c)) DEALLOCATE (c)
IF (ALLOCATED(invC)) DEALLOCATE (invC)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
