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

SUBMODULE(LinearPoroElasticModel_Class) IOMethods
USE BaseMethod
USE LinearElasticModel_Class, ONLY: TypeElasticityOpt
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_Import
CHARACTER(*), PARAMETER :: myName = "lpem_Import"
INTEGER(I4B) :: elasticityType
TYPE(String) :: dsetname, strval
LOGICAL(LGT) :: isPlaneStrain, isPlaneStress, isIsotropic
REAL(DFP) :: PoissonRatio, YoungsModulus, ShearModulus, lambda
REAL(DFP), ALLOCATABLE :: C(:, :), invC(:, :)
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "[START] Import()")
#endif

IF (obj%isInitiated()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: The object is already initiated, deallocate first!')
END IF

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: HDF5 file does not have read permission')
END IF

dsetname = TRIM(group)//"/name"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: The dataset name should be present')
END IF

CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
CALL obj%SetName(strval%chars())
strval = ""

!> READ elasticityType
dsetname = TRIM(group)//"/elasticityType"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: The dataset elasticityType should be present')
END IF

CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
elasticityType = TypeElasticityOpt%ToNumber(strval%chars())

!> READ isPlaneStrain
dsetname = TRIM(group)//"/isPlaneStrain"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=isPlaneStrain)
ELSE
  isPlaneStrain = .FALSE.
END IF

!> READ isPlaneStress
dsetname = TRIM(group)//"/isPlaneStress"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=isPlaneStress)
ELSE
  isPlaneStress = .FALSE.
END IF

isIsotropic = elasticityType .EQ. TypeElasticityOpt%Isotropic
IF (isIsotropic) THEN
  dsetname = TRIM(group)//"/PoissonRatio"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & 'The dataset PoissonRatio should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=PoissonRatio)
  END IF

  dsetname = TRIM(group)//"/YoungsModulus"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & 'The dataset YoungsModulus should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=YoungsModulus)
  END IF

  dsetname = TRIM(group)//"/ShearModulus"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & 'The dataset ShearModulus should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=ShearModulus)
  END IF

  dsetname = TRIM(group)//"/lambda"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & 'The dataset lambda should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=lambda)
  END IF
ELSE
  dsetname = TRIM(group)//"/C"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & 'The dataset C should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=C)
  END IF
  dsetname = TRIM(group)//"/invC"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & 'The dataset invC should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=invC)
  END IF
END IF

CALL FPL_INIT(); CALL param%initiate()

CALL SetLinearPoroElasticModelParam(param=param, &
  & elasticityType=elasticityType, isPlaneStrain=isPlaneStrain, &
  & isPlaneStress=isPlaneStress, PoissonRatio=PoissonRatio, &
  & YoungsModulus=YoungsModulus, ShearModulus=ShearModulus, &
  & lambda=lambda, C=C, invC=invC)

CALL obj%initiate(param)

CALL param%DEALLOCATE(); CALL FPL_FINALIZE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Import')
#endif
END PROCEDURE lpem_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_Export
CHARACTER(*), PARAMETER :: myName = "lpem_Export"
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Export()')
#endif

IF (.NOT. obj%isInitiated()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: The object is not initiated, initiate first!')
END IF

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: HDF5 file does not have write permission')
END IF

!> WRITE name
dsetname = TRIM(group)//"/name"
strval = obj%GetName()
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
strval = ""

!> WRITE elasticityType
dsetname = TRIM(group)//"/elasticityType"
strval = TypeElasticityOpt%ToString(obj%elasticityType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

!> WRITE isPlaneStrain
dsetname = TRIM(group)//"/isPlaneStrain"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isPlaneStrain())

!> WRITE isPlaneStress
dsetname = TRIM(group)//"/isPlaneStress"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isPlaneStress())

!> C and invC
IF (obj%elasticityType .EQ. TypeElasticityOpt%Isotropic) THEN
  dsetname = TRIM(group)//"/PoissonRatio"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%nu)
  dsetname = TRIM(group)//"/YoungsModulus"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%E)
  dsetname = TRIM(group)//"/ShearModulus"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%G)
  dsetname = TRIM(group)//"/lambda"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%lambda)
  dsetname = TRIM(group)//"/C"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%C)
  dsetname = TRIM(group)//"/invC"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%invC)
ELSE
  dsetname = TRIM(group)//"/C"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%C)
  dsetname = TRIM(group)//"/invC"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%invC)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "[END] Export()")
#endif
END PROCEDURE lpem_Export

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_Display
LOGICAL(LGT) :: isPlaneStrain
LOGICAL(LGT) :: isPlaneStress

CALL Display(TRIM(msg), unitNo=unitNo)
CALL Display(obj%isInitiated(), "isInitiated: ", unitNo=unitNo)
IF (.NOT. obj%isInitiated()) THEN
  RETURN
END IF

CALL Display("name: "//myPrefix, unitNo=unitNo)
IF (obj%elasticityType .EQ. TypeElasticityOpt%Isotropic) THEN
  CALL Display("elasticityType: IsoLinearElasticModel", unitNo=unitNo)
ELSE IF (obj%elasticityType .EQ. TypeElasticityOpt%Anisotropic) THEN
  CALL Display("elasticityType: AnisoLinearElasticModel", unitNo=unitNo)
ELSE IF (obj%elasticityType .EQ. TypeElasticityOpt%Orthotropic) THEN
  CALL Display("elasticityType: OrthoLinearElasticModel", unitNo=unitNo)
ELSE IF (obj%elasticityType .EQ. TypeElasticityOpt%TransIsotropic) THEN
  CALL Display("elasticityType: TransLinearElasticModel", unitNo=unitNo)
ELSE
  CALL Display("elasticityType: Unknown", unitNo=unitNo)
END IF

isPlaneStrain = obj%isPlaneStrain()
isPlaneStress = obj%isPlaneStress()

CALL Display(isPlaneStress, "isPlaneStress: ", unitNo=unitNo)
CALL Display(isPlaneStrain, "isPlaneStrain: ", unitNo=unitNo)

IF (obj%elasticityType .EQ. TypeElasticityOpt%Isotropic) THEN

  CALL Display(obj%nu, "Poisson ratio: ", unitNo=unitNo)
  CALL Display(obj%G, "Shear modulus: ", unitNo=unitNo)
  CALL Display(obj%E, "Youngs modulus: ", unitNo=unitNo)
  CALL Display(obj%lambda, "Lambda: ", unitNo=unitNo)

  IF (isPlaneStress .OR. isPlaneStrain) THEN
    CALL Display(obj%C(1:3, 1:3), "Tangent matrix: ", &
      & unitNo=unitNo)
    CALL Display(obj%invC(1:3, 1:3), "Compliance matrix: ", &
      & unitNo=unitNo)
  ELSE
    CALL Display(obj%C, "Tangent matrix: ", &
      & unitNo=unitNo)
    CALL Display(obj%invC, "Compliance matrix: ", &
      & unitNo=unitNo)
  END IF

ELSE

  CALL Display(obj%C, "Tangent matrix: ", unitNo=unitNo)
  CALL Display(obj%invC, "Compliance matrix: ", unitNo=unitNo)

END IF

END PROCEDURE lpem_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
