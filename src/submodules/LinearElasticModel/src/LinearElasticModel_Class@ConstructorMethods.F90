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
USE BaseMethod, ONLY: Input, UpperCase, ToString
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   ElasticityType_tonumber
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticityType_tonumber
CHARACTER(*), PARAMETER :: myName = "ElasticityType_tonumber"
TYPE(String) :: name0

name0 = UpperCase(name)
SELECT CASE (name0%chars())
CASE ("ISO")
  ans = TypeElasticity%Isotropic
CASE ("ANISO")
  ans = TypeElasticity%Anisotropic
CASE ("ORTHO")
  ans = TypeElasticity%Orthotropic
CASE ("TRANS")
  ans = TypeElasticity%TransIsotropic
CASE default
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'NO CASE FOUND for name = '//name0)
  RETURN
END SELECT

name0 = ""
END PROCEDURE ElasticityType_tonumber

!----------------------------------------------------------------------------
!                                                 ElasticityType_char
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticityType_char
CHARACTER(*), PARAMETER :: myName = "ElasticityType_char"
SELECT CASE (num)
CASE (IsoLinearElasticModel)
  ans = TypeElasticity%Isotropic_char
CASE (AnisoLinearElasticModel)
  ans = TypeElasticity%Anisotropic_char
CASE (OrthoLinearElasticModel)
  ans = TypeElasticity%Orthotropic_char
CASE (TransLinearElasticModel)
  ans = TypeElasticity%TransIsotropic_chars
CASE default
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] No case found for elasticityType = '//   &
    & tostring(num))
  RETURN
END SELECT
END PROCEDURE ElasticityType_char

!----------------------------------------------------------------------------
!                                                setLinearElasticModelParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetLinearElasticModelParam
CHARACTER(*), PARAMETER :: myName = "SetLinearElasticModelParam()"
INTEGER(I4B) :: ierr
REAL(DFP) :: lam, EE, nu, G
TYPE(String) :: astr
LOGICAL(LGT) :: isIsotropic

CALL Set(obj=param, datatype="char", prefix=myprefix, key="name",  &
  & VALUE=myprefix)

astr = ElasticityType_char(elasticityType)

CALL Set(obj=param, datatype="char", prefix=myprefix,  &
  & key="elasticityType", VALUE=astr%chars())

CALL Set(obj=param, datatype=.TRUE., prefix=myprefix, key="isPlaneStrain",  &
  & VALUE=INPUT(option=isPlaneStrain, default=.FALSE.))

CALL Set(obj=param, datatype=.TRUE., prefix=myprefix, key="isPlaneStress",  &
  & VALUE=input(option=isPlaneStress, default=.FALSE.))

isIsotropic = elasticityType .EQ. TypeElasticity%Isotropic

IF (isIsotropic) THEN
  CALL GetElasticParam(lam=lam, G=G, EE=EE, nu=nu, &
    & ShearModulus=ShearModulus, YoungsModulus=YoungsModulus, &
    & PoissonRatio=PoissonRatio, lambda=lambda)

  CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix, key="lambda",  &
    & VALUE=lam)
  CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix,  &
    & key="shearModulus", VALUE=G)
  CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix,  &
    & key="poissonRatio", VALUE=nu)
  CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix,  &
    & key="youngsModulus", VALUE=EE)

END IF

IF (.NOT. isIsotropic) THEN

  IF (.NOT. PRESENT(C)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[CONFIG ERROR] :: In case of Anisotropic '//  &
      & CHAR_LF//' Elasticity C and invC should be present.')
  END IF

  IF (.NOT. PRESENT(invC)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[CONFIG ERROR] :: In case of Anisotropic Elasticity '//  &
      & 'C and invC should be present')
  END IF

  ierr = param%Set(key=myprefix//"/c", VALUE=C)
  ierr = param%Set(key=myprefix//"/invC", VALUE=invC)
END IF

CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix, key="stiffnessPower",&
  & VALUE=INPUT(option=stiffnessPower, default=0.0_DFP))

END PROCEDURE SetLinearElasticModelParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "lem_CheckEssentialParam()"
CHARACTER(15) :: charVar
INTEGER(I4B) :: ierr, cc
INTEGER(I4B), ALLOCATABLE :: shapeOfC(:)
LOGICAL(LGT) :: isPlaneStress, isPlaneStrain

IF (.NOT. param%IsPresent(key=myprefix//"/name")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & myprefix//'/name should be present in param')
END IF

IF (.NOT. param%IsPresent(key=myprefix//"/elasticityType")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & myprefix//'/elasticityType should be present in param')
END IF

IF (param%IsPresent(key=myprefix//"/isPlaneStress")) THEN
  ierr = param%get(key=myprefix//"/isPlaneStress", &
    & VALUE=isPlaneStress)
END IF

IF (param%IsPresent(key=myprefix//"/isPlaneStrain")) THEN
  ierr = param%get(key=myprefix//"/isPlaneStrain", &
    & VALUE=isPlaneStrain)
END IF

IF (isPlaneStress .AND. isPlaneStrain) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Both '//myprefix//'/isPlaneStress and '//myprefix//'/ &
    & isPlaneStrain parameters are true, which is conflicting with each &
    & other. Choose one.')
END IF

ierr = param%get(key=myprefix//"/elasticityType", VALUE=charVar)
cc = 0

IF (TRIM(charVar) .EQ. TypeElasticity%Isotropic_char) THEN
  IF (.NOT. param%IsPresent(key=myprefix//"/lambda")) &
    & CALL e%RaiseError(modName//'::'//myName//" - "// &
    & myprefix//'/lambda should be present in param')

  IF (.NOT. param%IsPresent(key=myprefix//"/shearModulus")) &
    & CALL e%RaiseError(modName//'::'//myName//" - "// &
    & myprefix//'/shearModulus should be present in param')

  IF (.NOT. param%IsPresent(key=myprefix//"/poissonRatio")) &
    & CALL e%RaiseError(modName//'::'//myName//" - "// &
    & myprefix//'/poissonRatio should be present in param')

  IF (.NOT. param%IsPresent(key=myprefix//"/youngsModulus")) &
    & CALL e%RaiseError(modName//'::'//myName//" - "// &
    & myprefix//'/youngsModulus should be present in param')

ELSE

  IF (.NOT. param%IsPresent(key=myprefix//"/c")) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & myprefix//'/c should be present for anisotropic elasticity')
    RETURN
  END IF

  ierr = param%GetShape(key=myprefix//"/c", shape=shapeOfC)
  IF (ANY(shapeOfC .NE. [6, 6])) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The shape of '//myprefix//'/c should be [6,6]')
    RETURN
  END IF

  IF (.NOT. param%IsPresent(key=myprefix//"/invC")) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & myprefix//'/invC should be present for anisotropic elasticity')
    RETURN
  END IF

  ierr = param%GetShape(key=myprefix//"/invC", shape=shapeOfC)
  IF (ANY(shapeOfC .NE. [6, 6])) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The shape of '//myprefix//'/invC should be [6,6]')
    RETURN
  END IF

END IF

IF (ALLOCATED(shapeOfC)) DEALLOCATE (shapeOfC)

END PROCEDURE lem_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_Initiate
CHARACTER(*), PARAMETER :: myName = "lem_Initiate()"
CHARACTER(15) :: charVar
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: isPlaneStress
LOGICAL(LGT) :: isPlaneStrain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Initiate()')
#endif

CALL obj%DEALLOCATE()

CALL obj%CheckEssentialParam(param)
CALL obj%SetIsInitiated(.TRUE.)
CALL obj%SetName(myprefix)

ierr = param%get(key=myprefix//"/isPlaneStress", VALUE=isPlaneStress)
CALL obj%SetPlaneStress(isPlaneStress)

ierr = param%get(key=myprefix//"/isPlaneStrain", VALUE=isPlaneStrain)
CALL obj%SetPlaneStrain(isPlaneStrain)

ierr = param%get(key=myprefix//"/elasticityType", VALUE=charVar)
obj%elasticityType = elasticityType_tonumber(charVar)

IF (obj%elasticityType .EQ. IsoLinearElasticModel) THEN
  ierr = param%get(key=myprefix//"/lambda", VALUE=obj%lambda)
  ierr = param%get(key=myprefix//"/shearModulus", VALUE=obj%G)
  ierr = param%get(key=myprefix//"/youngsModulus", VALUE=obj%E)
  ierr = param%get(key=myprefix//"/poissonRatio", VALUE=obj%nu)
  IF (isPlaneStress) THEN
    CALL Get_PlaneStress_C_InvC(C=obj%C, invC=obj%invC,  &
      & youngsModulus=obj%E, nu=obj%nu)
  ELSEIF (isPlaneStrain) THEN
    CALL Get_PlaneStrain_C_InvC(C=obj%C, invC=obj%invC,  &
      & youngsModulus=obj%E, nu=obj%nu)
  ELSE
    CALL Get_3D_C_InvC(C=obj%C, invC=obj%invC,  &
      & youngsModulus=obj%E, nu=obj%nu)
  END IF
ELSE
  ierr = param%get(key=myprefix//"/c", VALUE=obj%C)
  ierr = param%get(key=myprefix//"/invC", VALUE=obj%invC)
END IF

ierr = param%get(key=myprefix//"/stiffnessPower", VALUE=obj%stiffnessPower)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Initiate()')
#endif

END PROCEDURE lem_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_Deallocate
CALL AbstractSolidMechanicsModelDeallocate(obj)
obj%elasticityType = -1
obj%nu = 0.0
obj%G = 0.0
obj%E = 0.0
obj%lambda = 0.0
obj%C = 0.0_DFP
obj%invC = 0.0_DFP
obj%stiffnessPower = 0.0_DFP
END PROCEDURE lem_Deallocate

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_Final
CALL obj%DEALLOCATE()
END PROCEDURE lem_Final

END SUBMODULE ConstructorMethods
