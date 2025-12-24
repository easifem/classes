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
!                                                setLinearElasticModelParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetLinearElasticModelParam
CHARACTER(*), PARAMETER :: myName = "SetLinearElasticModelParam()"
INTEGER(I4B) :: ierr, nc
REAL(DFP) :: lam, EE, nu, G, def_c(6, 6), def_c_inv(6, 6), shapeOfC(2)
TYPE(String) :: astr
LOGICAL(LGT) :: isIsotropic, problem, isPlaneStrain0, isPlaneStress0, &
                shapeProblem(2), is2D

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Set(obj=param, datatype="char", prefix=myprefix, key="name", &
         VALUE=myprefix)

astr = TypeElasticityOpt%ToString(elasticityType)

CALL Set(obj=param, datatype="char", prefix=myprefix, &
         key="elasticityType", VALUE=astr%chars())

astr = ""

nc = 6
isPlaneStrain0 = INPUT(option=isPlaneStrain, default=.FALSE.)
CALL Set(obj=param, datatype=.TRUE., prefix=myprefix, key="isPlaneStrain", &
         VALUE=isPlaneStrain0)
IF (isPlaneStrain0) nc = TypeElasticityOpt%size_c_plane_strain

isPlaneStress0 = input(option=isPlaneStress, default=.FALSE.)
CALL Set(obj=param, datatype=.TRUE., prefix=myprefix, key="isPlaneStress", &
         VALUE=isPlaneStress0)
IF (isPlaneStress0) nc = TypeElasticityOpt%size_c_plane_stress

is2D = (isPlaneStress0 .OR. isPlaneStrain0)

CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix, key="stiffnessPower", &
         VALUE=INPUT(option=stiffnessPower, default=0.0_DFP))

lam = 1.0; G = 1.0; EE = 1.0; nu = 0.3
isIsotropic = elasticityType .EQ. TypeElasticityOpt%Isotropic
IF (isIsotropic) THEN
  CALL GetElasticParam(lam=lam, G=G, EE=EE, nu=nu, &
                       shearModulus=shearModulus, &
                       youngsModulus=youngsModulus, &
                       poissonRatio=poissonRatio, &
                       lambda=lambda)
END IF

CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix, key="lambda", &
         VALUE=lam)
CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix, &
         key="shearModulus", VALUE=G)
CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix, &
         key="poissonRatio", VALUE=nu)
CALL Set(obj=param, datatype=1.0_DFP, prefix=myprefix, &
         key="youngsModulus", VALUE=EE)

IF (PRESENT(c)) THEN
  shapeOfC = SHAPE(c)
  shapeProblem = shapeOfC .LT. [nc, nc]
  problem = is2D .AND. ANY(shapeProblem)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INTERNAL ERROR] :: The shape of c should be '// &
                'at least [3,3] in case of plane-stress or plane-strain. '// &
                      'In 3D case it should be [6,6]. '// &
                      'But it is '//tostring(shapeOfC))
    RETURN
  END IF
  def_c(1:nc, 1:nc) = c(1:nc, 1:nc)
ELSE
  def_c = 0.0_DFP
END IF

ierr = param%Set(key=myprefix//"/c", VALUE=def_c)

IF (PRESENT(invC)) THEN
  shapeOfC = SHAPE(invC)
  shapeProblem = shapeOfC .LT. [nc, nc]
  problem = ANY(shapeProblem)

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INTERNAL ERROR] :: The shape of invC should be '// &
                'at least [3,3] in case of plane-stress or plane-strain. '// &
                      'In 3D case it should be [6,6]. '// &
                      'But it is '//tostring(shapeOfC))
    RETURN
  END IF
  def_c_inv(1:nc, 1:nc) = invC(1:nc, 1:nc)
ELSE
  def_c_inv = 0.0_DFP
END IF

ierr = param%Set(key=myprefix//"/invC", VALUE=def_c_inv)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE SetLinearElasticModelParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
INTEGER(I4B) :: ierr, ii
LOGICAL(LGT) :: isPlaneStress, isPlaneStrain
TYPE(String), ALLOCATABLE :: essentialParam(:)
TYPE(String) :: astr

astr = '/name/elasticityType/isPlaneStress/isPlaneStrain/'// &
       'elasticityType/youngsModulus/lambda/shearModulus/poissonRatio/c/invC'

CALL astr%Split(essentialParam, sep='/')
CALL CheckEssentialParam(obj=param, &
                         keys=essentialParam, &
                         prefix=myprefix, &
                         myName=myName, &
                         modName=modName)
!note: CheckEssentialParam param is defined in easifemClasses FPL_Method

IF (ALLOCATED(essentialParam)) THEN
  DO ii = 1, SIZE(essentialParam)
    essentialParam(ii) = ''
  END DO
  DEALLOCATE (essentialParam)
END IF
astr = ''

ierr = param%Get(key=myprefix//"/isPlaneStress", VALUE=isPlaneStress)
ierr = param%Get(key=myprefix//"/isPlaneStrain", VALUE=isPlaneStrain)

IF (isPlaneStress .AND. isPlaneStrain) THEN
  CALL AssertError1(.FALSE., myName, &
       'Both isPlaneStress and isPlaneStrain parameters are true. &
       &Please choose one.')
  RETURN
END IF

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
CHARACTER(15) :: charVar
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: isPlaneStress
LOGICAL(LGT) :: isPlaneStrain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL obj%CheckEssentialParam(param)

CALL obj%SetIsInitiated(.TRUE.)
CALL obj%SetName(myprefix)

ierr = param%Get(key=myprefix//"/isPlaneStress", VALUE=isPlaneStress)
CALL obj%SetPlaneStress(isPlaneStress)
IF (isPlaneStress) obj%nc = TypeElasticityOpt%size_c_plane_stress

ierr = param%Get(key=myprefix//"/isPlaneStrain", VALUE=isPlaneStrain)
CALL obj%SetPlaneStrain(isPlaneStrain)
IF (isPlaneStrain) obj%nc = TypeElasticityOpt%size_c_plane_strain

ierr = param%Get(key=myprefix//"/elasticityType", VALUE=charVar)
obj%elasticityType = TypeElasticityOpt%ToNumber(charVar)

IF (obj%elasticityType .EQ. TypeElasticityOpt%isotropic) THEN
  ierr = param%Get(key=myprefix//"/lambda", VALUE=obj%lambda)
  ierr = param%Get(key=myprefix//"/shearModulus", VALUE=obj%G)
  ierr = param%Get(key=myprefix//"/youngsModulus", VALUE=obj%E)
  ierr = param%Get(key=myprefix//"/poissonRatio", VALUE=obj%nu)
  IF (isPlaneStress) THEN
    CALL Get_PlaneStress_C_InvC(C=obj%C, invC=obj%invC, &
                                youngsModulus=obj%E, nu=obj%nu)
  ELSEIF (isPlaneStrain) THEN
    CALL Get_PlaneStrain_C_InvC(C=obj%C, invC=obj%invC, &
                                youngsModulus=obj%E, nu=obj%nu)
  ELSE
    CALL Get_3D_C_InvC(C=obj%C, invC=obj%invC, &
                       youngsModulus=obj%E, nu=obj%nu)
  END IF
ELSE
  ierr = param%Get(key=myprefix//"/c", VALUE=obj%C)
  ierr = param%Get(key=myprefix//"/invC", VALUE=obj%invC)
END IF

ierr = param%Get(key=myprefix//"/stiffnessPower", VALUE=obj%stiffnessPower)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                 LinearElasticModelInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelInitiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelInitiate()"
#endif

LOGICAL(LGT) :: isok, isPlaneStress0, isPlaneStrain0, isIsotropic, abool
INTEGER(I4B) :: shapeOfC(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
CALL obj%SetIsInitiated(.TRUE.)
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

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN

END IF

abool = PRESENT(c)
IF (abool) THEN
#ifdef DEBUG_VER
  shapeOfC = SHAPE(c)
  isok = (shapeOfC(1) .GE. obj%nc) .AND. (shapeOfC(2) .GE. obj%nc)
  CALL AssertError1(isok, myName, &
      'The shape of c should be at least [3,3] in case of plane-stress or &
      &plane-strain. In 3D case it should be [6,6]. But it is '// &
      ToString(shapeOfC))
  obj%c(1:obj%nc, 1:obj%nc) = c(1:obj%nc, 1:obj%nc)
#endif
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
END PROCEDURE LinearElasticModelInitiate

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
