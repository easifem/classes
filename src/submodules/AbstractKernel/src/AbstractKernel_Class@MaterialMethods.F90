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

SUBMODULE(AbstractKernel_Class) MaterialMethods
USE BaseMethod
USE FieldFactory
USE KernelUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         AddSolidMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSolidMaterial
CHARACTER(*), PARAMETER :: myName = "obj_AddSolidMaterial"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. ALLOCATED(obj%solidMaterialToMesh)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: solidMaterialToMesh is not allocated!')
  RETURN
END IF

CALL AddSolidMaterial(obj=obj%solidMaterial,  &
  & tMaterials=obj%tSolidMaterials,  &
  & materialNo=materialNo,  &
  & param=param,  &
  & region=region,  &
  & solidMaterialToMesh=obj%solidMaterialToMesh)
!! INFO: AddSolidMaterial is defined in SolidMaterial_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_AddSolidMaterial

!----------------------------------------------------------------------------
!                                                       InitiateMassDensity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMassDensity
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMassDensity()"
LOGICAL(LGT) :: isok
#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

isok = ALLOCATED(obj%solidMaterial)
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[NOTHING TODO] :: AbstractKernel_::obj%solidMaterial is not allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom not ASSOCIATED.')
  RETURN
END IF

CALL KernelInitiateScalarProperty(vars=obj%massDensity,  &
  & materials=obj%solidMaterial, dom=obj%dom, nnt=obj%nnt,  &
  & varname="massDensity", matid=obj%SOLID_MATERIAL_ID,  &
  & engine=obj%engine%chars())

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_InitiateMassDensity

!----------------------------------------------------------------------------
!                                               InitiateElasticityProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElasticityProperties
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElasticityProperties()"
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

isok = ALLOCATED(obj%solidMaterial)
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[NOTHING TODO] :: AbstractKernel_::obj%solidMaterial is not allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom not ASSOCIATED.')
  RETURN
END IF

CALL KernelInitiateScalarProperty(vars=obj%lame_lambda,  &
  & materials=obj%solidMaterial, dom=obj%dom, nnt=obj%nnt,  &
  & varname="lambda", matid=obj%SOLID_MATERIAL_ID,  &
  & engine=obj%engine%chars())

CALL KernelInitiateScalarProperty(vars=obj%lame_mu,  &
  & materials=obj%solidMaterial, dom=obj%dom, nnt=obj%nnt,  &
  & varname="mu", matid=obj%SOLID_MATERIAL_ID,  &
  & engine=obj%engine%chars())

CALL KernelInitiateTensorProperty(vars=obj%Cijkl,  &
  & materials=obj%solidMaterial, dom=obj%dom, nnt=obj%nnt,  &
  & varname="Cijkl", matid=obj%SOLID_MATERIAL_ID,  &
  & engine=obj%engine%chars())

CALL KernelInitiateConstantElasticityProperties(lambda=obj%lame_lambda,  &
  & mu=obj%lame_mu, Cijkl=obj%Cijkl, dom=obj%dom,  &
  & nnt=obj%nnt, engine=obj%engine%chars())

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_InitiateElasticityProperties

!----------------------------------------------------------------------------
!                                                  InitiateConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMaterialProperties
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMaterialProperties"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%InitiateMassDensity()
CALL obj%InitiateElasticityProperties()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateMaterialProperties

!----------------------------------------------------------------------------
!                                                             SetMassDensity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMassDensity
CHARACTER(*), PARAMETER :: myName = "obj_SetMassDensity()"
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

isok = ALLOCATED(obj%solidMaterial)
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[NOTHING TODO] :: AbstractKernel_::obj%solidMaterial is not allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom not ASSOCIATED.')
  RETURN
END IF

CALL KernelSetScalarProperty(vars=obj%massDensity,  &
  & materials=obj%solidMaterial, dom=obj%dom, timeVec=obj%timeVec,  &
  & varname="massDensity", matid=obj%SOLID_MATERIAL_ID)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_SetMassDensity

!----------------------------------------------------------------------------
!                                                    SetElasticityProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElasticityProperties
CHARACTER(*), PARAMETER :: myName = "obj_SetElasticityProperties()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_SetElasticityProperties

!----------------------------------------------------------------------------
!                                                       SetConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterialProperties
CHARACTER(*), PARAMETER :: myName = "obj_SetConstantMatProp"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_SetMaterialProperties

END SUBMODULE MaterialMethods
