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
  & tMaterials=obj%tMaterials,  &
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
!                                                  InitiateConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMaterialProperties
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMaterialProperties"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_InitiateMaterialProperties

!----------------------------------------------------------------------------
!                                                       SetConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterialProperties
CHARACTER(*), PARAMETER :: myName = "obj_SetConstantMatProp"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetMaterialProperties

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MaterialMethods
