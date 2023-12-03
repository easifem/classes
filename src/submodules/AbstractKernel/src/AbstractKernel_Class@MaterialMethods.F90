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

SUBMODULE(AbstractKernel_Class) MaterialMethods
! USE FieldFactory
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         AddSolidMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSolidMaterial
CHARACTER(*), PARAMETER :: myName = "obj_AddSolidMaterial"

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')

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

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
END PROCEDURE obj_AddSolidMaterial

!----------------------------------------------------------------------------
!                                                  InitiateConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateConstantMatProp
CHARACTER(*), PARAMETER :: myName = "obj_InitiateConstantMatProp"

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR]')

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_InitiateConstantMatProp

!----------------------------------------------------------------------------
!                                                       SetConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetConstantMatProp
CHARACTER(*), PARAMETER :: myName = "obj_SetConstantMatProp"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by child class')
END PROCEDURE obj_SetConstantMatProp

!----------------------------------------------------------------------------
!                                                       SetVariableMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetVariableMatProp
CHARACTER(*), PARAMETER :: myName = "obj_SetVariableMatProp"
CALL e%RaiseError(modName//'::'//myName//' - '// &
 & '[IMPLEMENT ERROR] :: This routine should be implemented by child class')
END PROCEDURE obj_SetVariableMatProp

!----------------------------------------------------------------------------
!                                                  InitiateVariableMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateVariableMatProp
CHARACTER(*), PARAMETER :: myName = "obj_InitiateVariableMatProp"
CALL e%RaiseError(modName//'::'//myName//' - '// &
 & '[IMPLEMENT ERROR] :: This routine should be implemented by child class')
END PROCEDURE obj_InitiateVariableMatProp

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MaterialMethods
