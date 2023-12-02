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

SUBMODULE(SolidMaterial_Class) SetMethods
USE BaseMethod, ONLY: ToString
USE MaterialFactory, ONLY: SolidMaterialFactory
! USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    obj_AddSolidMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSolidMaterial
CHARACTER(*), PARAMETER :: myName = "obj_AddSolidMaterial"

IF (materialNo .GT. tMaterials) THEN

  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[OUT OF BOUND ERROR] :: Given MaterialNo [='//TOSTRING(materialNo)// &
    & '] is greater than total number of solidMaterials [='//  &
    & TOSTRING(tMaterials)//']!')

END IF

IF (PRESENT(region) .AND. PRESENT(solidMaterialToMesh)) THEN

  IF (materialNo .GT. SIZE(solidMaterialToMesh)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[OUT OF BOUND ERROR] :: Given MaterialNo [='//TOSTRING(materialNo)// &
      & '] is greater than the size of solidMaterialToMesh [='//  &
      & TOSTRING(SIZE(solidMaterialToMesh))//']!')
  END IF
  solidMaterialToMesh(materialNo) = region

END IF

IF (PRESENT(param)) THEN

  IF (materialNo .GT. SIZE(obj)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[OUT OF BOUND ERROR] :: Given MaterialNo [='//TOSTRING(materialNo)// &
      & '] is greater than the size of solidMaterial[='//  &
      & TOSTRING(SIZE(obj))//']!')
  END IF

  IF (ASSOCIATED(obj(materialNo)%ptr)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[POINTER ERROR] :: solidMaterial('//TOSTRING(materialNo)// &
      & ')%ptr is already associated.')
  END IF

  IF (.NOT. PRESENT(materialName)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[ARG MISSING] :: materialName should be present.')
  END IF

  obj(materialNo)%ptr => &
    & SolidMaterialFactory(TRIM(materialName))
  !! INFO: Solid material factory is defined in MaterialFactory.

  CALL obj(materialNo)%ptr%initiate(param)

END IF

END PROCEDURE obj_AddSolidMaterial

END SUBMODULE SetMethods
