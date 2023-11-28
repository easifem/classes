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

SUBMODULE(SolidMaterial_Class) ConstructorMethods
USE BaseMethod, ONLY: ToString
USE MaterialFactory
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     setSolidMaterialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSolidMaterialParam
CALL SetAbstractMaterialParam(param=param, prefix=myprefix, name=name)
CALL Set(obj=param, prefix=myprefix, key="stressStrainModel",  &
  & VALUE=stressStrainModel, dataType="char")
END PROCEDURE SetSolidMaterialParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE solid_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "solid_CheckEssentialParam()"
IF (.NOT. param%isPresent(key=myprefix//"/name")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & myprefix//'/name should be present in param')
END IF
IF (ASSOCIATED(obj%stressStrainModel)) THEN
  CALL obj%stressStrainModel%CheckEssentialParam(param)
END IF
END PROCEDURE solid_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE solid_Initiate
CHARACTER(*), PARAMETER :: myName = "solid_Initiate()"
TYPE(String) :: prefix0, stressStrainModel
LOGICAL(LGT) :: bool1
! main

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Initiate()')
#endif

IF (PRESENT(prefix)) THEN
  prefix0 = prefix
ELSE
  prefix0 = obj%GetPrefix()
END IF

CALL AbstractMaterialInitiate(obj=obj, param=param, prefix=prefix0%chars())

! stressStrainModel

CALL GetValue(obj=param, prefix=prefix0%chars(), key="stressStrainModel",  &
  & VALUE=stressStrainModel)

IF (ASSOCIATED(obj%stressStrainModel)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[CONFIG ERROR] :: The "//prefix0//"/stressStrainModel is "//  &
    & "already associated, "//  &
    & CHAR_LF//"nullify it first.")
  RETURN
END IF

bool1 = param%isPresent(key=prefix0//"/stressStrainModel")
IF (bool1) THEN
  obj%stressStrainModel => SolidMechanicsModelFactory( &
    & stressStrainModel%chars())
  CALL obj%stressStrainModel%Initiate(param)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Initiate()')
#endif
END PROCEDURE solid_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE solid_Deallocate
CALL AbstractMaterialDeallocate(obj)
IF (ASSOCIATED(obj%stressStrainModel)) THEN
  DEALLOCATE (obj%stressStrainModel)
  NULLIFY (obj%stressStrainModel)
END IF
END PROCEDURE solid_Deallocate

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE solid_Final
CALL obj%DEALLOCATE()
END PROCEDURE solid_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL obj(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                    solid_AddSolidMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE solid_AddSolidMaterial
CHARACTER(*), PARAMETER :: myName = "solid_AddSolidMaterial"

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

END PROCEDURE solid_AddSolidMaterial

END SUBMODULE ConstructorMethods
