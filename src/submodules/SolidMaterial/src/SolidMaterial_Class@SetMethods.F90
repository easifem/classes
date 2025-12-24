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
USE Display_Method, ONLY: ToString, Display
USE MaterialFactory, ONLY: SolidMaterialFactory
! USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    obj_AddSolidMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSolidMaterial
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddSolidMaterial"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize1
#endif

LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = materialNo .LE. tMaterials
CALL AssertError1(isok, myName, &
     'Given MaterialNo [='//ToString(materialNo)//'] is greater than &
     &total number of solidMaterials [='//ToString(tMaterials)//']!')
#endif

abool = PRESENT(region) .AND. PRESENT(solidMaterialToMesh)

#ifdef DEBUG_VER
isok = .TRUE.
tsize1 = 0
IF (abool) THEN
  tsize1 = SIZE(solidMaterialToMesh)
  isok = materialNo .LE. tsize1
END IF
CALL AssertError1(isok, myName, &
     'Given MaterialNo [='//TOSTRING(materialNo)//'] is greater than the &
      &size of solidMaterialToMesh [='//ToString(tsize1)//']!')
#endif

IF (abool) solidMaterialToMesh(materialNo) = region

abool = PRESENT(materialName)

#ifdef DEBUG_VER
isok = .TRUE.
tsize1 = 0
IF (abool) THEN
  tsize1 = SIZE(obj)
  isok = materialNo .LE. tsize1
END IF
CALL AssertError1(isok, myName, &
     'Given MaterialNo [='//ToString(materialNo)//'] is greater than the &
     &size of solidMaterial[='//ToString(tsize1)//']!')

isok = .NOT. ASSOCIATED(obj(materialNo)%ptr)
CALL AssertError1(isok, myName, &
       'solidMaterial('//ToString(materialNo)//')%ptr is already associated.')
#endif

IF (abool) THEN
  obj(materialNo)%ptr => SolidMaterialFactory(TRIM(materialName))
  !! Info: Solid material factory is defined in MaterialFactory.
END IF

END PROCEDURE obj_AddSolidMaterial

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
