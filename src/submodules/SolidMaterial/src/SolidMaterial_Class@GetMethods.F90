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

SUBMODULE(SolidMaterial_Class) GetMethods
USE BaseMethod, ONLY: Tostring
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   GetSolidMaterialPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSolidMaterialPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetSolidMaterialPointer"
LOGICAL(LGT) :: problem
INTEGER(I4B) :: tsize
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

tsize = SIZE(obj)
problem = materialNo .GT. tsize
ans => NULL()
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: materialNo = '//Tostring(materialNo)//  &
    & ' is greater than total materials = '//Tostring(tsize))
  RETURN
END IF
ans => obj(materialNo)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_GetSolidMaterialPointer

!----------------------------------------------------------------------------
!                                                GetStressStrainModelPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStressStrainModelPointer
ans => obj%stressStrainModel
END PROCEDURE obj_GetStressStrainModelPointer

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

END SUBMODULE GetMethods
