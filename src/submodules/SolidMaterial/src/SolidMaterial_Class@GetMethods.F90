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
USE Display_Method, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   GetSolidMaterialPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSolidMaterialPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSolidMaterialPointer()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(obj)

#ifdef DEBUG_VER
isok = materialNo .LE. tsize
CALL AssertError1(isok, myName, &
     'materialNo = '//Tostring(materialNo)//' is greater than total &
     &materials = '//Tostring(tsize))
#endif

ans => NULL()
ans => obj(materialNo)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSolidMaterialPointer

!----------------------------------------------------------------------------
!                                                GetStressStrainModelPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStressStrainModelPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetStressStrainModelPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%stressStrainModel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetStressStrainModelPointer

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = myprefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                            Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
