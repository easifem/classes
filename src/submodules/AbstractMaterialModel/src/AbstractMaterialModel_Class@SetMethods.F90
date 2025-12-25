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

SUBMODULE(AbstractMaterialModel_Class) SetMethods
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           SetIsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIsInitiated
obj%isInit = VALUE
END PROCEDURE obj_SetIsInitiated

!----------------------------------------------------------------------------
!                                                                   SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetName()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%nameSize = LEN(VALUE)

#ifdef DEBUG_VER
isok = obj%nameSize .LE. TypeMaterialModelOpt%maxNameSize
CALL AssertError1(isok, myName, &
  "the size of value exceeds the maximum allowed size &
  &size of value is "//ToString(obj%nameSize)//" max allowed &
  &size is "//ToString(TypeMaterialModelOpt%maxNameSize))
#endif

obj%name(1:obj%nameSize) = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetName

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
