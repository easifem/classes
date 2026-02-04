! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(FortranModuleFile_Class) IOMethods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL obj%moduleName%Display(msg="moduleName: ", unitNo=unitNo)
CALL obj%moduleDir%Display(msg="moduleDir: ", unitNo=unitNo)
CALL obj%moduleUsed%Display(msg="moduleUsed: ", unitNo=unitNo)
CALL obj%md%Display(msg="md: ", unitNo=unitNo)

isok = ALLOCATED(obj%userTypes)
CALL Display(isok, "userTypes ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%userTypes)
  CALL Display(tsize, "size of userTypes: ", unitNo=unitNo)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%userTypes(ii)%ptr)
    CALL Display(isok, "userTypes("//ToString(ii)//")%ptr ASSOCIATED: ", &
                 unitNo=unitNo)
    IF (isok) &
      CALL obj%userTypes(ii)%ptr%Display( &
      msg="userTypes("//ToString(ii)//")%ptr ASSOCIATED: ", &
      unitNo=unitNo)

  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
