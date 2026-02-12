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

SUBMODULE(UserTypeData_Class) IOMethods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = __FILE__
#endif

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

CALL Display(msg, "msg: ", unitNo=unitNo)
CALL obj%name%Display(msg="name: ", unitNo=unitNo)

CALL Display(obj%isChild, msg="isChild: ", unitNo=unitNo)
CALL Display(obj%isAbstract, msg="isAbstract: ", unitNo=unitNo)

isok = ALLOCATED(obj%fields)
CALL Display(isok, "fields ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%fields)
  CALL Display(tsize, "SIZE(obj%fields): ", unitNo=unitNo)

  DO ii = 1, tsize
    isok = ASSOCIATED(obj%fields(ii)%ptr)
    CALL Display(isok, "obj%fields("//ToString(ii)//")%ptr ASSOCIATED: ", &
                 unitNo=unitNo)

    IF (isok) CALL UserTypeEntry_Display( &
      obj=obj%fields(ii)%ptr, &
      msg="obj%fields("//ToString(ii)//")%ptr: ", &
      unitNo=unitNo)

  END DO
END IF

isok = ALLOCATED(obj%methods)
CALL Display(isok, "methods ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%methods)
  CALL Display(tsize, "SIZE(obj%methods): ", unitNo=unitNo)

  DO ii = 1, tsize
    isok = ASSOCIATED(obj%methods(ii)%ptr)
    CALL Display(isok, "obj%methods("//ToString(ii)//")%ptr ASSOCIATED: ", &
                 unitNo=unitNo)

    IF (isok) CALL UserTypeEntry_Display( &
      obj=obj%methods(ii)%ptr, &
      msg="obj%methods("//ToString(ii)//")%ptr: ", &
      unitNo=unitNo)

  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Display the content of UserTypeEntry_
!
!# Display
!
! This method displays the content of UserTypeEntry_

SUBROUTINE UserTypeEntry_Display(obj, msg, unitNo)
  CLASS(UserTypeEntry_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "UserTypeEntry_Display()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Display(msg, "msg: ", unitNo=unitNo)
  CALL obj%name%Display(msg="name: ", unitNo=unitNo)
  CALL obj%doc%Display(msg="doc: ", unitNo=unitNo)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE UserTypeEntry_Display

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
