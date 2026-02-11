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

SUBMODULE(ProcedureData_Class) IOMethods
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

CALL Display(obj%isSubroutine, msg="isSubroutine: ", unitNo=unitNo)
CALL Display(obj%isFunction, msg="isFunction: ", unitNo=unitNo)
CALL Display(obj%isModuleProcedure, msg="isModuleProcedure: ", unitNo=unitNo)
CALL Display(obj%isAbstract, msg="isAbstract: ", unitNo=unitNo)
CALL Display(obj%isGeneric, msg="isGeneric: ", unitNo=unitNo)
CALL obj%md%Display("md: ", unitNo=unitNo)
CALL obj%code%Display("code: ", unitNo=unitNo)

isok = ALLOCATED(obj%args)
CALL Display(isok, "args ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%args)
  CALL Display(tsize, "SIZE(obj%args): ", unitNo=unitNo)

  DO ii = 1, tsize
    isok = ASSOCIATED(obj%args(ii)%ptr)
    CALL Display(isok, "obj%args("//ToString(ii)//")%ptr ASSOCIATED: ", &
                 unitNo=unitNo)

    IF (isok) CALL ProcedureEntry_Display( &
      obj=obj%args(ii)%ptr, &
      msg="obj%args("//ToString(ii)//")%ptr: ", &
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
! summary: Display the content of ProcedureEntry_
!
!# Display
!
! This method displays the content of ProcedureEntry_

SUBROUTINE ProcedureEntry_Display(obj, msg, unitNo)
  CLASS(ProcedureEntry_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ProcedureEntry_Display()"
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
END SUBROUTINE ProcedureEntry_Display

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
