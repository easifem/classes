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

SUBMODULE(UserTypeData_Class) SetMethods
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = __FILE__
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              SetHeaderLine
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetHeaderLine
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetHeaderLine()"
#endif

TYPE(String) :: temp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

temp = val%ADJUSTL()
obj%headerLine = temp%TRIM()
temp = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetHeaderLine

!----------------------------------------------------------------------------
!                                                             SetFieldPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFieldPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFieldPointer()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%fields)
CALL AssertError1(isok, myName, &
                  "obj%fields not allocated.")
#endif

#ifdef DEBUG_VER
tsize = SIZE(obj%fields)
CALL AssertError3(indx, tsize, myName, &
                  "a=indx, b=size of fields,")
#endif

#ifdef DEBUG_VER
isok = .NOT. ASSOCIATED(obj%fields(indx)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%fields(indx)%ptr is already associated.")
#endif

obj%fields(indx)%ptr => val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFieldPointer

!----------------------------------------------------------------------------
!                                                         SetMethodPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMethodPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMethodPointer()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%methods)
CALL AssertError1(isok, myName, &
                  "obj%methods not allocated.")
#endif

#ifdef DEBUG_VER
tsize = SIZE(obj%methods)
CALL AssertError3(indx, tsize, myName, &
                  "a=indx, b=size of methods,")
#endif

#ifdef DEBUG_VER
isok = .NOT. ASSOCIATED(obj%methods(indx)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%methods(indx)%ptr is already associated.")
#endif

obj%methods(indx)%ptr => val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMethodPointer

!----------------------------------------------------------------------------
!                                                                    SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE UserTypeEntry_SetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "UserTypeEntry_SetName()"
#endif

TYPE(String) :: temp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

temp = val%ADJUSTL()
obj%name = temp%TRIM()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE UserTypeEntry_SetName

!----------------------------------------------------------------------------
!                                                                    SetDoc
!----------------------------------------------------------------------------

MODULE PROCEDURE UserTypeEntry_SetDoc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "UserTypeEntry_SetDoc()"
#endif

TYPE(String) :: temp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

temp = val%ADJUSTL()
obj%doc = temp%TRIM()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE UserTypeEntry_SetDoc

!----------------------------------------------------------------------------
!                                                                 SetIsChild
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIsChild
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIsChild()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isChild = val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIsChild

!----------------------------------------------------------------------------
!                                                              SetIsAbstract
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIsAbstract
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIsAbstract()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isAbstract = val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIsAbstract

!----------------------------------------------------------------------------
!                                                                     SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetName()"
#endif

TYPE(String) :: temp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

temp = val%ADJUSTL()
obj%name = temp%TRIM()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetName

!----------------------------------------------------------------------------
!                                                                      SetMd
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMd
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMd()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%md%Copy(val)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMd

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
