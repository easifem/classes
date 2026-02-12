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

SUBMODULE(ProcedureData_Class) SetMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: fileopt => TypeFileOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = __FILE__
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             SetArgPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetArgPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetArgPointer()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%args)
CALL AssertError1(isok, myName, &
                  "obj%args not allocated.")
#endif

#ifdef DEBUG_VER
tsize = SIZE(obj%args)
CALL AssertError3(indx, tsize, myName, &
                  "a=indx, b=size of args,")
#endif

#ifdef DEBUG_VER
isok = .NOT. ASSOCIATED(obj%args(indx)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%args(indx)%ptr is already associated.")
#endif

obj%args(indx)%ptr => val
obj%code = obj%code//fileopt%lf//val%name
obj%code = obj%code//fileopt%lf//val%doc

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetArgPointer

!----------------------------------------------------------------------------
!                                                            SetIsSubroutine
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIsSubroutine
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIsSubroutine()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isSubroutine = val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIsSubroutine

!----------------------------------------------------------------------------
!                                                               SetIsFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIsFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIsFunction()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isFunction = val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIsFunction

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
!                                                               SetIsGeneric
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIsGeneric
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIsGeneric()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isGeneric = val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIsGeneric

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
temp = ""

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
!                                                                    SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE ProcedureEntry_SetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ProcedureEntry_SetName()"
#endif

TYPE(String) :: temp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

temp = val%ADJUSTL()
obj%name = temp%TRIM()
temp = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ProcedureEntry_SetName

!----------------------------------------------------------------------------
!                                                                    SetDoc
!----------------------------------------------------------------------------

MODULE PROCEDURE ProcedureEntry_SetDoc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ProcedureEntry_SetDoc()"
#endif

TYPE(String) :: temp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

temp = val%ADJUSTL()
obj%doc = temp%TRIM()
temp = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ProcedureEntry_SetDoc

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
