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

SUBMODULE(UserTypeData_Class) ConstructorMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: fileopt => TypeFileOpt

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = __FILE__
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              AllocateFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateFields
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateFields()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(obj%fields)
CALL AssertError1(isok, myName, &
                  "obj%fields is already allocated")
#endif

ALLOCATE (obj%fields(tsize))

DO ii = 1, tsize
  obj%fields(ii)%ptr => NULL()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateFields

!----------------------------------------------------------------------------
!                                                            AllocateMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateMethods
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateMethods()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(obj%methods)
CALL AssertError1(isok, myName, &
                  "obj%methods is already allocated")
#endif

ALLOCATE (obj%methods(tsize))

DO ii = 1, tsize
  obj%methods(ii)%ptr => NULL()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateMethods

!----------------------------------------------------------------------------
!                                                                 ParseLine1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ParseLine1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ParseLine1()"
#endif

CHARACTER(*), PARAMETER :: abstractStr = "ABSTRACT"
CHARACTER(*), PARAMETER :: typeStr = "TYPE"
CHARACTER(*), PARAMETER :: extendsStr = "EXTENDS"
CHARACTER(*), PARAMETER :: sep = "::"
TYPE(String) :: threeParts(4)
INTEGER(I4B) :: indx
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%code = aline

indx = aline%INDEX(typeStr)
#ifdef DEBUG_VER
isok = indx .NE. math%zero_i
CALL AssertError1(isok, myName, &
                  "TYPE not found in the user type header line")
#endif

indx = aline%INDEX(extendsStr)
obj%isChild = indx .NE. math%zero_i

indx = aline%INDEX(abstractStr)
obj%isAbstract = indx .NE. math%zero_i

threeParts(1:3) = aline%partition(sep=sep)

CALL obj%SetName(threeParts(3))

DO indx = 1, 4
  threeParts(indx) = ""
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ParseLine1

!----------------------------------------------------------------------------
!                                                                 ParseLine2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ParseLine2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ParseLine2()"
#endif

CHARACTER(*), PARAMETER :: endtypeStr = "END TYPE"
TYPE(String) :: templine
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%code = obj%code//fileopt%lf//aline

templine = aline%ADJUSTL()
templine = templine%TRIM()
isok = templine%start_with(prefix=endtypeStr)

#ifdef DEBUG_VER
CALL AssertError1(isok, myName, &
                  "line does start with END TYPE")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ParseLine2

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
