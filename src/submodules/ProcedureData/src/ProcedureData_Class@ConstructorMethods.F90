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

SUBMODULE(ProcedureData_Class) ConstructorMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: fileopt => TypeFileOpt
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "ProcedureData_Class@ConstructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             AllocateFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateArgs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateArgs()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(obj%args)
CALL AssertError1(isok, myName, &
                  "obj%args is already allocated")
#endif

ALLOCATE (obj%args(tsize))

DO ii = 1, tsize
  obj%args(ii)%ptr => NULL()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateArgs

!----------------------------------------------------------------------------
!                                                                 ParseLine1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ParseLine1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ParseLine1()"
#endif

TYPE(String) :: threeParts(4)
INTEGER(I4B) :: linelen
CHARACTER(*), PARAMETER :: abstractStr = "ABSTRACT"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%code = aline
threeParts(1) = aline%ADJUSTL()
threeParts(4) = threeParts(1)%TRIM()

obj%isAbstract = threeParts(4)%start_with(prefix=abstractStr)

IF (obj%isAbstract) obj%isGeneric = math%no

threeParts(1:3) = threeParts(4)%partition(sep=fileopt%space)

linelen = threeParts(3)%LEN_TRIM()
obj%isGeneric = linelen .NE. math%zero_i

IF (obj%isGeneric) THEN
  threeParts(1) = threeParts(3)%ADJUSTL()
  obj%genericName = threeParts(1)%TRIM()
END IF

threeParts(1) = ""
threeParts(2) = ""
threeParts(3) = ""
threeParts(4) = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ParseLine1

!----------------------------------------------------------------------------
!                                                             obj_ParseLine2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ParseLine2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ParseLine2()"
#endif

CHARACTER(*), PARAMETER :: moduleStr = "MODULE"
CHARACTER(*), PARAMETER :: procedureStr = "PROCEDURE"
CHARACTER(*), PARAMETER :: subroutineStr = "SUBROUTINE"
CHARACTER(*), PARAMETER :: functionStr = "FUNCTION"
TYPE(String) :: tempstr, tempstr2
INTEGER(I4B) :: subroutineIndx, functionIndx, procedureIndx, a, b
LOGICAL(LGT) :: isModule

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%code = obj%code//fileopt%lf//aline
tempstr2 = aline%ADJUSTL()
tempstr = tempstr2%TRIM()

subroutineIndx = tempstr%INDEX(subroutineStr)
obj%isSubroutine = subroutineIndx .NE. math%zero_i

functionIndx = tempstr%INDEX(functionStr)
obj%isFunction = functionIndx .NE. math%zero_i

isModule = tempstr%start_with(moduleStr)
#ifdef DEBUG_VER
IF (.NOT. isModule) THEN
  CALL AssertError1(obj%isAbstract, myName, &
       "Interface seems to be an Abstract Interface,&
       &but isAbstract is false")
END IF
#endif

procedureIndx = tempstr%INDEX(procedureStr)
obj%isModuleProcedure = (procedureIndx .NE. math%zero_i) .AND. isModule

#ifdef DEBUG_VER
IF (.NOT. isModule) THEN
  CALL AssertError1(obj%isAbstract, myName, &
       "Interface seems to be an Abstract Interface,&
       &but isAbstract is false")
END IF
#endif

IF (obj%isFunction) THEN
  a = functionIndx + LEN(functionStr)
  b = tempstr%INDEX("(")
  tempstr2 = tempstr%slice(a, b - 1)
  CALL obj%SetName(tempstr2)
END IF

IF (obj%isSubroutine) THEN
  a = subroutineIndx + LEN(subroutineStr)
  b = tempstr%INDEX("(")
  tempstr2 = tempstr%slice(a, b - 1)
  CALL obj%SetName(tempstr2)
END IF

IF (obj%isModuleProcedure) THEN
  a = procedureIndx + LEN(procedureStr)
  b = tempstr%LEN_TRIM()
  tempstr2 = tempstr%slice(a, b)
  CALL obj%SetName(tempstr2)
END IF

tempstr = ""
tempstr2 = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ParseLine2

!----------------------------------------------------------------------------
!                                                                 ParseLine3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ParseLine3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ParseLine3()"
#endif

CHARACTER(*), PARAMETER :: subroutineStr = "SUBROUTINE"
CHARACTER(*), PARAMETER :: functionStr = "FUNCTION"
CHARACTER(*), PARAMETER :: endStr = "END"
LOGICAL(LGT) :: isok
TYPE(String) :: temp
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%code = obj%code//fileopt%lf//aline

temp = aline%ADJUSTL()
temp = temp%TRIM()

#ifdef DEBUG_VER
isok = temp%start_with(endStr)
CALL AssertError1(isok, myName, &
                  "line does not start with END")
#endif

#ifdef DEBUG_VER
IF (obj%isSubroutine) THEN
  indx = temp%INDEX(subroutineStr)
  isok = indx .NE. math%zero_i
  CALL AssertError1(isok, myName, &
                    "END SUBROUTINE not found.")
END IF
#endif

#ifdef DEBUG_VER
IF (obj%isFunction) THEN
  indx = temp%INDEX(functionStr)
  isok = indx .NE. math%zero_i
  CALL AssertError1(isok, myName, &
                    "END FUNCTION not found.")
END IF
#endif

temp = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ParseLine3

!----------------------------------------------------------------------------
!                                                                  ParseLine
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ParseLine4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ParseLine4()"
#endif

CHARACTER(*), PARAMETER :: endInterfaceStr = "END INTERFACE"
LOGICAL(LGT) :: isok
TYPE(String) :: temp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%code = obj%code//fileopt%lf//aline

temp = aline%ADJUSTL()
temp = temp%TRIM()

#ifdef DEBUG_VER
isok = temp%start_with(endInterfaceStr)
CALL AssertError1(isok, myName, &
                  "line does not start with END INTERFACE")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ParseLine4

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
