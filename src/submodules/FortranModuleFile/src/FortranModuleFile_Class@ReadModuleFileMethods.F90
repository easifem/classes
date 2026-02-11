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

SUBMODULE(FortranModuleFile_Class) ReadModuleFileMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: CHAR_SPACE
USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                            ReadModuleFile
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadModuleFile
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadModuleFile()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: lineLoc, numLineRead

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsOpen()
CALL AssertError1(isok, myName, &
                  "file is not open")
#endif

#ifdef DEBUG_VER
isok = obj%IsRead()
CALL AssertError1(isok, myName, &
                  "file does not have read access")
#endif

lineLoc = 0

CALL obj%ReadMarkdownData( &
  md=obj%md, lineLoc=lineLoc, numLineRead=numLineRead)

CALL obj%ReadModuleName(lineLoc=lineLoc, numLineRead=numLineRead, &
                        isFound=isok)
#ifdef DEBUG_VER
CALL AssertError1(isok, myName, &
                  "module name not found")
#endif

CALL obj%ReadModuleDir()

CALL obj%ReadUseStatements(lineLoc=lineLoc, numLineRead=numLineRead)

CALL obj%ReadUserTypes(lineLoc=lineLoc, numLineRead=numLineRead)

CALL obj%ReadProcedures(lineLoc=lineLoc, numLineRead=numLineRead)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  '[WIP] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadModuleFile

!----------------------------------------------------------------------------
!                                                              ReadModuleName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadModuleName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadModuleName()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
CHARACTER(*), PARAMETER :: keyword = "MODULE"

TYPE(String) :: aline, threeParts(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0

CALL obj%SearchKeywordInSourceAtStart( &
  aline=aline, keyword=keyword, lineLoc=lineLoc, &
  numLineRead=numLineRead, isFound=isFound)

! The following code is executed when the line starts with the keyword
IF (isFound) THEN
  ! split the line to get the keyword and the value of keyword
  threeParts = aline%Partition(sep=CHAR_SPACE)

#ifdef DEBUG_VER
  isFound = threeParts(3)%LEN_TRIM() .NE. math%zero_i
  CALL AssertError1(isFound, myName, &
                    "MODULE keyword found but module name not found")
#endif

  obj%moduleName = threeParts(3)%TRIM()
END IF

threeParts(1) = ""
threeParts(2) = ""
threeParts(3) = ""

aline = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadModuleName

!----------------------------------------------------------------------------
!                                                            ReadModuleDir
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadModuleDir
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadModuleDir()"
#endif

INTEGER(I4B) :: linelen
LOGICAL(LGT) :: isok
TYPE(String) :: threeParts(3)
CHARACTER(*), PARAMETER :: underscore = "_"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

linelen = obj%moduleName%LEN_TRIM()

isok = linelen .EQ. 0
IF (isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

threeParts = obj%moduleName%partition(sep=underscore)

#ifdef DEBUG_VER
linelen = threeParts(1)%LEN_TRIM()
isok = linelen .NE. 0
CALL AssertError1(isok, myName, &
                  "error in getting moduleDir from moduleName.")
#endif

obj%moduleDir = threeParts(1)%Chars()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadModuleDir

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadModuleFileMethods
