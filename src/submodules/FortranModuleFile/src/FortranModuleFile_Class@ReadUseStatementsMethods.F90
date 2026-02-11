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

SUBMODULE(FortranModuleFile_Class) ReadUseStatementsMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: fileopt => TypeFileOpt
USE Display_Method, ONLY: Display
USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                            ReadUseStatement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadUseStatements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadUseStatements()"
#endif

INTEGER(I4B), PARAMETER :: tkeys = 8
CHARACTER(*), PARAMETER :: keyword = "USE"
INTEGER(I4B) :: numLineRead0, ikey
TYPE(String) :: aline
LOGICAL(LGT) :: isok, abool
TYPE(String) :: exitkeywords(tkeys)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

exitkeywords(1) = "IMPLICIT"
exitkeywords(2) = "PRIVATE"
exitkeywords(3) = "PUBLIC"
exitkeywords(4) = "TYPE"
exitkeywords(5) = "CLASS"
exitkeywords(6) = "INTEGER"
exitkeywords(7) = "REAL"
exitkeywords(8) = "LOGICAL"

numLineRead = 0

loop1: DO
  CALL obj%SearchKeywordInSourceAtStart( &
    aline=aline, keyword=keyword, lineLoc=lineLoc, &
    numLineRead=numLineRead0, isFound=isok, readSingleLine=math%yes)

  numLineRead = numLineRead + numLineRead0

  IF (.NOT. isok) THEN
    loop2: DO ikey = 1, tkeys
      abool = aline%start_with(exitkeywords(ikey)%Chars())
      IF (abool) EXIT loop1
    END DO loop2
  END IF

  IF (isok) &
    CALL AppendToModuleUsed(moduleUsed=obj%moduleUsed, aline=aline)

END DO loop1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadUseStatements

!----------------------------------------------------------------------------
!                                                         AppendToModuleUsed
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This method parse the use statement line and append to moduleUsed

SUBROUTINE AppendToModuleUsed(moduleUsed, aline)
  TYPE(String), INTENT(INOUT) :: moduleUsed
  TYPE(String), INTENT(IN) :: aline

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "AppendToModuleUsed()"
#endif
  CHARACTER(*), PARAMETER :: onlyString = "ONLY", &
                             onlySep = ", ONLY:"
  TYPE(String) :: astr, threeParts(3)
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: indx

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  threeParts = aline%Partition(sep=fileopt%space)

  indx = threeParts(3)%COUNT(onlyString)
  isok = indx .EQ. 0

  IF (isok) THEN
    moduleUsed = moduleUsed//fileopt%lf//threeParts(3)%TRIM()
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  astr = threeParts(3)
  threeParts = astr%Partition(sep=onlySep)
  moduleUsed = moduleUsed//fileopt%lf//threeParts(1)%TRIM()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE AppendToModuleUsed

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadUseStatementsMethods
