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
USE Display_Method, ONLY: Display
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: CHAR_SPACE
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

INTEGER(I4B) :: numLineRead0
TYPE(String) :: aline
CHARACTER(*), PARAMETER :: keyword = "USE"
CHARACTER(*), PARAMETER :: implicitNoneKeyword = "IMPLICIT NONE"
LOGICAL(LGT) :: isok, abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0

DO
  CALL obj%SearchKeywordInSourceAtStart( &
    aline=aline, keyword=keyword, lineLoc=lineLoc, &
    numLineRead=numLineRead0, isFound=isok, readSingleLine=math%yes)

  numLineRead = numLineRead + numLineRead0

  IF (isok) THEN
    CALL AppendToModuleUsed(moduleUsed=obj%moduleUsed, aline=aline)
  ELSE
    abool = aline%TRIM() .EQ. implicitNoneKeyword
    IF (abool) EXIT
  END IF

END DO

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

  threeParts = aline%Partition(sep=CHAR_SPACE)

  indx = threeParts(3)%COUNT(onlyString)
  isok = indx .EQ. 0

  IF (isok) THEN
    moduleUsed = moduleUsed//CHAR_LF//threeParts(3)%TRIM()
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  astr = threeParts(3)
  threeParts = astr%Partition(sep=onlySep)
  moduleUsed = moduleUsed//CHAR_LF//threeParts(1)%TRIM()

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
