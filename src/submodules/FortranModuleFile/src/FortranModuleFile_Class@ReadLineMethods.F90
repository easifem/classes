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

SUBMODULE(FortranModuleFile_Class) ReadLineMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: fileopt => TypeFileOpt
USE Display_Method, ONLY: Display
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: CHAR_SPACE
USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             ReadFortranLine
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadFortranLine
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadFortranLine()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!", linebreak = "&"
INTEGER(I4B) :: iostat
CHARACTER(fileopt%fortranLineLen) :: fixstr
LOGICAL(LGT) :: isok, inFortranLine
TYPE(String) :: fmtline

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0
isFound = math%no
inFortranLine = math%no
aline = ""

DO
  CALL obj%ReadLine(val=fmtline, iostat=iostat, iomsg=fixstr)
  lineLoc = lineLoc + 1
  numLineRead = numLineRead + 1

  ! Exit if end of file is reached
  isok = obj%isEOF()
  IF (isok) EXIT

  ! skip if the line is blank line
  isok = fmtline%LEN_TRIM() .EQ. 0
  IF (isok) CYCLE

  ! remove spaces from head and tail
  fixstr = fmtline%chars()
  fmtline = TRIM(ADJUSTL(fixstr))

  ! exit if line is a comment and readSingleLine is true
  isok = fmtline%start_with(prefix=commentString)
  IF (isok .AND. readSingleLine) THEN
    aline = fmtline
    EXIT
  END IF

  ! skip if line is a comment here readSingleLine is false
  IF (isok) CYCLE

  ! At this stage we know that the line is a fortran line
  ! check if the line end with the linebreak
  isok = fmtline%end_with(suffix=linebreak)

  IF (isok) THEN

    ! isok .AND. inFortranLine
    IF (inFortranLine) THEN
      aline = aline//CHAR_LF//fmtline

      ! isok .AND. (.NOT. inFortranLine)
    ELSE
      aline = fmtline
    END IF

    inFortranLine = math%yes
    isFound = math%yes

  ELSE

    IF (inFortranLine) THEN
      aline = aline//CHAR_LF//fmtline

    ELSE
      aline = fmtline

    END IF

    inFortranLine = math%no
    isFound = math%yes

    EXIT

  END IF

END DO

fmtline = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadFortranLine

!----------------------------------------------------------------------------
!                                                      obj_ReadDocCommentLine
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadDocCommentLine
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadDocCommentLine()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!", linebreak = "&"
INTEGER(I4B) :: iostat
CHARACTER(1024) :: fixstr
LOGICAL(LGT) :: isok, firstComment
TYPE(String) :: fmtline

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0
isFound = math%no
firstComment = math%yes
aline = ""

DO
  CALL obj%ReadLine(val=fmtline, iostat=iostat, iomsg=fixstr)
  lineLoc = lineLoc + 1
  numLineRead = numLineRead + 1

  ! Exit if end of file is reached
  isok = obj%isEOF()
  IF (isok) EXIT

  ! exit if the line is blank line
  isok = fmtline%LEN_TRIM() .EQ. 0
  IF (isok) EXIT

  ! Remove spaces from head and tail
  fixstr = fmtline%chars()
  fmtline = TRIM(ADJUSTL(fixstr))

  ! EXIT if line is not a doc comment and readSingleLine is true
  isok = fmtline%start_with(prefix=docString)
  IF (.NOT. isok) THEN
    CALL obj%BACKSPACE()
    EXIT
  END IF

  ! At this stage we know that the line is a docComment
  IF (firstComment) THEN
    aline = fmtline
    isFound = math%yes
    firstComment = math%no
  ELSE
    aline = aline//CHAR_LF//fmtline
  END IF

END DO

fmtline = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadDocCommentLine

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadLineMethods
