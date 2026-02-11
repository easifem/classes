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

SUBMODULE(FortranModuleFile_Class) SearchMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: fileopt => TypeFileOpt
USE Display_Method, ONLY: Display
USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                             SearchKeywordInCommentAtStart
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SearchKeywordInCommentAtStart
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SearchKeywordInCommentAtStart()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
INTEGER(I4B) :: iostat
CHARACTER(fileopt%fortranLineLen) :: fixstr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0
isFound = math%no

DO
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  lineLoc = lineLoc + 1
  numLineRead = numLineRead + 1

  ! Exit if end of file is reached
  isok = obj%isEOF()
  IF (isok) EXIT

  ! Skip if the line is blank line
  isok = aline%LEN_TRIM() .EQ. 0
  IF (isok) CYCLE

  ! skip if line is not comment
  isok = aline%start_with(prefix=commentString)
  IF (.NOT. isok) THEN
    CALL obj%BACKSPACE()
    lineLoc = lineLoc - 1
    numLineRead = numLineRead - 1
    EXIT
  END IF

  ! Skip if the line does not contains the keyword
  isFound = aline%start_with(prefix=keyword)
  IF (isFound) EXIT
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SearchKeywordInCommentAtStart

!----------------------------------------------------------------------------
!                                          SearchKeywordInCommentAtStartBack
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SearchKeywordInCommentAtStartBack
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SearchKeywordInCommentAtStartBack()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
INTEGER(I4B) :: iostat
CHARACTER(1024) :: fixstr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0
isFound = math%no

isok = lineLoc .NE. 0
IF (isok) THEN
  lineLoc = lineLoc - 1
  CALL obj%BACKSPACE()
END IF

loop1: DO
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  numLineRead = numLineRead + 1

  ! Exit if current line location is zero
  isok = lineLoc .NE. 0
  IF (.NOT. isok) THEN
    CALL obj%BACKSPACE()
    lineLoc = 0
    EXIT loop1
  END IF

  ! Exit if the line contains the keyword, we are done here
  ! Exit if the line is not a comment including a blanklines
  isFound = aline%start_with(prefix=keyword)
  isok = aline%start_with(prefix=commentString)
  IF (isFound .OR. (.NOT. isok)) THEN
    CALL obj%BACKSPACE()
    EXIT loop1
  END IF

  CALL obj%BACKSPACE()
  CALL obj%BACKSPACE()
  lineLoc = lineLoc - 1
END DO loop1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SearchKeywordInCommentAtStartBack

!----------------------------------------------------------------------------
!                                                         SkipBlankLinesBack
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SkipBlankLinesBack
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SkipBlankLinesBack()"
#endif

INTEGER(I4B) :: iostat
CHARACTER(1024) :: fixstr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0

isok = lineLoc .NE. 0
IF (isok) THEN
  lineLoc = lineLoc - 1
  CALL obj%BACKSPACE()
END IF

loop1: DO
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  numLineRead = numLineRead + 1

  ! Exit if current line location is zero
  isok = lineLoc .NE. 0
  IF (.NOT. isok) THEN
    CALL obj%BACKSPACE()
    lineLoc = 0
    EXIT loop1
  END IF

  ! move records back, if the line is blank line
  ! otherwise exit loop1 as we are done
  isok = aline%LEN_TRIM() .EQ. 0
  IF (isok) THEN
    CALL obj%BACKSPACE()
    CALL obj%BACKSPACE()
    lineLoc = lineLoc - 1
  ELSE
    CALL obj%BACKSPACE()
    EXIT loop1
  END IF
END DO loop1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SkipBlankLinesBack

!----------------------------------------------------------------------------
!                                               SearchKeywordInSourceAtStart
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SearchKeywordInSourceAtStart
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SearchKeywordInSourceAtStart()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
INTEGER(I4B) :: numLineRead0
LOGICAL(LGT) :: isok, readSingleLine0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0
isFound = math%no
readSingleLine0 = Input(option=readSingleLine, default=math%no)

DO
  CALL obj%ReadFortranLine( &
    aline=aline, lineLoc=lineLoc, numLineRead=numLineRead0, isfound=isok, &
    readSingleLine=math%no)
  numLineRead = numLineRead + numLineRead0

  ! exit if isok is false
  IF (.NOT. isok) EXIT

  ! exit if the line contains the keyword
  isFound = aline%start_with(prefix=keyword)
  IF (isFound) EXIT

  ! exit if readSingleLine
  IF (readSingleLine0) EXIT
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SearchKeywordInSourceAtStart

!----------------------------------------------------------------------------
!                                                             SearchUserTypes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SearchUserTypes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SearchUserTypes()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
TYPE(String) :: keywords(3)
INTEGER(I4B) :: ikey, tkeys, numLineRead0
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

keywords(1) = "TYPE ::"
keywords(2) = "TYPE, EXTENDS"
keywords(3) = "TYPE, ABSTRACT"
tkeys = 3

isFound = math%no
numLineRead = 0

loop1: DO
  CALL obj%ReadFortranLine( &
    aline=aline, lineLoc=lineLoc, numLineRead=numLineRead0, isfound=isok, &
    readSingleLine=math%no)
  numLineRead = numLineRead + numLineRead0

  ! Exit if isok is false
  IF (.NOT. isok) EXIT loop1

  loop2: DO ikey = 1, tkeys
    isFound = aline%start_with(prefix=keywords(ikey)%chars())
    IF (isFound) THEN
      caseType = ikey
      EXIT loop1
    END IF
  END DO loop2

END DO loop1

DO ikey = 1, tkeys
  keywords(ikey) = ""
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SearchUserTypes

!----------------------------------------------------------------------------
!                                                         SearchUserTypesBack
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SearchUserTypesBack
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SearchUserTypesBack()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
TYPE(String) :: keywords(3)
INTEGER(I4B) :: iostat, ikey, tkeys
CHARACTER(1024) :: fixstr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

keywords(1) = "TYPE ::"
keywords(2) = "TYPE, EXTENDS"
keywords(3) = "TYPE, ABSTRACT"
tkeys = 3

isFound = math%no
numLineRead = 0

isok = lineLoc .NE. 0
IF (isok) THEN
  lineLoc = lineLoc - 1
  CALL obj%BACKSPACE()
END IF

loop1: DO

  !! change it to ReadFortranLine
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  numLineRead = numLineRead + 1

  ! Exit if current line location is zero
  isok = lineLoc .NE. 0
  IF (.NOT. isok) THEN
    CALL obj%BACKSPACE()
    lineLoc = 0
    EXIT loop1
  END IF

  loop2: DO ikey = 1, tkeys
    isFound = aline%start_with(prefix=keywords(ikey)%chars())
    IF (isFound) THEN
      caseType = ikey
      CALL obj%BACKSPACE()
      EXIT loop1
    END IF
  END DO loop2

  ! If line does not contains the keywords then we go back
  ! decrease the lineLoc. it includes following cases
  ! ! Skip if the line is blank line
  ! isok = aline%LEN_TRIM() .EQ. 0
  ! ! Skip if line is comment
  ! isok = aline%start_with(prefix=commentString)
  CALL obj%BACKSPACE()
  CALL obj%BACKSPACE()
  lineLoc = lineLoc - 1
END DO loop1

DO ikey = 1, tkeys
  keywords(ikey) = ""
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SearchUserTypesBack

!----------------------------------------------------------------------------
!                                                           SearchProcedures
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SearchProcedures
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SearchProcedures()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
INTEGER(I4B), PARAMETER :: tkeys = 2
TYPE(String) :: keywords(tkeys)
INTEGER(I4B) :: ikey, numLineRead0
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

keywords(1) = "INTERFACE"
keywords(2) = "ABSTRACT INTERFACE"

isFound = math%no
numLineRead = 0

loop1: DO
  CALL obj%ReadFortranLine(aline=aline, lineLoc=lineLoc, &
                           numLineRead=numLineRead0, isfound=isok, &
                           readSingleLine=math%no)
  numLineRead = numLineRead + numLineRead0

  ! Exit if the above method returns not isok
  IF (.NOT. isok) EXIT loop1

  loop2: DO ikey = 1, tkeys
    isFound = aline%start_with(prefix=keywords(ikey)%chars())
    IF (isFound) THEN
      caseType = ikey
      EXIT loop1
    END IF
  END DO loop2

END DO loop1

DO ikey = 1, tkeys
  keywords(ikey) = ""
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SearchProcedures

!----------------------------------------------------------------------------
!                                                        SearchProceduresBack
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SearchProceduresBack
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SearchProceduresBack()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
INTEGER(I4B), PARAMETER :: tkeys = 2
TYPE(String) :: keywords(tkeys)
INTEGER(I4B) :: iostat, ikey
CHARACTER(fileopt%fortranLineLen) :: fixstr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

keywords(1) = "INTERFACE"
keywords(2) = "ABSTRACT INTERFACE"

isFound = math%no
numLineRead = 0

isok = lineLoc .NE. 0
IF (isok) THEN
  lineLoc = lineLoc - 1
  CALL obj%BACKSPACE()
END IF

loop1: DO

  !! change it to ReadFortranLine
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  numLineRead = numLineRead + 1

  ! Exit if current line location is zero
  isok = lineLoc .NE. 0
  IF (.NOT. isok) THEN
    CALL obj%BACKSPACE()
    lineLoc = 0
    EXIT loop1
  END IF

  loop2: DO ikey = 1, tkeys
    isFound = aline%start_with(prefix=keywords(ikey)%chars())
    IF (isFound) THEN
      caseType = ikey
      CALL obj%BACKSPACE()
      EXIT loop1
    END IF
  END DO loop2

  ! If line does not contains the keywords then we go back
  ! decrease the lineLoc. it includes following cases
  ! ! Skip if the line is blank line
  ! isok = aline%LEN_TRIM() .EQ. 0
  ! ! Skip if line is comment
  ! isok = aline%start_with(prefix=commentString)
  CALL obj%BACKSPACE()
  CALL obj%BACKSPACE()
  lineLoc = lineLoc - 1
END DO loop1

DO ikey = 1, tkeys
  keywords(ikey) = ""
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SearchProceduresBack

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SearchMethods
