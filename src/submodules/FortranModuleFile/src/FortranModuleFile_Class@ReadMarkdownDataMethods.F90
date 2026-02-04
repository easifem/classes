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

SUBMODULE(FortranModuleFile_Class) ReadMarkdownDataMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: CHAR_SPACE
USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                            ReadPreComments
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadMarkdownData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadMarkdownData()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!", &
                           predocmark_alt = "!>"

LOGICAL(LGT) :: isok
INTEGER(I4B) :: iostat, linelen, numLineRead0
CHARACTER(1024) :: fixstr
TYPE(String) :: aline, frontmatter, content

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0

! find predocmark_alt
CALL obj%SearchKeywordInCommentAtStart( &
  aline=aline, keyword=predocmark_alt, &
  lineLoc=lineLoc, numLineRead=numLineRead0, isfound=isok)

numLineRead = numLineRead + numLineRead0

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! the following code is executed when predocmark_alt is found
! read the front matter
DO
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  lineLoc = lineLoc + 1
  numLineRead = numLineRead + 1

  ! Exit if end of file is reached
  isok = obj%isEOF()
  IF (isok) EXIT

  ! exit if line is not a comment
  isok = aline%start_with(prefix=commentString)
  IF (.NOT. isok) EXIT

  ! exit if we get an empty comment or a blank line
  isok = aline%LEN_TRIM() .GT. math%one_i
  IF (.NOT. isok) EXIT

  ! TODO: before we append the aline to frontmatter, we should
  !       senetize it, because yaml is indent based lang
  !       here we are just reading from third col, because
  !       the first column contains !, second contains a space
  ! append data to frontmatter after removing the comment
  linelen = aline%LEN_TRIM()
  fixstr(1:linelen - 2) = aline%slice(3, linelen)
  linelen = linelen - 1
  fixstr(linelen:linelen) = CHAR_LF
  frontmatter = frontmatter//TRIM(fixstr(1:linelen))
END DO

! read the content
content = ""
DO
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  lineLoc = lineLoc + 1
  numLineRead = numLineRead + 1

  ! Exit if end of file is reached
  isok = obj%isEOF()
  IF (isok) EXIT

  ! exit if line is not a comment
  isok = aline%start_with(prefix=commentString)
  IF (.NOT. isok) EXIT

  ! append data to content after removing the comment
  ! here we are not sentizing the docs
  linelen = aline%LEN_TRIM()
  fixstr(1:linelen - 1) = aline%slice(2, linelen)
  fixstr(linelen:linelen) = CHAR_LF
  content = content//TRIM(fixstr(1:linelen))
END DO

CALL md%Initiate(frontmatter=frontmatter, content=content)

aline = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadMarkdownData

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadMarkdownDataMethods
