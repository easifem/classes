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

SUBMODULE(FortranModuleFile_Class) ReadUserTypesMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: CHAR_SPACE
USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                            ReadUserTypes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadUserTypes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadUserTypes()"
#endif

TYPE(UserTypeData_), POINTER :: atype => NULL()
TYPE(UserTypeEntry_), POINTER :: afield => NULL()
INTEGER(I4B) :: tsize, itype, caseType, numLineRead0, tfields, ifield
TYPE(String) :: aline
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalUserTypes(lineLoc=lineLoc)

numLineRead = 0

ALLOCATE (obj%userTypes(tsize))

DO itype = 1, tsize

  CALL obj%SearchUserTypes( &
    aline=aline, lineLoc=lineLoc, numLineRead=numLineRead0, isFound=isok, &
    caseType=caseType)

  numLineRead = numLineRead + numLineRead0

  IF (.NOT. isok) EXIT

  ALLOCATE (atype)

  CALL UserTypeDataSetIsAbstract(atype, caseType)
  CALL UserTypeDataSetIsChild(atype, caseType)
  CALL UserTypeDataSetName(obj=atype, aline=aline)
  CALL UserTypeDataSetMd(obj=obj, val=atype, lineLoc=lineLoc)

  tfields = obj%GetTotalFieldsInUserType()
  ALLOCATE (atype%fields(tfields))

  DO ifield = 1, tfields
    ALLOCATE (afield)
    CALL obj%ReadFieldInUserType( &
      val=afield, lineLoc=lineLoc, numLineRead=numLineRead0, isFound=isok)

    atype%fields(ifield)%ptr => afield
  END DO

  obj%userTypes(itype)%ptr => atype

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadUserTypes

!----------------------------------------------------------------------------
!                                                    GetTotalFieldsInUserType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFieldsInUserType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalFieldsInUserType()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
TYPE(String) :: aline, keywords(7), exitkeywords(2)
INTEGER(I4B) :: numLineRead, ikey, tkeys, lineLoc, numLineRead0, texitkeys, &
                iline
LOGICAL(LGT) :: isFound, keyfound, exitkeyfound

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tkeys = 0
keywords(1) = "TYPE("; tkeys = tkeys + 1
keywords(2) = "CLASS("; tkeys = tkeys + 1
keywords(3) = "INTEGER("; tkeys = tkeys + 1
keywords(4) = "REAL("; tkeys = tkeys + 1
keywords(5) = "LOGICAL("; tkeys = tkeys + 1
keywords(6) = "CHARACTER("; tkeys = tkeys + 1
keywords(7) = "PROCEDURE("; tkeys = tkeys + 1

texitkeys = 0
exitkeywords(1) = "CONTAINS"; texitkeys = texitkeys + 1
exitkeywords(2) = "END TYPE"; texitkeys = texitkeys + 1

numLineRead = 0

ans = 0

loop1: DO

  CALL obj%ReadFortranLine( &
    aline=aline, lineLoc=lineLoc, numLineRead=numLineRead0, &
    isFound=isFound, readSingleLine=math%no)

  numLineRead = numLineRead + numLineRead0

  ! exit if not found, if isFound is false, with above option,
  ! it means that there is fortran source code found
  IF (.NOT. isFound) EXIT loop1

  ! Check the keywords
  loop2: DO ikey = 1, tkeys
    keyfound = aline%start_with(prefix=keywords(ikey)%chars())
    IF (keyfound) THEN
      ans = ans + 1
      EXIT loop2
    END IF

  END DO loop2

  IF (keyfound) CYCLE loop1

  ! Check exit keywords
  loop3: DO ikey = 1, texitkeys
    exitkeyfound = aline%start_with(prefix=exitkeywords(ikey)%chars())
    IF (exitkeyfound) THEN
      EXIT loop3
    END IF
  END DO loop3

  IF (exitkeyfound) EXIT loop1

END DO loop1

! here we are going to back where we came from in the fortran file
DO iline = 1, numLineRead
  CALL obj%BACKSPACE()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalFieldsInUserType

!----------------------------------------------------------------------------
!                                                          GetTotalUserTypes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalUserTypes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalUserTypes()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
TYPE(String) :: aline, keywords(3)
INTEGER(I4B) :: iostat, ikey, tkeys, iline
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

CALL obj%REWIND()

ans = 0

DO
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  !! check the error here

  ! Exit if end of file is reached
  isok = obj%isEOF()
  IF (isok) EXIT

  ! Skip if the line is blank line
  isok = aline%LEN_TRIM() .EQ. 0
  IF (isok) CYCLE

  ! Skip if line is comment
  isok = aline%start_with(prefix=commentString)
  IF (isok) CYCLE

  DO ikey = 1, tkeys
    isok = aline%start_with(prefix=keywords(ikey)%chars())
    IF (isok) ans = ans + 1
  END DO

END DO

CALL obj%REWIND()
DO iline = 1, lineLoc
  CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
  !! check the error here
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalUserTypes

!----------------------------------------------------------------------------
!                                                       ReadFieldInUserType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadFieldInUserType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadFieldInUserType()"
#endif

TYPE(String) :: aline
INTEGER(I4B) :: numLineRead0
CHARACTER(*), PARAMETER :: docString = "!!"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

numLineRead = 0

CALL obj%ReadFortranLine( &
  aline=aline, lineLoc=lineLoc, numLineRead=numLineRead0, &
  isFound=isFound, readSingleLine=math%no)

numLineRead = numLineRead + numLineRead0

! exit if not found, if isFound is false, with above option,
! it means that there is fortran source code found
IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

val%name = aline

! Now we will read the docstring for the field
CALL obj%ReadDocCommentLine( &
  aline=aline, lineLoc=lineLoc, numLineRead=numLineRead0, &
  isFound=isFound, docString=docString)

numLineRead = numLineRead + numLineRead0

val%doc = aline

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ReadFieldInUserType

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE UserTypeData_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "UserTypeData_Display()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, "msg: ", unitNo=unitNo)
CALL obj%name%Display(msg="name: ", unitNo=unitNo)

CALL Display(obj%isChild, msg="isChild: ", unitNo=unitNo)
CALL Display(obj%isAbstract, msg="isAbstract: ", unitNo=unitNo)

isok = ALLOCATED(obj%fields)
CALL Display(isok, "fields ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%fields)
  CALL Display(tsize, "SIZE(obj%fields): ", unitNo=unitNo)

  DO ii = 1, tsize
    isok = ASSOCIATED(obj%fields(ii)%ptr)
    CALL Display(isok, "obj%fields("//ToString(ii)//")%ptr ASSOCIATED: ", &
                 unitNo=unitNo)

    IF (isok) CALL obj%fields(ii)%ptr%Display( &
      msg="obj%fields("//ToString(ii)//")%ptr: ", &
      unitNo=unitNo)

  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE UserTypeData_Display

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE UserTypeEntry_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "UserTypeEntry_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, "msg: ", unitNo=unitNo)
CALL obj%name%Display(msg="name: ", unitNo=unitNo)
CALL obj%doc%Display(msg="doc: ", unitNo=unitNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE UserTypeEntry_Display

!----------------------------------------------------------------------------
!                                                         UserTypeSetIsChild
!----------------------------------------------------------------------------

SUBROUTINE UserTypeDataSetIsChild(obj, caseType)
  TYPE(UserTypeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: caseType

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "UserTypeDataSetIsChild()"
#endif
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%isChild = math%no
  isok = caseType .EQ. math%two_i
  IF (isok) obj%isChild = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE UserTypeDataSetIsChild

!----------------------------------------------------------------------------
!                                                  UserTypeDataSetIsAbstract
!----------------------------------------------------------------------------

SUBROUTINE UserTypeDataSetIsAbstract(obj, caseType)
  TYPE(UserTypeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: caseType

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "UserTypeDataSetIsAbstract()"
#endif
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%isAbstract = math%no

  isok = caseType .EQ. math%three_i
  IF (isok) obj%isAbstract = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE UserTypeDataSetIsAbstract

!----------------------------------------------------------------------------
!                                                       UserTypeDataSetName
!----------------------------------------------------------------------------

SUBROUTINE UserTypeDataSetName(obj, aline)
  TYPE(UserTypeData_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: aline

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "UserTypeDataSetName()"
#endif
  TYPE(String) :: threeParts(3)
  CHARACTER(*), PARAMETER :: sep = "::"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  threeParts = aline%Partition(sep=sep)
  obj%name = threeParts(3)%TRIM()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE UserTypeDataSetName

!----------------------------------------------------------------------------
!                                                         ReadUserTypePredoc
!----------------------------------------------------------------------------

SUBROUTINE UserTypeDataSetMd(obj, val, lineLoc)
  CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  !! Fortran module file
  TYPE(UserTypeData_), INTENT(INOUT) :: val
  !! data will be read in val%md
  INTEGER(I4B), INTENT(IN) :: lineLoc
  !! line location in file before calling this file
  !! after calling this routine line number will be set to the original

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "UserTypeDataSetMd()"
#endif
  TYPE(String) :: aline
  CHARACTER(*), PARAMETER :: predocmark_alt = "!>"
  CHARACTER(1024) :: fixstr
  INTEGER(I4B) :: numLineRead0, lineLoc0, caseType, iline, tline, iostat
  LOGICAL(LGT) :: isFound, isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  lineLoc0 = lineLoc

  CALL obj%SearchUserTypesBack( &
    aline=aline, lineLoc=lineLoc0, numLineRead=numLineRead0, &
    isFound=isFound, caseType=caseType)

  CALL obj%SkipBlankLinesBack( &
    aline=aline, lineLoc=lineLoc0, numLineRead=numLineRead0)

  CALL obj%SearchKeywordInCommentAtStartBack( &
    aline=aline, keyword=predocmark_alt, lineLoc=lineLoc0, &
    numLineRead=numLineRead0, isFound=isFound)

  IF (isFound) THEN
    CALL obj%ReadMarkdownData( &
      md=val%md, lineLoc=lineLoc0, numLineRead=numLineRead0)
  END IF

  ! now we need to go back where we came from
  isok = lineLoc0 .LT. lineLoc
  IF (isok) THEN
    tline = lineLoc - lineLoc0
    DO iline = 1, tline
      lineLoc0 = lineLoc0 + 1
      CALL obj%ReadLine(val=aline, iostat=iostat, iomsg=fixstr)
    END DO
  ELSE
    tline = lineLoc0 - lineLoc
    DO iline = 1, tline
      lineLoc0 = lineLoc0 - 1
      CALL obj%BACKSPACE()
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE UserTypeDataSetMd

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadUserTypesMethods
