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

SUBMODULE(FortranModuleFile_Class) ProcedureMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: fileopt => TypeFileOpt
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: CHAR_SPACE
USE InputUtility, ONLY: Input
USE MarkdownFile_Class, ONLY: MarkdownFile_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             ReadProcedures
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadProcedures
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadProcedures()"
#endif

TYPE(ProcedureData_), POINTER :: atype => NULL()
TYPE(ProcedureEntry_), POINTER :: afield => NULL()
INTEGER(I4B) :: tsize, itype, caseType, numLineRead0, tfields, ifield
TYPE(String) :: aline
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalProcedures(lineLoc=lineLoc)

numLineRead = 0

ALLOCATE (obj%procs(tsize))

DO itype = 1, tsize

  ! the following will read  the line which starts with Interface
  ! or Abstract Interface
  CALL obj%SearchProcedures( &
    aline=aline, lineLoc=lineLoc, numLineRead=numLineRead0, isFound=isok, &
    caseType=caseType)
  numLineRead = numLineRead + numLineRead0

  ! check error
  IF (.NOT. isok) EXIT

  ALLOCATE (atype)

  ! send the first line to ProcedureData for parsing
  CALL atype%ParseLine1(aline)

  ! the following will read the fortran line just after interface line
  ! This can start with
  ! MODULE SUBROUTINE
  ! MODULE FUNCTION
  ! MODULE PROCEDURE
  ! SUBROUTINE <- for ABSTRACT INTERFACE
  ! FUNCTION <- for ABSTRACT INTERFACE
  CALL obj%ReadFortranLine(aline=aline, numLineRead=numLineRead0, &
                           lineLoc=lineLoc, isFound=isok, &
                           readSingleLine=math%no)
  numLineRead = numLineRead + numLineRead0

  ! check error
  IF (.NOT. isok) EXIT

  ! send the second line to procedure for parsing
  CALL atype%ParseLine2(aline)

#ifdef DEBUG_VER
  isok = atype%GetIsModuleProcedure()
  IF (isok) THEN
    CALL AssertError1(math%no, myName, &
                      "At present we cannot parse module procedures, WIP")
  END IF
#endif

  CALL ProcedureDataSetMd(obj=obj, val=atype, lineLoc=lineLoc)

  !! make fields
  tfields = obj%GetTotalArgsInProcedure()
  CALL atype%AllocateArgs(tfields)

  DO ifield = 1, tfields
    ALLOCATE (afield)
    CALL obj%ReadArgInProcedure( &
      val=afield, lineLoc=lineLoc, numLineRead=numLineRead0, isFound=isok)

    CALL atype%SetArgPointer(indx=ifield, val=afield)
  END DO

  ! Read END SUBROUTINE or END FUNCTION line
  CALL obj%ReadFortranLine(aline=aline, numLineRead=numLineRead0, &
                           lineLoc=lineLoc, isFound=isok, &
                           readSingleLine=math%no)
  numLineRead = numLineRead + numLineRead0
  ! check error
  IF (.NOT. isok) EXIT
  ! send the line to procedure for parsing
  CALL atype%ParseLine3(aline)

  ! Read END INTERFACE
  CALL obj%ReadFortranLine(aline=aline, numLineRead=numLineRead0, &
                           lineLoc=lineLoc, isFound=isok, &
                           readSingleLine=math%no)
  numLineRead = numLineRead + numLineRead0
  ! check error
  IF (.NOT. isok) EXIT
  ! send the line to procedure for parsing
  CALL atype%ParseLine4(aline)

  obj%procs(itype)%ptr => atype

END DO

atype => NULL()
afield => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadProcedures

!----------------------------------------------------------------------------
!                                                          GetTotalProcedures
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalProcedures
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalProcedures()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
INTEGER(I4B), PARAMETER :: tkeys = 2
TYPE(String) :: aline, keywords(tkeys)
INTEGER(I4B) :: iostat, ikey, iline, lineLoc0, numLineRead0
CHARACTER(fileopt%fortranLineLen) :: fixstr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

keywords(1) = "INTERFACE"
keywords(2) = "ABSTRACT INTERFACE"

CALL obj%REWIND()
lineLoc0 = 0

ans = 0

DO
  CALL obj%ReadFortranLine(aline=aline, lineLoc=lineLoc0, &
                           numLineRead=numLineRead0, isfound=isok, &
                           readSingleLine=math%no)

  ! Exit if there is any error in the above call
  IF (.NOT. isok) EXIT

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

DO ikey = 1, tkeys
  keywords(ikey) = ""
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalProcedures

!----------------------------------------------------------------------------
!                                                    GetTotalArgsInProcedure
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalArgsInProcedure
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalArgsInProcedure()"
#endif

CHARACTER(*), PARAMETER :: commentString = "!"
INTEGER(I4B), PARAMETER :: tkeys = 7, texitkeys = 2
TYPE(String) :: aline, keywords(tkeys), exitkeywords(texitkeys)
INTEGER(I4B) :: numLineRead, ikey, lineLoc, numLineRead0, &
                iline
LOGICAL(LGT) :: isFound, keyfound, exitkeyfound

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

keywords(1) = "TYPE("
keywords(2) = "CLASS("
keywords(3) = "INTEGER("
keywords(4) = "REAL("
keywords(5) = "LOGICAL("
keywords(6) = "CHARACTER("
keywords(7) = "PROCEDURE("

exitkeywords(1) = "END SUBROUTINE"
exitkeywords(2) = "END FUNCTION"

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
    IF (exitkeyfound) EXIT loop1
  END DO loop3

END DO loop1

! here we are going to back where we came from in the fortran file
DO iline = 1, numLineRead
  CALL obj%BACKSPACE()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalArgsInProcedure

!----------------------------------------------------------------------------
!                                                         ReadArgInProcedure
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadArgInProcedure
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadArgInProcedure()"
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

CALL val%SetName(aline)

! Now we will read the docstring for the Arg
CALL obj%ReadDocCommentLine( &
  aline=aline, lineLoc=lineLoc, numLineRead=numLineRead0, &
  isFound=isFound, docString=docString)

numLineRead = numLineRead + numLineRead0

CALL val%SetDoc(aline)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ReadArgInProcedure

!----------------------------------------------------------------------------
!                                                         ProcedureDataSetMd
!----------------------------------------------------------------------------

SUBROUTINE ProcedureDataSetMd(obj, val, lineLoc)
  CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  !! Fortran module file
  TYPE(ProcedureData_), INTENT(INOUT) :: val
  !! data will be read in val%md
  INTEGER(I4B), INTENT(IN) :: lineLoc
  !! line location in file before calling this file
  !! after calling this routine line number will be set to the original

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ProcedureDataSetMd()"
#endif
  TYPE(String) :: aline
  CHARACTER(*), PARAMETER :: predocmark_alt = "!>"
  CHARACTER(fileopt%fortranLineLen) :: fixstr
  INTEGER(I4B) :: numLineRead0, lineLoc0, caseType, iline, tline, iostat
  LOGICAL(LGT) :: isFound, isok
  TYPE(MarkdownData_) :: md

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  lineLoc0 = lineLoc

  CALL obj%SearchProceduresBack( &
    aline=aline, lineLoc=lineLoc0, numLineRead=numLineRead0, &
    isFound=isFound, caseType=caseType)

  CALL obj%SkipBlankLinesBack( &
    aline=aline, lineLoc=lineLoc0, numLineRead=numLineRead0)

  CALL obj%SearchKeywordInCommentAtStartBack( &
    aline=aline, keyword=predocmark_alt, lineLoc=lineLoc0, &
    numLineRead=numLineRead0, isFound=isFound)

  IF (isFound) THEN
    CALL obj%ReadMarkdownData( &
      md=md, lineLoc=lineLoc0, numLineRead=numLineRead0)
    CALL val%SetMd(md)
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
END SUBROUTINE ProcedureDataSetMd

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ProcedureMethods
