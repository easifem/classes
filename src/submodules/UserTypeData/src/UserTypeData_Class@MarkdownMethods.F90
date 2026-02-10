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

SUBMODULE(UserTypeData_Class) MarkdownMethods
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

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = __FILE__
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                        GenerateMarkdownDocs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GenerateMarkdownDocs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GenerateMarkdownDocs()"
#endif

TYPE(MarkdownFile_) :: md
TYPE(UserTypeEntry_), POINTER :: field
TYPE(String) :: filename, aline
INTEGER(I4B) :: iostat, linelen, tsize, ii
CHARACTER(fileopt%maxStrLen) :: iomsg
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Create an index_.md using md, check md%content, senetize it
aline = obj%name%ADJUSTL()
filename = aline%TRIM()//".md"
CALL md%Initiate( &
  filename=filename%Chars(), status=fileopt%replace, action=fileopt%WRITE)

CALL md%OPEN()

aline = obj%md%GetFrontmatter()
CALL md%WriteFrontmatter(val=aline)

aline = obj%md%GetContent()
linelen = aline%LEN_TRIM()
isok = linelen .NE. 0

IF (isok) THEN
  CALL md%WRITE(val=fileopt%space, iostat=iostat, iomsg=iomsg)
  CALL md%WRITE(val=aline, iostat=iostat, iomsg=iomsg)

ELSE
  linelen = obj%name%LEN_TRIM()
  isok = linelen .NE. 0

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, "name is empty")
#endif

  CALL md%WriteH1(val=obj%name)
END IF

! ## Subheading for fields
isok = ALLOCATED(obj%fields)
IF (isok) THEN
  aline = "Fields"
  CALL md%WriteH2(val=aline)

  CALL md%StartCodeFence(lang="fortran")
  CALL md%WRITE(val=obj%headerLine, iostat=iostat, iomsg=iomsg)

  tsize = SIZE(obj%fields)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%fields(ii)%ptr)
    IF (.NOT. isok) CYCLE

    field => obj%fields(ii)%ptr

    CALL md%WRITE(val=field%name, iostat=iostat, iomsg=iomsg)
    CALL md%WRITE(val=field%doc, iostat=iostat, iomsg=iomsg)
  END DO

  aline = "END TYPE "//obj%name
  CALL md%WRITE(val=aline, iostat=iostat, iomsg=iomsg)
  CALL md%EndCodeFence()
END IF

! ## Subheading for methods
isok = ALLOCATED(obj%methods)
IF (isok) THEN
  aline = "Methods"
  CALL md%WriteH2(val=aline)

  CALL md%StartCodeFence(lang="fortran")
  CALL md%WRITE(val=obj%headerLine, iostat=iostat, iomsg=iomsg)

  tsize = SIZE(obj%methods)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%methods(ii)%ptr)
    IF (.NOT. isok) CYCLE

    field => obj%methods(ii)%ptr

    CALL md%WRITE(val=field%name, iostat=iostat, iomsg=iomsg)
    CALL md%WRITE(val=field%doc, iostat=iostat, iomsg=iomsg)
  END DO

  aline = "END TYPE "//obj%name
  CALL md%WRITE(val=aline, iostat=iostat, iomsg=iomsg)
  CALL md%EndCodeFence()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GenerateMarkdownDocs

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE MarkdownMethods
